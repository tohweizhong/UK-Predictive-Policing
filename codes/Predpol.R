library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyr)
library(caret)
library(ggmap)
library(randomForest)
library(RColorBrewer)
library(ROCR)

set.seed(10000)

#Sys.setenv(http_proxy="https://proxy.ncs.com.sg:8080")

# Date scrape from Nestoria -----------------------------------------------

rawData = read.csv("rawData.csv", stringsAsFactors = FALSE)

#format data frame
rawData$Month = paste(rawData$Month, "01", sep = "-")
rawData$Date = as.Date(rawData$Month, format = "%Y-%m-%d") # convert to character
rawData$Month = as.character(month(rawData$Month, label = TRUE, abbr = TRUE))

columnNames = month.abb
rawData$latlong = paste(rawData$Latitude, rawData$Longitude, sep = ",")
complete_latlong = rawData %>% distinct(latlong)

#scrape housing prices for each unique latlongs
cnt <- 0
errorCount <- 0
output5 = apply(complete_latlong, MARGIN = 1, FUN = executeQuery)

## convert list to dataframe, save as csv
data = processList(output5)

#get latlongs with NA price data
missing = data[which(data %in% "NA")]
complete = data[-which(data %in% "NA")]
latlong = names(missing)
lat = unlist(strsplit(latlong, ","))[seq(1,length(unlist(strsplit(latlong, ","))),2)]
long = unlist(strsplit(latlong, ","))[seq(2,length(unlist(strsplit(latlong, ","))),2)]
lat = gsub(" ", "", lat)
long = gsub(" ", "", long)
missing = cbind(long,lat,latlong)

##scrape incremental/missing latlongs
cnt <- 0
errorCount <- 0
newOutput = apply(missing, MARGIN = 1, FUN = executeQuery)
newOutput1 = processList(newOutput)

#append new list of housing prices
data = c(complete, newOutput1)

##store scraped data
output = NULL
convertCount = 0
desc = data.frame(rownames(data[[1]]), stringsAsFactors = FALSE) #default row names

for(i in 1:length(data)){
  df = listToDF(data[i])
  output = rbind(df, output)
}

write.csv(output, file = "master housing price.csv", row.names = FALSE)

## refer to master housing price nas imputed,csv file for updated hosuing price data


# match property prices to crime data -------------------------------------

masterPriceRef = read.csv("master housing price nas imputed.csv", stringsAsFactors = FALSE)

cnt = 1
errorcount = 0
## default headers
priceType_headers = c("avg_1bed_property_rent_monthly", "avg_1bed_property_buy_monthly",
                      "avg_2bed_property_rent_monthly", "avg_2bed_property_buy_monthly",
                      "avg_3bed_property_rent_monthly", "avg_3bed_property_buy_monthly",
                      "avg_4bed_property_rent_monthly", "avg_4bed_property_buy_monthly",
                      "avg_5bed_property_rent_monthly", "avg_5bed_property_buy_monthly",
                      "avg_property_rent_monthly", "avg_property_buy_monthly") 

# ##Extract complete set of latlongs from data across all crime types
listOfDF = splitByMonth(rawData)
df1 <- lapply(listOfDF, FUN = fill_missing_POIs)
rawData = rbind(df1[[1]], df1[[2]], df1[[3]], df1[[4]], df1[[5]], df1[[6]], df1[[7]], df1[[8]], df1[[9]], df1[[10]], df1[[11]])

##initialize all values to 0
rawData[,11:22] = rep(rep(as.character(0), times = nrow(rawData)), times = 12)
colnames(rawData)[11:22] = priceType_headers

output = do.call(rbind,apply(rawData, MARGIN = 1, FUN = matchCode))

## crime data matched with housing price data
#write.csv(output, file = "Updated aggregated.csv", row.names = FALSE)


# 1. Data cleaning --------------------------------------------------------

##clean location information
output$Location = gsub("On or near ", "", output$Location)
output$Location = tolower(output$Location)

# Categorize location types
output$loc_type = sapply(output$Location, FUN = get_location_type)

# narrow scope to burglary crime type
burglary1 = output %>% filter(Crime.type == "Burglary" | is.na(Crime.type) == TRUE)

## aggregate crime volume by latlong and month
consolidated = burglary1 %>% group_by(latlong, Month) %>% summarize(crime_count = n())
burglary1 = merge(consolidated, burglary1, all.x = TRUE)
burglary1$Crime.ID = NULL

##remove duplicates
burglary1 = subset(burglary1, duplicated(burglary1) == FALSE)
burglary1$Month = match(burglary1$Month, month.abb) ## change month abbr to integers

## create hasCrime variable
burglary1$hasCrime = 0
burglary1$hasCrime[burglary1$crime_count > 0] = 1

# change NA values to 0
prop_prices = apply(burglary1[,c(11:22)], MARGIN = 2, FUN = function(x){x[is.na(x) == TRUE] = 0 ;return(x)})
burglary1[,c(11:22)] = prop_prices


# 2. Calculate expected rental yield --------------------------------------

exp_yield = t(apply(burglary1[,12:23], MARGIN = 1, FUN = get_exp_yield))
exp_yield = as.numeric(as.vector(exp_yield))

## clean INF values, set to N.A where denominator = -1
exp_yield[is.infinite(exp_yield)] = -1
exp_yield[is.nan(exp_yield)] = -1

##Note: 5-bedroom purchase cost outlier! #317913, #317923 removed from scaling function
#exp_yield[317913] = 0
#exp_yield[317923] = 0
#exp_yield = scale(exp_yield, center = min(exp_yield, na.rm = TRUE), scale = max(exp_yield, na.rm = TRUE) - min(exp_yield, na.rm = TRUE))
#exp_yield[317913] = 1
#exp_yield[317923] = 1
exp_yield = as.data.frame(matrix(exp_yield, ncol = 6, byrow = FALSE))
exp_yield = sapply(exp_yield, FUN = as.numeric)
colnames(exp_yield) = c("exp_yield_1bed","exp_yield_2bed","exp_yield_3bed", "exp_yield_4bed", "exp_yield_5bed", "exp_yield_avg")

burglary1 = cbind(burglary1,exp_yield)


# 3. standardize purchase price -------------------------------------------

buy_price = c(burglary1$avg_1bed_property_buy_monthly,
              burglary1$avg_2bed_property_buy_monthly,
              burglary1$avg_3bed_property_buy_monthly,
              burglary1$avg_4bed_property_buy_monthly,
              burglary1$avg_5bed_property_buy_monthly,
              burglary1$avg_property_buy_monthly)
#buy_price = scale(buy_price, center = min(buy_price, na.rm = TRUE), scale = max(buy_price, na.rm = TRUE) - min(buy_price, na.rm = TRUE))
buy_price = as.data.frame(matrix(buy_price, ncol = 6, byrow = FALSE))
colnames(buy_price) = c("avg_1bed_property_buy_monthly_std",
                        "avg_2bed_property_buy_monthly_std",
                        "avg_3bed_property_buy_monthly_std",
                        "avg_4bed_property_buy_monthly_std",
                        "avg_5bed_property_buy_monthly_std",
                        "avg_property_buy_monthly_std")

burglary1 = cbind(burglary1, buy_price)


# 4. calculate price disparity and standardize ----------------------------

diff_rental = burglary1$avg_5bed_property_rent_monthly - burglary1$avg_1bed_property_rent_monthly
#diff_rental = scale(diff_rental, center = min(diff_rental, na.rm = TRUE), scale = max(diff_rental, na.rm = TRUE) - min(diff_rental, na.rm = TRUE))
diff_buy = burglary1$avg_5bed_property_buy_monthly - burglary1$avg_1bed_property_buy_monthly
#diff_buy = scale(diff_buy, center = min(diff_buy, na.rm = TRUE), scale = max(diff_buy, na.rm = TRUE) - min(diff_buy, na.rm = TRUE))

## consolidate engineered features
burglary1 = cbind(burglary1, diff_rental, diff_buy)


# 5. create dummy variables for location type (loc_type) ------------------

loc_type_dummy_obj = dummyVars(~loc_type, data = burglary1)
loc_dummyVar = as.factor(predict(loc_type_dummy_obj, newdata = burglary1))
colnames(loc_dummyVar) = gsub(" ", "", colnames(loc_dummyVar))
burglary1 = cbind(burglary1,loc_dummyVar)

##clean variable names
namesc_= colnames(burglary1)
namesc_ = gsub("/", "_", namesc_)
colnames(burglary1) = namesc_


# 6. Add in 3 month lag time housing price data ---------------------------

## convert from month number to month (abb)
burglary1$Month = month(burglary1$Month, label = TRUE)

lag3_df = rep(rep(as.character(0), times = nrow(burglary1)), times = 12)
lag3_df = as.data.frame(matrix(lag3_df, ncol=12), stringsAsFactors = FALSE)

priceType_headers = gsub("avg", "3_mth_lag_avg", priceType_headers)

priceDF = cbind(burglary1$latlong, burglary1$Month, lag3_df, stringsAsFactors = FALSE)
colnames(priceDF) = c("latlong", "Month", priceType_headers)

cnt = 1

priceDF = do.call(rbind,apply(priceDF, MARGIN = 1, FUN = matchCode2, lag = 3))
#priceDF = read.csv("housing prices (3 month lag).csv", stringsAsFactors = FALSE)
priceDF$Month = match(priceDF$Month, month.abb)
colnames(priceDF) = c("latlong", "Month", priceType_headers)

temp = as.data.frame(apply(burglary1[,c(68:79)], MARGIN = 2, FUN = naToZero), stringsAsFactors = FALSE)
priceDF[,c(3:14)] = temp

## merge with master data set
burglary1 = merge(burglary1, priceDF, by.x = c("latlong", "Month"), by.y = c("latlong", "Month"), all.x = TRUE)


# 7. Add in feature to determine if neighbour crime was solved ------------

original = read.csv("aggregated_latest.csv", stringsAsFactors = FALSE)
original = original %>% filter(Crime.type == "Burglary")
original$Month = match(original$Month, month.abb)

original$latlong = paste(original$Latitude, original$Longitude, sep = ",")
original$Last.outcome.category = as.character(original$Last.outcome.category)

outcome_vec = as.matrix(original$Last.outcome.category)
## discretize outcome_vec 
## 1--> case closed, safe
## 0 --> case not closed, not safe
outcome_indicator = vector(mode = "integer",length = nrow(original))
outcome_indicator[grepl("Under investigation|Awaiting court outcome|Court result unavailable|Defendant sent to Crown Court|Local resolution|Offender fined|Offender given a caution|Offender given community sentence|Offender given conditional discharge|Offender given suspended prison sentence|Offender sent to prison|Suspect charged as part of another case|Offender ordered to pay compensation|Offender otherwise dealt with|Offender given penalty notice",
      outcome_vec)] = 1
original$indicator = outcome_indicator

original_summary = original %>% group_by(Latitude, Longitude, Month, indicator) %>% summarise(total = n())
burglary1 = left_join(burglary1, original_summary)
burglary1$indicator[is.na(burglary1$indicator)] = 1
burglary1$total[is.na(burglary1$total)] = 0
burglary1 = spread(burglary1, indicator, total, fill = 0)
colnames(burglary1)[78:79] = c("outcome_open", "outcome_closed")

## remove Jan data points
burglary2 = burglary1 %>% filter(Month != 1)

#write.csv(burglary2, file = "output with outcomes.csv", row.names = FALSE)


# 8. Add in neighbour crime matrix - this part needs to be run on server --------

## NOTE: ensure that neighbour matrix latlong is in the same order as the latlong in the burglary1 dataframe
## if not in order, load the following in the server to align order. Run the sortRows function
#
# load neighborhoodmatrix.RData                                         ## Matrix showing which latlong POIs are neighbours
# output = read.csv("complete latlong.csv", stringsAsFactors = FALSE)   ## Main dataframe used
# ref = read.csv("reference.csv", stringsAsFactors = FALSE)             ## Latlong reference used by the neighbour matrix
#
## populate monthly crime data with data points for no crime
## run the findNeighbour script on the server to get neighbour crime output for the previous month

load("//sscda/Shared Drive/_Delivery(TBR)/Team_A/Calvin/Nestoria/Final/neighborhoodmatrix.RData")
ref = read.csv("reference.csv", stringsAsFactors = FALSE)
ref = data.frame(ref[,-c(1,3:7)])
colnames(ref) = "reference"

ref_order = order(ref)
row.names(output) = NULL
monthList = splitByMonth(burglary2)
monthList[[1]] = NULL ##remove Jan

##sort each month's dataframe individually
sortedMonthList = lapply(monthList, FUN = sortRows)

##find neighbours
#define column names
neighbour_crime_col_names = colnames(sortedMonthList[[1]][27:42])
neighbour_crime_col_names = gsub("/", "_", neighbour_crime_col_names)
neighbour_crime_col_names_closed = gsub("loc", "NC_closed", neighbour_crime_col_names)
neighbour_crime_col_names_open = gsub("loc", "NC_open", neighbour_crime_col_names)
neighbour_crime_col_names = c(neighbour_crime_col_names_closed, neighbour_crime_col_names_open)

count = 1
sortedMonthList_nighbour = lapply(sortedMonthList, FUN = checkNeighbourCrime)

burglary2 = do.call(rbind, sortedMonthList_nighbour)

##takes in monthly crime DF
checkNeighbourCrime = function(df){
  x = do.call(cbind, list(apply(final, MARGIN = 2, FUN = matchMonthData, cc_closed = df$outcome_closed, cc_open = df$outcome_open, loc = df[,19:34])))
  x = t(as.data.frame(unlist(x, recursive = FALSE), stringsAsFactors = FALSE))
  colnames(x) = neighbour_crime_col_names
  #x = cbind(df,x)
  return(x)
}

# takes in vector indicating neighbour of each latlong
matchMonthData = function(neighbour, cc_closed, cc_open, loc){
  loc_closed = cc_closed*loc
  loc_open = cc_open*loc
  combined = cbind(neighbour, loc_closed, loc_open, stringsAsFactors = FALSE)
  colnames(combined) = c("neighbour indicator", neighbour_crime_col_names)
  print(paste("matching month data entry", count, sep = " "))
  count <<- count + 1
  output = combined %>% filter(neighbour == 1) %>% select(-neighbour) %>% summarise_each(funs(sum))
  output = data.frame(unlist(output), stringsAsFactors = FALSE)
  
  return(output)
}

neighbourDF = neighbourDF[,-c(2:6,8:44)]
temp = as.data.frame(apply(neighbourDF[,c(3:16)], MARGIN = 2, FUN = naToZero), stringsAsFactors = FALSE)
neighbourDF[,c(3:16)] = temp

naToZero = function(vec){
  vec[is.na(vec)] = 0
  return(vec)
}

## load neighbour matrix dataset
#load("...//sscda/Shared Drive/_Delivery(TBR)/Team_A/Calvin/Nestoria/1/list_neighbour_crime_by_location_and_outcome.RData")
#neighbourDF = consolidated_list1
burglary3 = left_join(burglary2, neighbourDF)


# split data set into training and validation set -------------------------

burglary3$hasCrime = as.factor(burglary3$hasCrime) ##full data set without Jan
burglary_filtered = burglary3[,-c(1,5:11,49)]

##split into training and test sets (for non-temporal analysis)
burglary_filtered = na.omit(burglary_filtered)   ## removes NA
burglary_train = burglary_filtered %>% filter(Month > 3,  Month < 11)
burglary_test = burglary_filtered %>% filter(Month == 11)

burglary_train$Month = NULL
burglary_test$Month = NULL


# MODEL 1 - Linear regression - Predict # of crime per latlong per --------

# adj-R^2 = 0.003
regModel1 = lm(crime_count~.-Month-latlong-loc_type, data = burglary_train)
regModel1_step = step(regModel1, direction = "backward", trace = 1)
regModel1_mod = lm(crime_count ~ exp_yield_2bed + neighborcrime + exp_yield_4bed + Longitude + 
                     loc_type.petrolstation + loc_type.others + Latitude + avg_property_buy_monthly_std + 
                     avg_1bed_property_buy_monthly_std, data = burglary_train)


# MODEL 2 - GLM - Predict # of crime per latlong per month ----------------

# AIC = 92810
regModel2 = glm(crime_count~.-Month-latlong-loc_type, family = "poisson", data = burglary_train)
regModel2_step = step(regModel2, direction = "backward", trace = 1)
regModel2_mod = glm(crime_count ~ loc_type.others + Latitude + avg_property_buy_monthly_std + 
                      avg_1bed_property_buy_monthly_std, family = "poisson", data = burglary_train)


# MODEL 3 - regression tree model - Predict # of crime per latlong --------

control = rpart.control(maxdepth = 10, cp =0.0005)

burglary_train = burglary_train[,-c(1,2,6:9,23)]
burglary_test = burglary_test[,-c(1,2,6:9,23)]
#burglary_train$crime_count = as.factor(burglary_train$crime_count)

regTreeModel = rpart(crime_count ~ ., method = "anova", data = burglary_train, control = control)
prp(regTreeModel, cex = 0.7)
regTreePred = predict(regTreeModel, newdata = burglary_test)
regTreePred = round(regTreePred, digits = 0)
regTreePred = as.data.frame(cbind(burglary_test$Latitude,burglary_test$Longitude, regTreePred))
colnames(regTreePred) = c("Latitude", "Longitude", "crime_count")

##estiate RMSE of prediction #RMSE = 11.58048
RMSE = sqrt((sum(nov$crime_count - regTreePred$crime_count)^2)/nrow(regTreePred))


# MODEL 4 - RF Model ------------------------------------------------------

rfModel = randomForest(hasCrime ~ .,
                       data = burglary_train,
                       mtry = 25,
                       ntree = 250,
                       importance = TRUE,
                       do.trace = TRUE,
                       strata = burglary_train$hasCrime,
                       sampsize = c(33300,33300)) ##downsample

novPred = predict(rfModel, newdata = burglary_test, type = "prob")
table(predOutput$prob >= 0.25, burglary_test$hasCrime)

varImpPlot(rfModel, n.var = 10)

## Plot ROC curve
predRF = prediction(novPred[,2], burglary_test$hasCrime)
predOutput = data.frame(burglary_test$Latitude, burglary_test$Longitude, predRF@predictions)
colnames(predOutput) = c("Latitude", "Longitude", "prob")

rocrPerf = performance(predRF, "tpr", "fpr")
plot(rocrPerf, colorize = TRUE)
auc = as.numeric(performance(predRF, "auc")@y.values)


# MODEL 5 - RF Model + tuning (Run on server only) ------------------------

## use caret package to optimize mtry parameter
control = trainControl(method = "cv", verboseIter = TRUE, returnData = FALSE,
                       returnResamp = "final", number = 2)
grid = expand.grid(.mtry = seq(25,40,5)) #optimal mtry was 25

trainObj = train(hasCrime ~ .,
                 data = burglary_train,
                 method = "rf",
                 metric = "Kappa",
                 trControl = control,
                 tuneGrid = grid)

#load("//sscda/Shared Drive/_Delivery(TBR)/Team_A/Calvin/Nestoria/Final/RF.RData") #ran on server

rfPred = predict.train(trainObj, newdata = burglary_test, type = "prob")
varImpPlot(rfModel, n = 10)

## Plot ROC curve
predRF = prediction(rfPred[,2], burglary_test$hasCrime)
predOutput = data.frame(burglary_test$Latitude, burglary_test$Longitude, predRF@predictions)
colnames(predOutput) = c("Latitude", "Longitude", "prob")

rocrPerf = performance(predRF, "tpr", "fpr")
plot(rocrPerf, colorize = TRUE, print.cutoffs.at = 0.3)
auc = as.numeric(performance(predRF, "auc")@y.values)
table(predOutput$prob >= 0.3, burglary_test$hasCrime)


# visualize burglary data -------------------------------------------------

london_map_obj = get_googlemap(center=c(lon = -0.15, lat = 51.52), zoom = 12, size = c(2048,2048), 
                               scale = 2, maptype = "roadmap", color = "bw")

## Actual
ggmap(london_map_obj) + geom_point(data = burglary_test[,c(1:3,69)][burglary_test$hasCrime == 1,], color = "blue", 
                                   aes(x = Longitude, y = Latitude, colour = burglary_test$hasCrime[burglary_test$hasCrime == 0]))

## regTreeModel prediction visualization
ggmap(london_map_obj) + geom_point(data = predOutput[predOutput$prob >= 0.3,], 
                                   aes(x = Longitude, y = Latitude, colour = predOutput$prob[predOutput$prob >= 0.3])) +
  scale_colour_gradientn(colours= c("blue", "green", "red"))

## combine actual + predicted
combined = cbind(burglary_test[,c(1,2,16)], predOutput$prob)
colnames(combined)[4] = "prob"

combined_filtered = subset(combined, hasCrime == 1 & prob >= 0.3)

ggmap(london_map_obj) + geom_point(data = combined_filtered, 
                                   aes(x = Longitude, y = Latitude, colour = combined_filtered$prob)) + 
  scale_colour_gradientn(colours= c("blue", "green", "red"))


# add in education data ---------------------------------------------------

load("//sscda/Shared Drive/_Delivery(TBR)/Team_A/Calvin/Nestoria/Final/edu_df.RData")
colnames(edu_df)[1] = "LSOA.code"
burglary4 = full_join(burglary3, edu_df)

# FUNCTIONS ---------------------------------------------------------------

### Extract housing price data for relevant latlong codes
## references data list for list of prices
#input: character vector (coerced into character vector by apply function), refers to masterPriceRef variable as price reference
#output: character vector
matchCode = function(vec){
  
  vec = data.frame(t(vec), stringsAsFactors = FALSE)
  month = vec$Month
  latlongRef = as.character(vec$latlong)
  
  # find correct list of housing price based on latlong
  price = masterPriceRef %>% 
    filter(latlong == latlongRef) %>%
    dplyr::select(matches(month))
  
  priceType = masterPriceRef %>% 
    filter(latlong == latlongRef) %>%
    dplyr::select(matches("desc"))
  
  rownames(price) = as.matrix(priceType)
  price = data.frame(t(price), stringsAsFactors = FALSE)
  
  ##merge prices based on price types
  vec_back = merge(x = vec[1,"avg_1bed_property_rent_monthly":"avg_property_buy_monthly"], y = price, all.y = TRUE)
  vec_front = vec[1,"latlong":"neighbourcrime"]
  
  vec = cbind(vec_front, vec_back)
  
  vec[1,"avg_1bed_property_rent_monthly":"avg_property_buy_monthly"] = as.character(vec[1,"avg_1bed_property_rent_monthly":"avg_property_buy_monthly"])
  print(paste("Completed matching entry",cnt,sep=" "))
  cnt <<- cnt + 1
  return(vec)
}

### Extract housing price data for relevant latlong codes
## references data list for list of prices
#input: character vector (coerced into character vector by apply function), refers to masterPriceRef variable as price reference
#output: character vector of housing prices with lag factor
matchCode2 = function(vec, lag){
  
  vec = data.frame(t(vec), stringsAsFactors = FALSE)
  month = match(as.character(vec$Month), month.abb) - lag

  if(month <= 0){ # no data available
    vec[1,(ncol(vec)-11):ncol(vec)] = 0
    print(paste("No data. Completed matching entry",cnt,sep=" "))
    cnt <<- cnt + 1
    return(vec)
    
  }else{
    latlongRef = as.character(vec$latlong)
    month = as.character(month(month, label = TRUE))
    # find correct list of housing price based on latlong
    price = masterPriceRef %>% 
      filter(latlong == latlongRef) %>%
      dplyr::select(matches(month))
    
    priceType = masterPriceRef %>% 
      filter(latlong == latlongRef) %>%
      dplyr::select(matches("desc"))
    
    rownames(price) = as.matrix(priceType)
    price = data.frame(t(price), stringsAsFactors = FALSE)
    
    ##merge prices based on price types
    vec_back = merge(x = vec[1,(ncol(vec)-11):ncol(vec)], y = price, all.y = TRUE)
    vec_front = vec[1,1:(ncol(vec)-11)]
    
    vec = cbind(vec_front, vec_back)
    
    vec[1,(ncol(vec)-11):ncol(vec)] = as.character(vec[1,(ncol(vec)-11):ncol(vec)])
    print(paste("Completed matching entry",cnt,sep=" "))
    cnt <<- cnt + 1
    return(vec)
  }
}

### format API queries
executeQuery = function(x){
  df = tryCatch(
    {
      long = x[4]
      lat = x[3]
      ID = x[1] #latlong string

      link = paste("http://api.nestoria.co.uk/api?action=metadata&country=uk&encoding=json&listing_type=buy&centre_point=",lat,",",long, sep ="")
      jsonOutput = fromJSON(link)
      names = jsonOutput[[2]]$metadata$metadata_name
      
      Jan = jsonOutput[[2]]$metadata$data[[1]][,-c(2,3)]
      Feb = jsonOutput[[2]]$metadata$data[[5]][,-c(2,3)]
      Mar = jsonOutput[[2]]$metadata$data[[6]][,-c(2,3)]
      Apr = jsonOutput[[2]]$metadata$data[[7]][,-c(2,3)]
      May = jsonOutput[[2]]$metadata$data[[8]][,-c(2,3)]
      Jun = jsonOutput[[2]]$metadata$data[[9]][,-c(2,3)]
      Jul = jsonOutput[[2]]$metadata$data[[10]][,-c(2,3)]
      Aug = jsonOutput[[2]]$metadata$data[[11]][,-c(2,3)]
      Sep = jsonOutput[[2]]$metadata$data[[12]][,-c(2,3)]
      Oct = jsonOutput[[2]]$metadata$data[[2]][,-c(2,3)]
      Nov = jsonOutput[[2]]$metadata$data[[3]][,-c(2,3)]
      Dec = jsonOutput[[2]]$metadata$data[[4]][,-c(2,3)]
      
      combined = data.frame(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec,
                            stringsAsFactors = FALSE)
      #print("combined dataframe")
      columnNames = colnames(combined)
      rownames(combined) = names
      combined = list(ID, combined) #store output as list
      #print("list of data + LSOA code")
      cnt <<- cnt + 1
      print(paste("Finished scrapping", cnt, "entries", sep = " "))
      #Sys.sleep(1)
      return(combined)
    },
    error = function(cond){
      error = list(ID, rep("NA", times = 1))
      errorCount <<- errorCount + 1
      print(paste("Error #", errorCount, sep = " "))
      geterrmessage()
      #Sys.sleep(1)
      return(error)
    }
  )
}

### processes output, returns named list of property prices based on LSOA codes
processList = function(list){
  temp = unlist(list, recursive = FALSE)
  codes = temp[seq(1,length(temp),2)]
  data = temp[seq(2,length(temp),2)]
  
  #name each list after corresponding LSOA code
  names(data) = unlist(codes)
  
  return(data)
}

##identify LSOA codes for re-scrapping
missingLSOA = function(data){
  remedy = unlist(sapply(data, FUN = function(x){ncol(x) == 0 | ncol(x) == 1}))
  LSOAcodesRemedy = names(data)[remedy]
  data <<- data[!remedy]
  LSOAcodesRemedy = subset(centroids, LSOA11CD %in% LSOAcodesRemedy)
  
  return(LSOAcodesRemedy)
}

##converts list to df for storage
#input: list of prices for each latlong
#output: df of values
listToDF = function(df){
  #check if data has "NA"
  if(length(df[[1]]) == 1){
    latlong = data.frame(rep(names(df), times = 12), stringsAsFactors = FALSE)
    df[[1]] = t(data.frame(rep("NA", times = 12)))
    colnames(df[[1]]) = columnNames
    out = cbind(latlong, desc, df[[1]]) #use default 12 rows
  }else{ #for all other price df with data
    latlong = data.frame(rep(names(df), times = nrow(df[[1]])), stringsAsFactors = FALSE)
    desc_default = rownames(df[[1]])
    out = cbind(latlong, desc_default, df[[1]])
  }
  colnames(out)[1:2] = c("latlong", " desc")
  rownames(out) = NULL
  convertCount <<- convertCount + 1
  print(paste("Completed converting", convertCount,"rows", sep = " "))
  return(out)
}

##Extract point of interest from Google geolocation API for data enrichment##
#Input: aggregated data (row wise)
#output: add enriched address component from Google Geocode, identify neighbouring POIs
matchPOI = function(x){
  ##construct API query
  #lat = 51.52734
  #long = 0.160896
  lat = x[4]
  long = x[3]
  location = x[5]
  key = "AIzaSyBrd-k3PmHhx8RwRWl9p4q6zsppciebiT0"
  result_type = "point_of_interest|establishment|street_address|political|premise|subpremise"
  
  link_googleAPI = paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",
                         lat,",",long,
                         "&key=",key,
                         "&result_type=",result_type,
                         sep="")
  
  jsonOutput1 = fromJSON(link_googleAPI)
  
  ##check if point of interest is residential
  if(grepl("avenue|road|grove|gardens|grove|close|crescent|lane|drive",location)){
    ##No other significant POIs in the vicinity
    if(jsonOutput1[[1]]$address_components[[2]]$types[[1]][1] == "locality" & jsonOutput1[[1]]$address_components[[2]]$types[[1]][2] == "political"){
      POI = "Residential"
      print("Residential by location name, confirmed by address component 2")
      ##Has significant POI in the vicinity
    }else if(grepl("point_of_interest",jsonOutput1[[1]]$address_components[[2]]$types[[1]][1])){
      POI = jsonOutput1[[1]]$address_components[[2]]$long_name[1]
      print("POI identified, confirmed by address component 2")
    }else if(...){
      POI = "Unknown"
      print("Unknown type")
    }
  }else{ ##check if coord corresponds to a type of premise. If yes, capture name of premise
    if(jsonOutput1[[1]]$address_components[[1]]$types[[1]][1] == "premise"){
      POI = jsonOutput1[[1]]$address_components[[1]]$long_name[1]
      print("Location name inconclusive, address component 1 = premise")
    }else{ ##catch all
      POI = jsonOutput1[[1]]$address_components[[1]]$long_name[1]
      POI_type = jsonOutput1[[1]]$address_components[[1]]$types[[1]][1]
      print(paste("Location name inconclusive, address component 1 =", POI, "of type", POI_type, sep = " "))
      POI = location
    }
  }
  ##postprocessing
  if(grepl("(Stop.*?)", POI)){
    POI = "Residential"
  }
  #street_name = paste(jsonOutput1[[1]]$address_components[[1]]$long_name[1], jsonOutput1[[1]]$address_components[[1]]$long_name[2], sep = " ")
  
  x = c(x, POI)
  
  return(x)
}

###Extract location type according to location description
##Input: location description vector
##output: vector of location categories
get_location_type = function(location){
  
  if(grepl("avenue|road|grove|gardens|grove|close|crescent|lane|drive|street|way|walk|terrace|court|place|hill|square|villas|circle", location)){
    return("residential")
  }else if(location == "petrol station"| location == "sports/recreation area"|
           location == "parking area"|location == "further/higher educational building"|
           location == "supermarket"|location == "shopping area"|
           location == "conference/exhibition centre"|location == "theatre/concert hall"|
           location == "airport/airfield"|location == "bus/coach station"|
           location == "park/open space"|location == "theme/adventure park"|
           location == "nightclub"){
    return(location)
  }else if(grepl("mews", location)){
    return("residential/commercial")
  }else
    return("others")
}

### calculate expected rental yield
#Input: housing prices only
#output: expected rental yield per house type
get_exp_yield = function(vec){
  #calculate expected yield from rental
  exp_yield_1bed = (vec[1]*12)/vec[2]
  exp_yield_2bed = (vec[3]*12)/vec[4]
  exp_yield_3bed = (vec[5]*12)/vec[6]
  exp_yield_4bed = (vec[7]*12)/vec[8]
  exp_yield_5bed = (vec[9]*12)/vec[10]
  exp_yield_avg = (vec[11]*12)/vec[12]
  
  vec = rbind(exp_yield_1bed,exp_yield_2bed,exp_yield_3bed, exp_yield_4bed, exp_yield_5bed, exp_yield_avg)
  
  vec = format(vec,digits = 5, scientific = FALSE)
  
  return(vec)
}

## Extracts POI list for POI with no crime for particular month, add on to list of POI w/ crime incidents
## Input: DF of crime incidents per month
## Output: DF w/ additional POIs corresponding to no crime incidents added to monthly data
## Note: to be executed on subset of data by crime type and month
fill_missing_POIs = function(df){
  month = df$Month[1]
  df = merge(df, complete_latlong, all.y = TRUE)
  df$Month = month
  df$crime_count[is.na(df$crime_count) == TRUE] = 0
  
  return(df)
}

## split data according to month
## Input: dataframe of rawdata
## Output: list of dataframes of crimes by month
splitByMonth = function(df){
  Jan = subset(df, df$Month == "Jan"|df$Month == 1)
  Feb = subset(df, df$Month == "Feb"|df$Month == 2)
  Mar = subset(df, df$Month == "Mar"|df$Month == 3)
  Apr = subset(df, df$Month == "Apr"|df$Month == 4)
  May = subset(df, df$Month == "May"|df$Month == 5)
  Jun = subset(df, df$Month == "Jun"|df$Month == 6)
  Jul = subset(df, df$Month == "Jul"|df$Month == 7)
  Aug = subset(df, df$Month == "Aug"|df$Month == 8)
  Sep = subset(df, df$Month == "Sep"|df$Month == 9)
  Oct = subset(df, df$Month == "Oct"|df$Month == 10)
  Nov = subset(df, df$Month == "Nov"|df$Month == 11)
  
  output = list(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov)
  
  return(output)
}

## sort main dataframe rows and align with neighbour matrix
sortRows = function(df){
  ### 1. align order of POIs to reference
  reordered = NULL
  for (count in 1:nrow(df)){
    rowRef = which(ref_order == count)
    reordered = rbind(reordered, df[rowRef,])
    print(paste("Completed row #", count, sep = " "))
  }
  
  ### check integrity of reordering
  #print(table(reordered$latlong == ref$reference))
  
  return(reordered)
}


# Others ------------------------------------------------------------------

#tokenization of location description
############################################################
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# loc <- annotate(burglary$Location, list(sent_token_annotator, word_token_annotator)) #unique annotation required for each entry
# 
# loc_annotator = Maxent_Entity_Annotator(kind = "location", probs = TRUE)
# y = annotate(burglary$Location[1], loc_annotator, loc)
# 
# test = as.data.frame(burglary[1,5])
# x = apply(test, MARGIN = 2, FUN = annotate, loc_annotator, loc)
############################################
####enhance location info using google API (not used)
###########################################
#rnames = colnames(burglary)
#test = t(apply(burglary[1:50,],MARGIN = 1, FUN = matchPOI))
#colnames(test) = c(rnames,"POI")

##create corpus
#################################
### process location data (not used)
################################
# preprocessCorpus <- function(ContentCorpus) {
#   # Function to preprocess corpus before processing for TfIdf or others
#   cc <- tm_map(ContentCorpus, tolower)
#   myStopwords <- c(stopwords("SMART"))
#   #cc <- tm_map(cc, removeWords, myStopwords)
#   #cc <- tm_map(cc, removeNumbers)
#   cc <- tm_map(cc, removePunctuation, TRUE)
#   cc <- tm_map(cc, stripWhitespace)
#   #cc <- tm_map(cc, stemDocument)
#   cc <- tm_map(cc, PlainTextDocument)
#   
#   return(cc)
# }
# 
# burglary$Location = tolower(burglary$Location)
# 
# corpus = Corpus(VectorSource(burglary$Location))
# corpus = preprocessCorpus(corpus)
# 
# #Tokenize key words
# XgramTokenizer1 = function(x) {
#   NGramTokenizer(x, Weka_control(min = 1, max = 2, delimiters = '\\W+'))
# }
# 
# dtm = DocumentTermMatrix(corpus, control = list(tokenize = XgramTokenizer1,
#                                                 weighting = function(x)
#                                                   weightTfIdf(x, normalize = FALSE),
#                                                 stopwords = TRUE))
# 
# dtm_noSparse = removeSparseTerms(dtm, 0.9953) #corresponds to lowFreq =~300
# dtm_noSparse = as.data.frame(as.matrix(dtm_noSparse))
# colnames(dtm_noSparse) = make.names(colnames(dtm_noSparse))
# dtm_noSparse = t(dtm_noSparse) # columns = location type, rows = crime incident record
# 
# findFreqTerms(dtm, lowfreq = 300)