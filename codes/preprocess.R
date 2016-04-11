
# rf.R

library(magrittr)
library(dplyr)
library(Standard)

df <- read.csv("data/for yimin.csv", stringsAsFactors = F)
#View(df)
edu_vars <- c("numSch", "numClosedSch", "numNewSch"
              ,"pctMaintainedSch", "pctIndpSch", "pctSpecialSch", "pctAcademy"
              ,"mostCommonSchTyp", "secondMostCommonSchTyp"
              ,"numPriSch", "numSecSch", "numPost16"
              ,"meanAgeL", "meanAgeH"
              ,"mostCommonGender", "mostCommonSFGender"
              ,"totalNumPupils", "aveNumPupils", "avePublishedNumPupils"
              ,"aveTKS1APS", "aveTKS1EXP_L", "avePTNOTFSM6CLA1A", "avePTREADWRITTAMATAX"
              ,"aveTAPS", "aveOVAMEANS", "aveTEACHINGSTAFF", "sumTEACHINGSTAFF"
              ,"avePREMISES", "sumPREMISES", "aveLEARNINGRESOURCES", "sumLEARNINGRESOURCES"
              ,"aveICT", "sumICT", "avePS", "sumPS", "aveTOTALEXP", "sumTOTALEXP"
)

edu_df <- df[,edu_vars]
#str(edu_df)

# preprocessing for edu variables
# @ remove mostCommonSchTyp and secondMostCommonSchTyp (char)
# @ remove mostCommonGender and mostCommonSFGender (char)
# @ aveOVAMEANS (100% missingness)
# @ replace the NAs with zeroes

edu_df <- subset(edu_df, select = -c(mostCommonSchTyp, secondMostCommonSchTyp, mostCommonGender, mostCommonSFGender))
edu_df <- subset(edu_df, select = -aveOVAMEANS)

edu_df <- apply(edu_df, MARGIN = 2, FUN = function(v){
    idx <- which(is.na(v))
    v[idx] <- 0
    return(v)
    
}) %>% data.frame

df <- df[,-which(colnames(df) %in% edu_vars)]
df <- cbind(df, edu_df)

# check cols with NA
#ColsWithNA(df)
#MissingnessCols(df)

# look at response variable
table(df$crime_count)

# drop columns
df <- subset(df, select = -c(latlong, LSOA.code, LSOA.name, loc_type, Location, Crime.type, neighbourcrime, outcome))

# remove rows with missingness
df <- na.omit(df)

# split in CV sets
# @ Feb to Oct for training
# @ Nov for testing

train <- df %>% filter(Month > 1, Month < 11)
test <- df %>% filter(Month == 11)

save(list = "train", file = "data/train.RData")
save(list = "test", file = "data/test.RData")

# burglary_filtered = burglary3[,-c(1,5:11,49)]
# 
# ##split into training and test sets (for non-temporal analysis)
# burglary_filtered = na.omit(burglary_filtered)   ## removes NA
# burglary_train = burglary_filtered %>% filter(Month > 3,  Month < 11)
# burglary_test = burglary_filtered %>% filter(Month == 11)
# 
# burglary_train$Month = NULL
# burglary_test$Month = NULL
# 
# 
# 
# 
# regModel1 = lm(crime_count~.-Month-latlong-loc_type, data = burglary_train)
# regModel1_step = step(regModel1, direction = "backward", trace = 1)
# regModel1_mod = lm(crime_count ~ exp_yield_2bed + neighborcrime + exp_yield_4bed + Longitude + 
#                        loc_type.petrolstation + loc_type.others + Latitude + avg_property_buy_monthly_std + 
#                        avg_1bed_property_buy_monthly_std, data = burglary_train)