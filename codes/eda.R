,
library(Standard)

pc_lsoa <- read.csv("UK Police data/POSTCODE_TO_LSOA.csv")
weather <- read.csv("UK Police data/weather_Heathrow_2015.csv")

#df1 <- read.csv("UK Police data/2015-01-metropolitan-street (updated).csv")
df2 <- read.csv("UK Police data/885927285d01f97ef1eab84bc0f03463612624d7/2015-01/2015-01-metropolitan-outcomes.csv")
df3 <- read.csv("UK Police data/885927285d01f97ef1eab84bc0f03463612624d7/2015-01/2015-01-metropolitan-street.csv")

foo <- merge(df1, df2, all = TRUE)


# ====

# Find all overlapping variables in the files

all_files <- list.files(path = "data/schools")

all_vars <- NULL

for(filename in all_files){
    print(filename)
    filename <- paste0("data/schools/", filename)
    tmp_df <- read.csv(filename, stringsAsFactors = FALSE)
    
    if(filename == "data/schools/urn100000.csv"){
        all_vars <- union(all_vars, unique(tmp_df$VARIABLE))  
    }
    else if(filename != "data/schools/urn100000.csv"){
        all_vars <- intersect(all_vars, unique(tmp_df$VARIABLE))
    }
    
}
common_vars <- all_vars



# find all vars
all_vars <- NULL

for(filename in all_files){
    print(filename)
    filename <- paste0("data/schools/", filename)
    tmp_df <- read.csv(filename, stringsAsFactors = FALSE)
    
    all_vars <- union(all_vars, unique(tmp_df$VARIABLE))  
    
}

save(list = c("all_vars", "common_vars"), file = "data/vars.RData")


# ====

# merge all files into one data frame
# may be sparse
# Only need variable and value

l <- list()

for(filename in all_files){
    print(filename)
    filename <- paste0("data/schools/", filename)
    tmp_df <- read.csv(filename, stringsAsFactors = F)
    
    tmp_df <- subset(tmp_df, select = c(VARIABLE, VALUE))
    
    l <- PushList(l, tmp_df)
}
save(list = "l", file = "data/l.RData")

#foo <- merge(t(l[[1]]), t(l[[2]]), all = TRUE)

l_df <- list()

for(i in seq_along(l)){
    
    print(i)
    tmp_df <- l[[i]]
    
    # change the structures of the data frames
    tmp_df <- data.frame(t(tmp_df), stringsAsFactors = F)
    varnames <- as.character(tmp_df[1,])
    tmp_df <- tmp_df[2,]
    colnames(tmp_df) <- varnames
    
    # remove duplicated columns
    dup <- duplicated(colnames(tmp_df))
    tmp_df <- tmp_df[, !dup]
    
    # include the additional columns, and fill with NAs if required
    AddCols <- function(df){
        
        for(ii in seq_along(all_vars)){
            
            one_col <- all_vars[ii]
            
            #print(one_col);
            #print(ii)
            
            if((one_col %in% colnames(df)) == FALSE){
                
                df <- cbind(df, NA)
                colnames(df)[ncol(df)] <- one_col
            }
            #print(dim(df))
            
        }
        return(df)   
    }
    
    # fill the columns
    tmp_df <- AddCols(tmp_df)
    
    l_df <- PushList(l_df, tmp_df)
    
}

all_sch <- do.call(rbind.data.frame, l_df)
save(list = "all_sch", file = "data/all_sch.RData")

