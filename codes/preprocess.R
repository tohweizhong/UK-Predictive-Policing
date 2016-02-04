
# preprocess.R

# required columns
req_vars <- c("URN"
              ,"POSTCODE"
              ,"ICLOSE"
              ,"ISNEW"
              ,"MINORGROUP"
              ,"NFTYPE"
              ,"ISPRIMARY"
              ,"ISSECONDARY"
              ,"ISPOST16"
              ,"AGEL"
              ,"AGEH"
              ,"GENDER"
              ,"SFGENDER"
              ,"TOTPUPS"
              ,"TPUPYEAR"
              ,"TELIG"
              ,"TKS1APS"
              ,"TKS1EXP_L"
              ,"PTNOTFSM6CLA1A"
              ,"PTREADWRITTAMATAX"
              ,"TAPS"
              ,"OVAMEAS"
              ,"TEACHINGSTAFF"
              ,"PREMISES"
              ,"LEARNINGRESOURCES"
              ,"ICT"
              ,"BOUGHTINPROFESSIONALSERVICES"
              ,"TOTALEXPENDITURE")

# function to select the required columns from one dataframe in l_df
# impute with NA when necessary

GetCols <- function(df){
    
    cn <- colnames(df)
    print(length(cn))
    returnMe <- data.frame(as.matrix("0", nrow = 1))
    
    for(this_var in req_vars){
        
        if(this_var %in% cn){
            print("here")
            returnMe <- cbind(returnMe, subset(df, select = this_var))
        }
        else{
            print("there")
            returnMe <- cbind(returnMe, data.frame(NA))
            colnames(returnMe)[ncol(returnMe)] <- this_var
        }
    }
    return(returnMe)
}

# apply function across l_df
l_df_req_vars <- lapply(l_df, FUN = GetCols)
save(list = "l_df_req_vars", file = "data/l_df_req_vars.RData")
all_sch <- do.call(rbind.data.frame, l_df_req_vars)
# drop first column
all_sch <- all_sch[,-1]

# remove duplicates (not supposed to have duplicates but got bug in scrape-edu.R)
all_sch <- all_sch[!duplicated(all_sch),]

# write to file
write.csv(all_sch, file = "data/all_sch.csv")

