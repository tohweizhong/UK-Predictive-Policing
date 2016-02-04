
# preprocess.R

# required columns
req_vars <- c("URN" # Unique Reference Number
              ,"POSTCODE"
              ,"ICLOSE" # Is school closed?
              ,"ISNEW" # Is school new
              ,"MINORGROUP" # Type of school/college
              ,"NFTYPE" # School type
              ,"ISPRIMARY"
              ,"ISSECONDARY"
              ,"ISPOST16" # 16-18?
              ,"AGEL" # Lowest age of entry
              ,"AGEH" # Highest age of entry
              ,"GENDER" # Gender at entry
              ,"SFGENDER" # Gender of entry to sixth form
              ,"TOTPUPS" # Total number of pupils (including part-time pupils)
              ,"TPUPYEAR" # Number of pupils aged 11 
              ,"TELIG" # Published eligible pupil number
              ,"TKS1APS" # Cohort level key stage 1 average points score
              ,"TKS1EXP_L" # Number of pupils in cohort with low KS1 attainment
              ,"PTNOTFSM6CLA1A" # % key stage 2 pupils who are not disadvantaged
              ,"PTREADWRITTAMATAX" # % pupils achieving level 5 or above in reading and maths test and writing TA
              ,"TAPS" # Average point score
              ,"OVAMEAS" # Overall value added measure
              ,"TEACHINGSTAFF" # Teaching staff (£ per pupil)
              ,"PREMISES" # Premises (incl. Staff costs) (£ per pupil)
              ,"LEARNINGRESOURCES" # Learning resources (not ICT equipment) (£ per pupil)
              ,"ICT" # ICT learning resources (£ per pupil)
              ,"BOUGHTINPROFESSIONALSERVICES" # Bought in professional services - curriculum (£ per pupil)
              ,"TOTALEXPENDITURE") # Total expenditure (£ per pupil)

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
write.csv(all_sch, file = "data/all_sch.csv", row.names = FALSE)

