
# preprocess.R

library(magrittr)

load("data/l_df.RData")

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
save(list = "all_sch", file = "data/all_sch.RData")

# ====
# ====
# ====
# (180216)

all_sch <- read.csv("data/all_sch.csv")

# add LSOA column
PC_to_LSOA <- read.csv("UK Police data/POSTCODE_TO_LSOA.csv", stringsAsFactors = F)

# remove a single space in PCD8 
PC_to_LSOA$PCD8 <- sapply(PC_to_LSOA$PCD8, FUN = CleanSpaces)

# function to match postal code to LSOA
P2L <- function(p){
    idx <- which(PC_to_LSOA$PCD8 == p)
    return(PC_to_LSOA$LSOA11CD[idx])
}
LSOA <- sapply(all_sch$POSTCODE, FUN = P2L)
#LSOA <- unlist(LSOA)

# replace chr(0) with NA
LSOA <- lapply(LSOA, FUN = function(x){
    
    if(length(x) == 0)
        return("Not found")
    else return(x)
}) %>% unlist

# cbind
all_sch2 <- cbind(all_sch, LSOA)
save(all_sch2, file = "data/all_sch2.RData")

# find unique LSOAs, then take averages of the school metrics
uq_LSOA <- (all_sch2$LSOA %>% unique)
# what features to engineer per LSOA?
# @ number of schools
# @ number of closed schools
# @ number of new schools
# @ %maintained schools
# @ %independent schools
# @ %special schools
# @ %academy colleges
# @ most common school type (NFTYPE)
# @ second most commmon school type (NFTYPE)
# @ number of primary schools
# @ number of secondary schools
# @ number of schools is post 16-18 (ISPOST16)
# @ average lowest age of entry (AGEL)
# @ average highest age of entry (AGEH)
# @ most common GENDER
# @ most common SFGENDER
# @ Total number of pupils (sum)
# @ Ave number of pupils per school
# @ Average published pupil number
# @ average TKS1APS <----
# @ average total number of pupils with low KS1 attainment
# @ average PTNOTFSM6CLA1A
# @ average PTREADWRITTAMATAX
# @ average TAPS
# @ average OVAMEANS
# @ average TEACHINGSTAFF
# @ total TEACHINGSTAFF
# @ average PREMISES
# @ total PREMISES
# @ average LEARNINGRESOURCES
# @ total LEARNINGRESOURCES
# @ average ICT
# @ total ICT
# @ average BOUGHTINPROFESSIONALSERVICES
# @ total BOUGHTINPROFESSIONALSERVICES
# @ average TOTALEXPENDITURE
# @ total TOTALEXPENDITURE

LSOA_df <- lapply(uq_LSOA %>% as.character, FUN = function(x){
    
    print(x)
    
    idx <- which(all_sch2$LSOA == x)
    sub_all_sch2 <- all_sch2[idx,]
    
    numSch <- length(idx)
    numClosedSch <- length(which(sub_all_sch2$ICLOSE == "Closure"))
    numNewSch <- length(which(sub_all_sch2$ISNEW == 1))
    
    pctMaintainedSch <- length(which(sub_all_sch2$MINORGROUP == "Maintained School")) / numSch
    pctIndpSch <- length(which(sub_all_sch2$MINORGROUP == "Independent School")) / numSch
    pctSpecialSch <- length(which(sub_all_sch2$MINORGROUP == "Special School")) / numSch
    pctAcademy <- length(which(sub_all_sch2$MINORGROUP == "Academy (including Free Schools) College")) / numSch
    
    tmp <- sort(sub_all_sch2$NFTYPE %>% as.character %>% table, decreasing = TRUE)
    mostCommonSchTyp <- names(tmp)[1]
    secondMostCommonSchTyp <- names(tmp)[2]
    
    numPriSch <- length(which(sub_all_sch2$ISPRIMARY == 1))
    numSecSch <- length(which(sub_all_sch2$ISSECONDARY == 1))
    numPost16 <- length(which(sub_all_sch2$ISPOST16 == 1))
    
    meanAgeL <- sub_all_sch2$AGEL %>% mean
    meanAgeH <- sub_all_sch2$AGEH %>% mean

    tmp <- sort(sub_all_sch2$GENDER %>% as.character %>% table, decreasing = TRUE)
    mostCommonGender <- names(tmp)[1]
    
    tmp <- sort(sub_all_sch2$SFGENDER %>% as.character %>% table, decreasing = TRUE)
    mostCommonSFGender <- names(tmp)[1]
    
    totalNumPupils <- sum(as.numeric(sub_all_sch2$TOTPUPS), na.rm = TRUE)
    aveNumPupils <- mean(sub_all_sch2$TOTPUPS %>% as.numeric, na.rm = TRUE)
    avePublishedNumPupils <- mean(sub_all_sch2$TELIG %>% as.numeric, na.rm = TRUE)
    
    aveTKS1APS           <- mean(sub_all_sch2$TKS1APS %>% as.numeric, na.rm = TRUE)
    aveTKS1EXP_L         <- mean(sub_all_sch2$TKS1EXP_L %>% as.numeric, na.rm = TRUE)
    avePTNOTFSM6CLA1A    <- mean(sub_all_sch2$PTNOTFSM6CLA1A %>% as.numeric, na.rm = TRUE)
    avePTREADWRITTAMATAX <- mean(sub_all_sch2$PTREADWRITTAMATAX %>% as.numeric, na.rm = TRUE)
    aveTAPS              <- mean(sub_all_sch2$TAPS %>% as.numeric, na.rm = TRUE)
    aveOVAMEANS          <- mean(sub_all_sch2$OVAMEANS %>% as.numeric, na.rm = TRUE)
    aveTEACHINGSTAFF     <- mean(sub_all_sch2$TEACHINGSTAFF %>% as.numeric, na.rm = TRUE)
    sumTEACHINGSTAFF     <- sum(sub_all_sch2$TEACHINGSTAFF %>% as.numeric, na.rm = TRUE)
    avePREMISES          <- mean(sub_all_sch2$PREMISES %>% as.numeric, na.rm = TRUE)
    sumPREMISES          <- sum(sub_all_sch2$PREMISES %>% as.numeric, na.rm = TRUE)
    aveLEARNINGRESOURCES <- mean(sub_all_sch2$LEARNINGRESOURCES %>% as.numeric, na.rm = TRUE)
    sumLEARNINGRESOURCES <- sum(sub_all_sch2$LEARNINGRESOURCES %>% as.numeric, na.rm = TRUE)
    aveICT               <- mean(sub_all_sch2$ICT %>% as.numeric, na.rm = TRUE)
    sumICT               <- sum(sub_all_sch2$ICT %>% as.numeric, na.rm = TRUE)
    avePS                <- mean(sub_all_sch2$BOUGHTINPROFESSIONALSERVICES %>% as.numeric, na.rm = TRUE)
    sumPS                <- sum(sub_all_sch2$BOUGHTINPROFESSIONALSERVICES %>% as.numeric, na.rm = TRUE)
    aveTOTALEXP          <- mean(sub_all_sch2$TOTALEXPENDITURE %>% as.numeric, na.rm = TRUE)
    sumTOTALEXP          <- sum(sub_all_sch2$TOTALEXPENDITURE %>% as.numeric, na.rm = TRUE)
    
    return(cbind(x, numSch, numClosedSch, numNewSch
                 ,pctMaintainedSch, pctIndpSch, pctSpecialSch, pctAcademy
                 ,mostCommonSchTyp, secondMostCommonSchTyp
                 ,numPriSch, numSecSch, numPost16
                 ,meanAgeL, meanAgeH
                 ,mostCommonGender, mostCommonSFGender
                 ,totalNumPupils, aveNumPupils, avePublishedNumPupils
                 ,aveTKS1APS, aveTKS1EXP_L, avePTNOTFSM6CLA1A, avePTREADWRITTAMATAX
                 ,aveTAPS, aveOVAMEANS, aveTEACHINGSTAFF, sumTEACHINGSTAFF
                 ,avePREMISES, sumPREMISES, aveLEARNINGRESOURCES, sumLEARNINGRESOURCES
                 ,aveICT, sumICT, avePS, sumPS, aveTOTALEXP, sumTOTALEXP
    ))
})

LSOA_df <- LSOA_df %>% do.call(args = ., what = rbind.data.frame)
edu_df <- LSOA_df[-1,] # Not found
colnames(edu_df)[1] <- "LSOA"

# change variable type
char_vars <- "LSOA"
factor_vars <- c("mostCommonSchTyp", "secondMostCommonSchTyp",
                 "mostCommonGender", "mostCommonSFGender")
num_vars <- colnames(edu_df) %>% setdiff(., char_vars) %>% setdiff(., factor_vars)

for(var in num_vars){
    edu_df[,var] <- edu_df[,var] %>% as.character %>% as.numeric
}
edu_df[,char_vars] <- edu_df[,char_vars] %>% as.character
str(edu_df)


save(edu_df, file = "data/edu_df.RData")

# ====

agg_latest <- read.csv("data/aggregated_latest.csv", stringsAsFactors = FALSE)


