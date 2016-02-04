
# eda2.R

load("data/l.RData")

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
    
    l_df <- PushList(l_df, tmp_df)
}

save(list = "l_df", file = "data/l_df.RData")


# how many primary schools
pri <- unlist(lapply(l_df, FUN = function(x){
    return(x$ISPRIMARY == 1)
}))

# how many secondary schools
sec <- unlist(lapply(l_df, FUN = function(x){
    return(x$ISSECONDARY == 1)
}))

