
library(rvest)
library(XML)

# read all postal codes
# formulate URLS
# go the URLs, extract all hyperlinks linking to schools
# extract all tabular data


met <- read.csv("C:/Users/weizhong/Desktop/UK Police data/2015-01-metropolitan-street (updated).csv", stringsAsFactors = F)
eg_url <- "http://education.gov.uk/cgi-bin/schools/performance/search.pl?searchType=postcode&postcode=CR0+3RL&distance=1&phase=all"

# remove one whitespace in PCD8
met$PCD8 <- gsub(pattern = "\\s+", replacement = "+", x = met$PCD8)
                   
for(i in seq_along(met$PCD8)){
    
    # check if it is a missing value
    if(met$PCD8 == "#N/A") next
    
    url <- gsub(x = eg_url, pattern = "CR0+3RL", replacement = met$PCD8[i], fixed = TRUE)
    pg <- html(url)
    
    # get school names
    school_names_nodes <- html_nodes(pg, ".schoolname")
    
    # get all results
    results_pane_node <- html_nodes(pg, "#results_pane")
    df <- readHTMLTable(results_pane_node[[1]][5][[1]])
    
    
    
    
}

# ====

edubase <- read.csv("C:/users/weizhong/Desktop/edubasealldata20160122.csv", stringsAsFactors = F)
eg_url <- "http://education.gov.uk/cgi-bin/schools/performance/school.pl?urn=101847"

for(i in seq_along(edubase)){
    
    if(nchar(edubase$CloseDate[i]) != 0) next
    
    url <- gsub(x = eg_url, pattern = "101847", replacement = edubase$URN[i], fixed = TRUE)
    pg <- html(url)
    
    # get the tables
    table_nodes <- html_nodes(pg, "table")
    school_details_df_1 <- readHTMLTable(table_nodes[[1]])
    school_details_df_2 <- readHTMLTable(table_nodes[[3]])
    
}


head(edubase$LSOA..name.)
