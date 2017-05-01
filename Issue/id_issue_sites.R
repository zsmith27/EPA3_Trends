#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 2/1/17
# Updated: 2/3/17
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
#==============================================================================
# Load the dataRetrieval package created to pull USGS and EPA data into R.
library(dataRetrieval)
# dplyr and RPostgreSQL aid in the communication between R and Postgresql.
library(dplyr)
library(RPostgreSQL)
#==============================================================================
# Connect to the PostgreSQL database "WQT".
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Import table from the WQT database.
wqt <- dbReadTable(con, "Trend_Data")
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Issue")
#==============================================================================
# Use lapply to loop through all unique parameters and identify sites where
# reported values are >= the 95th or <= the 5th percentile.
param.list <- sort(unique(wqt$PARAMETER))
wqt.list <- lapply(param.list, function(i){
  print(paste0(i, " (", min(which(param.list == i)),
               "/", length(param.list), ")"))
  
  sub.df <- wqt[wqt$PARAMETER %in% i, ]
  thresh.95 <- quantile(sub.df$REPORTED_VALUE, 0.95, na.rm= TRUE)
  thresh.05 <- quantile(sub.df$REPORTED_VALUE, 0.05, na.rm= TRUE)
  sub.df$ISSUE_95 <- ifelse(sub.df$REPORTED_VALUE >= thresh.95, TRUE, FALSE)
  sub.df$ISSUE_05 <- ifelse(sub.df$REPORTED_VALUE <= thresh.05, TRUE, FALSE)
  sub.df$'95th' <- thresh.95
  sub.df$'05th' <- thresh.05
  if (i %in% c("HARDNESS", "HOT_ACIDITY", "NO2F", "TALK", "TEMP", "TURB_NTU")) {
    keep.sites <- unique(sub.df[sub.df$ISSUE_95 == TRUE | sub.df$ISSUE_05 == TRUE, "SITE"])
  } else {
    keep.sites <- unique(sub.df[sub.df$ISSUE_95 == TRUE, "SITE"])
  }
  
  final.df <- sub.df[sub.df$SITE %in% keep.sites, ]
  write.csv(final.df,
            paste0(i, "_issue_site.csv"),
            row.names = FALSE)
  
  
  param.sites <- lapply(keep.sites, function(x){
    site <- sub.df[sub.df$SITE %in% x, ]
    new.df <- data.frame(SITE = x)
    new.df$PARAMETER <- unique(sub.df$PARAMETER)
    new.df$MIN <- min(site$REPORTED_VALUE, na.rm = TRUE)
    new.df$MEDIAN <- median(site$REPORTED_VALUE, na.rm = TRUE)
    new.df$MAX <- max(site$REPORTED_VALUE, na.rm = TRUE)
    new.df$'95th' <- unique(site$'95th')
    new.df$'>=95' <- nrow(site[site$ISSUE_95 == TRUE, ])
    new.df$'<95' <- nrow(site[site$ISSUE_95 == FALSE, ])
    new.df$'%>=95' <- round((new.df$'>=95' / (new.df$'>=95' + new.df$'<95')) * 100, 0)
    if(new.df$PARAMETER %in% c("HARDNESS", "HOT_ACIDITY", "NO2F",
                               "TALK", "TEMP", "TURB_NTU")){
      new.df$'05th' <- unique(site$'05th')
      new.df$'<=05' <- nrow(site[site$ISSUE_05 == TRUE, ])
      new.df$'>05' <- nrow(site[site$ISSUE_05 == FALSE, ])
      new.df$'%<=05' <- round((new.df$'<=05' / (new.df$'<=05' + new.df$'>05')) * 100, 0)
    } else {
      new.df$'05th' <- NA
      new.df$'<=05' <- NA
      new.df$'>05' <- NA
      new.df$'%<=05' <- NA
    }
    return(new.df)
  })
  
  param.sites2 <- do.call(rbind, param.sites)
  
  write.csv(param.sites2,
            paste0(i, "_sites.csv"),
            row.names = FALSE)
  return(final.df)
})
#==============================================================================
# Create a dataframe of all parameters.
wqt.all <- do.call(rbind, wqt.list)
sites.all <- unique


