#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 2/8/17
# Updated: 2/8/17
# Maintained: Zachary M. Smith
# Purpose: Compare dissolved values to total values.
# Output: No output.
# Conclusion: No instances were dissolved values were measured at the
#             same Site and date as total values.
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
dis_vs_tot <- function(wqt.df, param){
  keep.cols <- c("SITE", "PARAMETER", "AGENCY.x", "REPORTED_VALUE", "UNITS", "DATE")
  dis.param <- paste(param, "DISS", sep = "_")
  tot.param <- paste(param, "TOT", sep = "_")
  dis <- wqt.df[wqt.df$PARAMETER %in% dis.param, keep.cols]
  tot <- wqt.df[wqt.df$PARAMETER %in% tot.param, keep.cols]
  merged.df <- merge(tot, dis, by = c("SITE", "PARAMETER", "AGENCY.x", "DATE"), all = TRUE)
  final.df <- merged.df[!is.na(merged.df$REPORTED_VALUE.x) & !is.na(merged.df$REPORTED_VALUE.y), ]
  return(final.df)
}
#==============================================================================
al.df <- dis_vs_tot(wqt, "AL")
ca.df <- dis_vs_tot(wqt, "CA")
mg.df <- dis_vs_tot(wqt, "MG")
pb.df <- dis_vs_tot(wqt, "PB")
fe.df <- dis_vs_tot(wqt, "FE")
cl.df <- dis_vs_tot(wqt, "CL")
mn.df <- dis_vs_tot(wqt, "MN")
s04.df <- dis_vs_tot(wqt, "SO4")
zn.df <- dis_vs_tot(wqt, "ZN")
al.df <- dis_vs_tot(wqt, "AL")
#==============================================================================
#==============================================================================

check_this <- function(wqt.df, param){
  keep.cols <- c("SITE", "PARAMETER", "AGENCY.x", "REPORTED_VALUE", "UNITS", "DATE")
  dis.param <- paste(param, "DISS", sep = "_")
  tot.param <- paste(param, "TOT", sep = "_")
  dis <- wqt.df[wqt.df$PARAMETER %in% dis.param, keep.cols]
  tot <- wqt.df[wqt.df$PARAMETER %in% tot.param, keep.cols]
  
  final.df <- cl.dis[cl.dis$SITE %in% cl.tot$SITE, ]
  unique(cl.tot$SITE)
  
  return(final.df)
}

#==============================================================================
check_this(wqt, "AL")
check_this(wqt, "CA")
check_this(wqt, "MG")
check_this(wqt, "PB")
check_this(wqt, "FE")
check_this(wqt, "CL")
check_this(wqt, "MN")
check_this(wqt, "SO4")
check_this(wqt, "ZN")
#==============================================================================