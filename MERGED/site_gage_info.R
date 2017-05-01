#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 2/1/17
# Updated: 2/1/17
# Maintained: Zachary M. Smith
# Purpose: Merge the Legacy STORET data with the WQP and gage information data.
# Output: A .csv file of all sites and their associate gages.  Nagel will 
#         use this file to find more gages and to eliminate duplicated sites.
#==============================================================================
#==============================================================================
# Laod the dataRetrieval package created to pull USGS and EPA data into R.
library(dataRetrieval)
# dplyr and RPostgreSQL aid in the communication between R and Postgresql.
library(dplyr)
library(RPostgreSQL)
#==============================================================================
# Import USGS Gage information.
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Output/1_26_2017")
gages <- read.csv("Station_Gage_List.csv", stringsAsFactors = FALSE)
names(gages) <- toupper(names(gages))
gages <- data.frame(lapply(gages, trimws), stringsAsFactors = FALSE)
#==============================================================================
# Import Legacy data.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Legacy/R_Output")
legacy <- read.csv("prep_merge_legacy_2017-02-01.csv", stringsAsFactors = FALSE)
#==============================================================================
# Import WQP data.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/WQP/R_Output")
wqp <- read.csv("prep_merge_wqp_2017-02-01.csv", stringsAsFactors = FALSE)
#==============================================================================
# Columns to keep.
keep.cols <- c("AGENCY", "SITE", "SITE_NAME", "LATITUDE", "LONGITUDE",
               "HORIZONTAL_DATUM")
legacy <- legacy[, keep.cols] %>% unique()
wqp <- wqp[, keep.cols] %>% unique()
#==============================================================================
agency.vec <- paste(unique(wqp$AGENCY), "-", sep = "") %>% paste0(collapse = "|")
wqp$SITE <- gsub(agency.vec, "", wqp$SITE)
wqp$AGENCY <- gsub("_WQX", "", wqp$AGENCY)
#==============================================================================
# Merge the two data sets and remove duplicate rows
merged <- rbind(legacy, wqp) %>% unique()
#==============================================================================
final.df <- merge(merged, gages, by = c("AGENCY", "SITE"), all = TRUE)
final.df[is.na(final.df)] <- ""
final.df <- final.df[order(final.df$AGENCY, final.df$SITE, final.df$SITE_NAME,
                           final.df$GAGE, final.df$GAGE_LOCATION), ]
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Merged/R_Output")
write.csv(final.df, 
          paste0("merged_sites_", Sys.Date(), ".csv"),
          row.names = FALSE)
#==============================================================================
#==============================================================================
gages.site <- gages[!gages$SITE %in% merged$SITE, ]




