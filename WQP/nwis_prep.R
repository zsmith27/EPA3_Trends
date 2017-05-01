#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/25/17
# Updated: 1/26/17
# Maintained: Zachary M. Smith
# Purpose: Identify the NWIS stations with the potential for long-term trend
# analysis.
# Output: Bind the NWIS stations with the selected legacy stations and export as
# a .csv.
#==============================================================================
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/NWIS/NWIS_Sites")
nwis.stations <- read.csv("potential_nwis_sites_1_19_17.csv", stringsAsFactors = FALSE)
#==============================================================================
# Load the parameter codes and reporting unit codes.
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Claire_Specifications/1_24_2017")
# This file contains the NWIS parameter codes selected by Buchanan.
keep.params <- read.csv("NWIS_Specifications_1_24_2017.csv", colClasses = "character")
nwis.stations <- nwis.stations[nwis.stations$parameter_cd %in% keep.params$parameter_cd, ]
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
stations.df <- read.table("LegacyStationsAll_050316Claire.txt", header = TRUE)
#==============================================================================
final.nwis<- data.frame(Agency = nwis.stations$OrganizationIdentifier)
final.nwis$Station <- nwis.stations$MonitoringLocationIdentifier
final.nwis$Station <- gsub("USGS-", "", final.nwis$Station)
final.nwis$State <- gsub("USGS-", "", final.nwis$Agency)
final.nwis <- final.nwis[, c("State", "Agency", "Station")]
final.df <- rbind(stations.df, final.nwis)
#==============================================================================
write.csv(final.df, "legacy_nwis_stations_1_26_2017.csv", row.names = FALSE)