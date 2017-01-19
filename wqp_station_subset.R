#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/10/17
# Updated: 1/11/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to subset the WQP data to only include
#          data from 1965 to present and long-term monitoring stations 
#          identified in the Legacy STORET.
# Output: Creates individual ".txt" files for each EPA Region 3 State.
#==============================================================================
#==============================================================================
# Load stations and parameters specified by Buchanan.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
stations.df <- read.table("LegacyStationsAll_050316Claire.txt", header = TRUE)
#==============================================================================
# Set working directory.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
#==============================================================================
# Import the data for each state. 
# The data was previously subset to only include potential params of interest.
dc.df <- read.csv("sub_wqp_dc.csv", stringsAsFactors = FALSE)
de.df <- read.csv("sub_wqp_de.csv", stringsAsFactors = FALSE)
md.df <- read.csv("sub_wqp_md.csv", stringsAsFactors = FALSE)
pa.df <- read.csv("sub_wqp_pa.csv", stringsAsFactors = FALSE)
va.df <- read.csv("sub_wqp_va.csv", stringsAsFactors = FALSE)
wv.df <- read.csv("sub_wqp_wv.csv", stringsAsFactors = FALSE)
#==============================================================================
# This function subsets the WQP data to only include data collected after 1965
# and stations selected in the Legacy STORET data.
keep_stations <- function(org.df, sta.df){
  org.df$ActivityStartDate <- as.Date(org.df$ActivityStartDate)
  test <- org.df[org.df$ActivityStartDate >= "1965-01-01", ]
  final.df <- org.df[grepl(paste(sta.df$Station, collapse = "|"),
                         org.df$MonitoringLocationIdentifier), ]
  return(final.df)
}
#==============================================================================
# Apply the "keep_stations" function.
dc <- keep_stations(dc.df, stations.df)
de <- keep_stations(de.df, stations.df)
md <- keep_stations(md.df, stations.df)
pa <- keep_stations(pa.df, stations.df)
va <- keep_stations(va.df, stations.df)
wv <- keep_stations(wv.df, stations.df)
#==============================================================================
# Join all of the station information into a single data frame.
bound <- rbind(dc, de, md, pa, va, wv)
#==============================================================================
# Load the WQP station information
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data/Site_Data")
wqp.station <- read.csv("sub_site_all.csv")
#==============================================================================
# Join the site info with the param data.
by.names <- c("OrganizationIdentifier", "OrganizationFormalName",
              "MonitoringLocationIdentifier") #, "MonitoringLocationName")
final.df <- dplyr::left_join(bound, wqp.station, by = by.names )
#==============================================================================
# Export all data as a single table.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
write.csv(final.df, "wqp_keep_stations.csv", row.names = FALSE)




