#==============================================================================
# Author: Zachary M. Smith
# Created: 1/11/17
# Updated: 1/12/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to 
# Output: Creates individual ".txt" files for each EPA Region 3 State.
#==============================================================================
#==============================================================================
# Load WQP data.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
wqp.stations <- read.csv("wqp_keep_stations.csv")
#==============================================================================
# Load stations and parameters specified by Buchanan.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
legacy <- read.table("LegacyStationsAll_050316Claire.txt", header = TRUE)
#==============================================================================
station.list <- list()
for(i in seq_along(legacy$Station)) {
  new.df <- data.frame(STATION = legacy$Station[i])
  new.df$BOOLEAN <- any(grepl(legacy$Station[i], wqp.stations$MonitoringLocationIdentifier) == TRUE)
  station.list[[i]] <- new.df
  
}
#==============================================================================
boolean.df <- do.call(rbind, station.list)
#==============================================================================
# Import the Legacy stations by year file and append the boolean WQP match.
# Claire will use this to make future decisions.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
legacy.stations <- read.csv("COPY_Claire_Station_by_Year_01_12_17.csv")
legacy.stations$Station <- trimws(legacy.stations$Station)
merged <- merge(boolean.df, legacy.stations, by.x = "STATION", by.y = "Station", all = TRUE)
merged[is.na(merged)] <- ""
names(merged)[names(merged)  %in% "BOOLEAN"] <- "WQP_Match"
write.csv(merged, "UPDATE_Claire_Station_by_Year_01_12_17.csv", row.names = FALSE)
table(test.2$BOOLEAN.x)
#==============================================================================
# Subset to keep only the Legacy STORET sites without a WQP match.
check.df <- boolean.df[boolean.df$BOOLEAN == FALSE, ]
#==============================================================================
# Import the Legacy STORET data.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Subset Stations and Params")
all.legacy <- read.table("all_sub.txt", header = T)
#==============================================================================
# Subset to only keep the stations without a match in the WQP dataset.
legacy.df <- all.legacy[all.legacy$STATION %in% check.df$STATION, ]
sub.leg <- unique(legacy.df[, 1:8])
#==============================================================================
#setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Match_to_Legacy")
#write.csv(sub.leg, "legacy_no_match.csv", row.names = FALSE)
#==============================================================================
# Keep only the necessary columns to compare the WQP site locations to the
# Legacy STORET site loactions.
keep.cols <- c("OrganizationIdentifier", "MonitoringLocationIdentifier",
               "LatitudeMeasure", "LongitudeMeasure")
sub.wqp <- unique(wqp.stations[, keep.cols])
#==============================================================================
# This function allows the Lat/Longs of the Legacy STORET sites to be compared
# to the Lat/Longs of the WQP sites. The goal was to identify common sites
# between the two datasets that may have undergone a name change.
# The r_num input allows the user to quickly round the Lat/Longs of both 
# datasets.  Lat/Longs may have become more specific in the WQP dataset, so
# exact matches may not be possible. Therefore, we attempted to round up the
# Lat/Longs to make them compareable.
test_round <- function(leg, wqp, r_num) {
  leg$LAT <- round(leg$LATITUDE, r_num)
  leg$LONG <- round(leg$LONGITUDE, r_num)
  wqp$LAT <- round(wqp$LatitudeMeasure, r_num)
  wqp$LONG <- round(wqp$LongitudeMeasure, r_num)
  
  final.df <- merge(leg, wqp, by = c("LAT", "LONG"))
  return(final.df)
}
#==============================================================================
# Apply the test_round function.
test <- test_round(sub.leg, sub.wqp, 4)






