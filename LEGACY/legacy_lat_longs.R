#==============================================================================
# Author: Zachary M. Smith
# Date: ~ October, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to provide Lat and Long information for
#          Legacy STORET data.
# Output: Creates a single ".csv" file of station information for all of the Legacy
#         STORET data.
#==============================================================================
setwd("C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R")
#==============================================================================
DE <- read.table("DELAWARE.txt")
DC <- read.table("DISTRICT.txt")
MD <- read.table("MARYLAND.txt")
PA <- read.table("PENNSYLVANIA.txt")
VA <- read.table("VIRGINIA.txt")
WV <- read.table("WEST_VIRGINIA.txt")
#==============================================================================
test<- head(DE, n = 2000)
new.df <- test[,c("Station", "Agency", "HUC", "Latitude", "Longitude")]
Delaware <- unique(new.df[c("Station")])

shrink <- function(State) {
  new.df <- State[,c("Station", "Agency", "HUC", "Latitude", "Longitude")]
  final.df <- unique(new.df[c("Station", "Agency", "HUC", "Latitude", "Longitude")])
  return(final.df)
}


Delaware <- shrink(DE)
District <- shrink(DC)
Maryland <- shrink(MD)
Penn <- shrink(PA)
Virginia <- shrink(VA)
West <- shrink(WV)

final.output <- rbind(Delaware, District, Maryland, Penn, Virginia, West)
write.table(final.output, "WQ_Trends_Stations.txt")
new <- read.table("WQ_Trends_Stations.txt")
write.csv(new, "WQ_Trends_Stations3.csv")
