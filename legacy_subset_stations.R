#==============================================================================
# Author: Zachary M. Smith
# Date: ~ October, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to subset only the Legacy STORET stations 
#          of interest.
# Output: Creates a subset of potential trend stations.
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Legacy STORET/Aggregate Raw Data/Legacy Station")
#==============================================================================
DE <- read.table("DE_Station.txt")
DC <- read.table("DC_Station.txt")
MD <- read.table("MD_Station.txt")
PA <- read.table("PA_Station.txt")
VA <- read.table("VA_Station.txt")
WV <- read.table("WV_Station.txt")
#==============================================================================

new.df <- rbind(DE, DC, MD, PA, VA, WV)

new.list <- data.frame(unique(new.df$Station.Type))



station.type <- paste(c("STREAM", "SPRING"), collapse = "|")

df.1 <- new.df[grepl(station.type, new.df$Station.Type), ]

reach <- paste(c("BAY", "LAKE" , "OCEAN"), collapse = "|")

df.2 <- df.1[!grepl(reach, df.1$Rchname), ]

exclude.type <- paste(c("ESTURY", "LAKE", "OCEAN"), collapse = "|")
new_stations <- df.2[!grepl(exclude.type, df.2$Station.Type), ]
#lotic.df <- data.frame(new_stations[, "Station"])
#colnames(lotic.df) <- "Station"
#final.station.list <- WQ[WQ$Station %in% LS$Station,]





wq_gis <- read.csv("WQ_Trends_Stations_Info.txt")
bad_coord <- wq_gis[which(wq_gis$Notes == "No coordinates" | 
                            wq_gis$Notes == "Outside EPA R3" |
                            wq_gis$NHDFlowl_1 == "COASTLINE" |
                            wq_gis$NHDFlowl_1 == "PIPELINE" |
                            wq_gis$NHDFlowl_1 == "CONNECTOR" |
                            wq_gis$FEATURE == "LAKE/POND"), ]
bad_list <- data.frame(unique(bad_coord[, c("Agency", "Station")]))
colnames(bad_list) <- c("Agency", "Station")

#station_list <- data.frame(unique(lotic_stations$Station))
#colnames(station_list) <- "Station"

station_list <- data.frame(new_stations[!new_stations[,c("Agency", "Station")] %in% 
                                          bad_list[,c("Agency", "Station")], ])
station_list2 <- data.frame(station_list[, c("Agency", "Station")])


write.csv(station_list2, "New_Station_List_2.txt")
