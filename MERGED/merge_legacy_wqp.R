#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/26/17
# Updated: 3/08/17
# Maintained: Zachary M. Smith
# Purpose: Merge the Legacy STORET data with the WQP data.
# Output: The join.df data is exported to the WQT PostgreSQL database.
#==============================================================================
#==============================================================================
# Load the necessary packages.
# Load the dataRetrieval package created to pull USGS and EPA data into R.
library(dataRetrieval)
# dplyr and RPostgreSQL aid in the communication between R and Postgresql.
library(dplyr)
library(RPostgreSQL)
# Load the RODBC package to connect to an MS Access database.
library(RODBC)
#==============================================================================
setwd("H:/Projects/EPA3Trends/Data/Data_May2017")
flow.df <- data.table::fread("Data_Flow_nocomposits_May2017.txt")
#==============================================================================
# Search for all odbc connections on the computer.
odbcDataSources()
# Create a connection with the EPA REgaion 3 Water Quality Trends database.
channel <- odbcConnect("EPA3_WQT")
#==============================================================================
# Import the necessary MS Access tabs.
tab_sites <- sqlFetch(channel, "Sites")
tab_sites_additional <- sqlFetch(channel, "Sites_AdditionalInfo")
tab_gages <- sqlFetch(channel, "Site_Gage_List")
tab_gage_flow <- sqlFetch(channel, "LTQ_Data")
tab_gage_info <- sqlFetch(channel, "LTQ_GageInfo")
# Close the odbc connection.
odbcCloseAll()
#==============================================================================
names(tab_sites) <- toupper(names(tab_sites))
tab_sites <- data.frame(lapply(tab_sites, trimws))

names(tab_sites_additional) <- toupper(names(tab_sites_additional))
tab_sites_additional <- data.frame(lapply(tab_sites_additional, trimws))

sites <- merge(tab_sites, tab_sites_additional[, 1:4], by = c("ICP_ID", "DUP_ID"), all = TRUE)

names(tab_gages) <- toupper(names(tab_gages))
tab_gages <- data.frame(lapply(tab_gages, trimws))


names(tab_gage_info) <- c("Gage_ID", "Gage_NAME", "Gage_State", "Gage_Site_TYPE",
                          "Gage_Lat", "Gage_Long", "Gage_Datum", "Gage_State_CD",
                          "Gage_County_CD", "HUC_10", "Gage_Drainage_Area_SQMI",
                          "Gage_MinDate", "Gage_MaxDate")
names(tab_gage_info) <- toupper(names(tab_gage_info))
tab_gage_info <- data.frame(lapply(tab_gage_info, trimws))
gages <- merge(tab_gages, tab_gage_info, by = c("GAGE_ID"), all = TRUE)
#==============================================================================
# Import Legacy and WQP data.
#setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Output/1_26_2017")
setwd("H:/Projects/EPA3Trends/Data/Data_May2017/merged/output")
#gages <- read.csv("Station_Gage_List.csv", stringsAsFactors = FALSE)
#names(tab_gages) <- toupper(names(tab_gages))
#gages <- data.frame(lapply(tab_gages, trimws))
#legacy <- read.csv("prep_merge_legacy_1_30_17.csv", stringsAsFactors = FALSE)
legacy <- data.table::fread("prep_merge_legacy_2017-05-22.csv",
                            colClasses = c(ICPRB_CONVERSION = "character"))
#wqp <- read.csv("prep_merge_wqp.csv", stringsAsFactors = FALSE)
wqp <- data.table::fread("prep_merge_wqp_2017-05-22.csv",
                         colClasses = c(ICPRB_CONVERSION = "character"))
#==============================================================================
agency.vec <- paste(unique(wqp$AGENCY), "-", sep = "") %>% paste0(collapse = "|")
wqp$SITE <- gsub(agency.vec, "", wqp$SITE)
wqp$AGENCY <- gsub("_WQX", "", wqp$AGENCY)
#==============================================================================
# Join the two data sets and remove duplicate rows
join.df <- rbind(legacy, wqp) %>% unique() 
join.df <- join.df[order(join.df$AGENCY, join.df$SITE,
                       join.df$ICPRB_NAME, #join.df$PARAMETER,
                       join.df$DATE,  
                       join.df$Replicate.Number, join.df$DEPTH), ]
#==============================================================================
join.df$DATE <- as.Date(join.df$DATE)
join.df$MONTH <- format(join.df$DATE, "%m")
join.df$YEAR <- format(join.df$DATE, "%Y")
join.df <- join.df[join.df$YEAR >= 1972, ]
#==============================================================================
# Parameters of intrest with a low number of reporting values (< 100) where
# present in the data set because they were associated with stations with 
# at least one other paramter with a high number of reporting values(>= 100).
# Previous specifiations were to pull a specific list of stations and a specific
# list of paramters, but parameters were not strictly associated with a 
# particular station. The script below counts the data by Site and Parameter,
# and removes rows associated with Site/paramters with < 100 counts.

agg_my_data <- function(join.df, my.id, gages){
  agg.count <- aggregate(AGENCY ~ my.id +
                           #SITE +
                           #ICP_ID +
                           ICPRB_NAME + #PARAMETER + 
                           YEAR, data = join.df, length)
  agg.count <- agg.count[agg.count$AGENCY >= 10, ]
  agg.count2 <- aggregate(AGENCY ~ my.id +
                            #SITE +
                            #ICP_ID +
                            ICPRB_NAME, #PARAMETER, 
                          data = agg.count, length)
  agg.count2 <- agg.count2[agg.count2$AGENCY >= 10, ]
  
  #join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "PARAMETER"), all.y = TRUE)
  join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "ICPRB_NAME"), all.y = TRUE)
  table(join.df.2[join.df.2$SITE %in% "USGS-01578310", "ICPRB_NAME"])
  # Re-order the columns.
  join.df.2 <- join.df.2[, names(join.df.2)]
  
  join.df.3 <- merge(join.df.2, sites, by = c("SITE", "SITE_NAME", "AGENCY", "LATITUDE", "LONGITUDE"), all = TRUE)
  final.df <- merge(join.df.3, gages, by = c("ICP_ID", "SITE", "AGENCY"), all = TRUE)
  return(final.df)
}

agg_my_data <- function(join.df, my.id, gages, sites){
  agg.count <- aggregate(AGENCY ~ SITE +
                           #ICP_ID +
                           ICPRB_NAME + #PARAMETER + 
                           YEAR, data = join.df, length)
  agg.count <- agg.count[agg.count$AGENCY >= 10, ]
  agg.count2 <- aggregate(AGENCY ~ SITE +
                            #ICP_ID +
                            ICPRB_NAME, #PARAMETER, 
                          data = agg.count, length)
  agg.count2 <- agg.count2[agg.count2$AGENCY >= 10, ]
  
  #join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "PARAMETER"), all.y = TRUE)
  join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "ICPRB_NAME"), all.y = TRUE)
  table(join.df.2[join.df.2$SITE %in% "USGS-01578310", "ICPRB_NAME"])
  # Re-order the columns.
  join.df.2 <- join.df.2[, names(join.df.2)]
  
  join.df.3 <- merge(join.df.2, sites, by = c("SITE", "SITE_NAME", "AGENCY", "LATITUDE", "LONGITUDE"), all = TRUE)
  final.df <- merge(join.df.3, gages, by = c("ICP_ID", "SITE", "AGENCY"), all = TRUE)
  
  if (my.id %in% "ICP_ID") {
    agg.count <- aggregate(AGENCY ~ ICP_ID +
                             YEAR, data = final.df, length)
    agg.count <- agg.count[agg.count$AGENCY >= 10, ]
    agg.count2 <- aggregate(AGENCY ~ ICP_ID, #PARAMETER, 
                            data = agg.count, length)
    agg.count2 <- agg.count2[agg.count2$AGENCY >= 10, ]
    
    #join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "PARAMETER"), all.y = TRUE)
    final.df <- merge(final.df, agg.count2[, 1:2], by = c("ICP_ID"), all.y = TRUE)
  }
  final.df <- unique(final.df)
  return(final.df)
}

agg_my_data <- function(join.df, my.id, gages, sites){

  agg.count <- aggregate(AGENCY ~ SITE +
                           #ICP_ID +
                           ICPRB_NAME + #PARAMETER + 
                           YEAR, data = join.df, length)
  agg.count <- agg.count[agg.count$AGENCY >= 10, ]
  agg.count2 <- aggregate(AGENCY ~ SITE +
                            #ICP_ID +
                            ICPRB_NAME, #PARAMETER, 
                          data = agg.count, length)
  agg.count2 <- agg.count2[agg.count2$AGENCY >= 10, ]
  
  sub.df <- join.df %>% 
    dplyr::group_by(SITE, ICPRB_NAME, YEAR) %>% 
    dplyr::summarise(COUNT = length(AGENCY)) %>% 
    dplyr::filter(COUNT >= 10) %>% 
    dplyr::group_by(SITE, ICPRB_NAME) %>% 
    dplyr::summarise(COUNT = length(COUNT)) %>% 
    dplyr::filter(COUNT >= 10) %>% 
    dplyr::select(1:2)
  
  join.df2 <- dplyr::left_join(sub.df, join.df, by = c("SITE", "ICPRB_NAME")) %>% 
    dplyr::full_join(, )
  


  
  #join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "PARAMETER"), all.y = TRUE)
  join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "ICPRB_NAME"), all.y = TRUE)
  table(join.df.2[join.df.2$SITE %in% "USGS-01578310", "ICPRB_NAME"])
  # Re-order the columns.
  join.df.2 <- join.df.2[, names(join.df.2)]
  
  join.df.3 <- merge(join.df.2, sites, by = c("SITE", "SITE_NAME", "AGENCY", "LATITUDE", "LONGITUDE"), all = TRUE)
  final.df <- merge(join.df.3, gages, by = c("ICP_ID", "SITE", "AGENCY"), all = TRUE)
  
  if (my.id %in% "ICP_ID") {
    agg.count <- aggregate(AGENCY ~ ICP_ID +
                             YEAR, data = final.df, length)
    agg.count <- agg.count[agg.count$AGENCY >= 10, ]
    agg.count2 <- aggregate(AGENCY ~ ICP_ID, #PARAMETER, 
                            data = agg.count, length)
    agg.count2 <- agg.count2[agg.count2$AGENCY >= 10, ]
    
    #join.df.2 <- merge(join.df, agg.count2[, 1:2], by = c("SITE", "PARAMETER"), all.y = TRUE)
    final.df <- merge(final.df, agg.count2[, 1:2], by = c("ICP_ID"), all.y = TRUE)
  }
  final.df <- unique(final.df)
  return(final.df)
}

final.site <- agg_my_data(join.df, "SITE", gages, sites)
#final.icp <- agg_my_data(join.df, "ICP_ID", gages, sites)
#==============================================================================
setwd("H:/Projects/EPA3Trends/Data/Data_May2017")
todays.date <- Sys.Date()
count.df <- data.frame(ICPRB_NAME = unique(final.site$ICPRB_NAME))
count.df$COUNT <- 1:nrow(count.df)
final.dt <- data.table::as.data.table(final.site)
system.time(
lapply(unique(final.dt$ICPRB_NAME), function(param.x) {
  sub.count <- count.df[count.df$ICPRB_NAME %in% param.x, ]
  print(paste(param.x, paste0("(", sub.count$COUNT, "/", nrow(count.df), ")")))
  sub.final <- final.dt[final.dt$ICPRB_NAME %in% param.x, ]
  file.name <- paste0(paste(param.x, todays.date, sep = "_"), ".csv")
  write.csv(sub.final, file.name, row.names = FALSE)
})
)
#write.csv(final.dt, "wq_data_5_1_2017.csv", row.names = FALSE)


#==============================================================================
# Connect to the PostgreSQL database "WQT".
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Export the join.df file to the WQT database.
dbWriteTable(con, "Trend_Data", final.site, overwrite = TRUE , row.names = FALSE)
#==============================================================================
param_range <- function(site){
  #============================================================================
  # Prepare Date
  #============================================================================
  # Identify column "DATE" as class date
  site$DATE <- as.Date(site$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  site$YEAR <- format(site$DATE, "%Y")
  # Create a new column by extracting the month from date
  site$MONTH <- format(site$DATE, "%m")
  #============================================================================
  # Sequence Dates
  #============================================================================
  site$REPORTED_VALUE <- as.numeric(as.character(site$REPORTED_VALUE))
  site <- site[,c("SITE", "MONTH", "YEAR",# "PARAMETER",
                  "ICPRB_NAME",
                  "REPORTED_VALUE", "ICPRB_UNITS")]
  site <- site[site$YEAR >= 1972, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(site)
  long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR,# PARAMETER,
                               ICPRB_NAME, ICPRB_UNITS)]
  
  
  #============================================================================
  print("Calculating Paramter Min/Max...")
  #max.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, max)
  max.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, max)
  #function(x) quantile(x, 0.95))
  #names(max.df) <- c("PARAMETER", "MAX")
  names(max.df) <- c("ICPRB_NAME", "MAX")
  #min.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, min)
  min.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, min)
  #function(x) quantile(x, 0.05))
  #names(min.df) <- c("PARAMETER", "MIN")
  names(min.df) <- c("ICPRB_NAME", "MIN")
  #final.df <- merge(min.df, max.df, by = "PARAMETER")
  final.df <- merge(min.df, max.df, by = "ICPRB_NAME")
  #============================================================================
  
  return(final.df)
}
#==============================================================================
param.range <- param_range(final.site)
#Updated on 2/28/17
# Using the outlier range to create the parameter gradient.
outliers <- dbReadTable(con, "Outliers")
param.range <- outliers[, c("PARAMETER", "LOW_FENCE_4.5", "UP_FENCE_4.5")]
names(param.range) <- c("ICPRB_NAME", "MIN", "MAX")
param.range$MIN <- ifelse(param.range$ICPRB_NAME %in% "TEMP", 0, param.range$MIN)
param.range$MIN <- ifelse(param.range$MIN <= 0, 0, param.range$MIN)
# Export the join.df file to the WQT database.
dbWriteTable(con, "param_range", param.range, overwrite = TRUE , row.names = FALSE)
#==============================================================================
huc <- unique(final.site[, c("SITE", "ICP_ID", "HUC_8")])
# Export the join.df file to the WQT database.
dbWriteTable(con, "huc_8", huc, overwrite = TRUE , row.names = FALSE)
#==============================================================================
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Connect to the PostgreSQL database "WQT2".
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT3", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Aggregate data by ICP_ID.
final.icp$ICP_ID <- as.character(final.icp$ICP_ID)
for (i in seq_along(unique(final.icp$ICP_ID))) {
  print(paste(i, "/", length(unique(final.icp$ICP_ID)), sep = ""))
  sub.final <- final.icp[final.icp$ICP_ID %in% unique(final.icp$ICP_ID)[i], ]
  sub.final <- sub.final[, !names(sub.final) %in% "EVENT_ID"]
  dbWriteTable(con, unique(final.icp$ICP_ID)[i], sub.final,
               overwrite = TRUE,
               row.names = FALSE)
  #for (j in 1:10) print(j)
}
#==============================================================================
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Connect to the PostgreSQL database "WQT2".
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT2", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Connect to the PostgreSQL database "WQT2".
con = dbConnect("PostgreSQL",
                dbname = "icprbcoo_wqt",
                   host = "icprbcoop.org",
                   user = "icprbcoo",
                   password = "5v4i1K3Unf",
                   port = 5432)
on.exit(dbDisconnect(con), add = TRUE)


con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT_May", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Aggregate data by Site. 
lapply (seq_along(unique(final.site$SITE)), function(i) {
  print(paste(i, "/", length(unique(final.site$SITE)), sep = ""))
  sub.final <- final.site[final.site$SITE %in% unique(final.site$SITE)[i], ]
  sub.final <- sub.final[, !names(sub.final) %in% "EVENT_ID"]
  dbWriteTable(conn = con,
               name = paste0("site_", unique(final.site$SITE)[i]),
               value = sub.final,
               overwrite = TRUE,
               row.names = FALSE)
  #for (j in 1:10) print(j)
})

#==============================================================================
param_range <- function(site){
  #============================================================================
  # Prepare Date
  #============================================================================
  # Identify column "DATE" as class date
  site$DATE <- as.Date(site$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  site$YEAR <- format(site$DATE, "%Y")
  # Create a new column by extracting the month from date
  site$MONTH <- format(site$DATE, "%m")
  #============================================================================
  # Sequence Dates
  #============================================================================
  site$REPORTED_VALUE <- as.numeric(as.character(site$REPORTED_VALUE))
  site <- site[,c("SITE", "MONTH", "YEAR",# "PARAMETER",
                  "ICPRB_NAME",
                  "REPORTED_VALUE", "ICPRB_UNITS")]
  site <- site[site$YEAR >= 1972, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(site)
  long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR,# PARAMETER,
                               ICPRB_NAME, ICPRB_UNITS)]
  
  
  #============================================================================
  print("Calculating Paramter Min/Max...")
  #max.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, max)
  max.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, max)
  #function(x) quantile(x, 0.95))
  #names(max.df) <- c("PARAMETER", "MAX")
  names(max.df) <- c("ICPRB_NAME", "MAX")
  #min.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, min)
  min.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, min)
  #function(x) quantile(x, 0.05))
  #names(min.df) <- c("PARAMETER", "MIN")
  names(min.df) <- c("ICPRB_NAME", "MIN")
  #final.df <- merge(min.df, max.df, by = "PARAMETER")
  final.df <- merge(min.df, max.df, by = "ICPRB_NAME")
  #============================================================================
  
  return(final.df)
}
#==============================================================================
param.range <- param_range(final.site)
#Updated on 5/19/17
dbWriteTable(con, "outliers", outliers, overwrite = TRUE , row.names = FALSE)
# Using the outlier range to create the parameter gradient.
param.range <- outliers[, c("PARAMETER", "LOW_FENCE_4.5", "UP_FENCE_4.5")]
names(param.range) <- c("ICPRB_NAME", "MIN", "MAX")
param.range$MIN <- ifelse(param.range$ICPRB_NAME %in% "TEMP", 0, param.range$MIN)
param.range$MIN <- ifelse(param.range$MIN <= 0, 0, param.range$MIN)
# Export the join.df file to the WQT database.
dbWriteTable(con, "param_range", param.range, overwrite = TRUE , row.names = FALSE)
#==============================================================================
huc <- unique(final.site[, c("SITE", "ICP_ID", "HUC_12", "ICPRB_NAME")])
huc$HUC_8 <- substr(as.character(huc$HUC_12), 1, 7)
# Export the join.df file to the WQT database.
dbWriteTable(con, "huc_8", huc, overwrite = TRUE , row.names = FALSE)
#==============================================================================
write.csv(final.site, "param_data_5_22_17.csv", row.names = FALSE)
write.csv(outliers, "outliers_5_22_17.csv", row.names = FALSE)
write.csv(param.range, "param_range_5_22_17.csv", row.names = FALSE)
write.csv(huc, "huc_5_22_17.csv", row.names = FALSE)
