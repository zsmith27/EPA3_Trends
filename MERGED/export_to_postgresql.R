#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/26/17
# Updated: 6/06/17
# Maintained: Zachary M. Smith
# Purpose: Merge the Legacy STORET data with the WQP data.
# Output: The join.df data is exported to the WQT PostgreSQL database.
#==============================================================================
#==============================================================================
# Load the necessary packages.
# dplyr and RPostgreSQL aid in the communication between R and Postgresql.
library(dplyr)
library(RPostgreSQL)
# Load the data.table package which allows for faster imports with "fread".
library(data.table)
#------------------------------------------------------------------------------
setwd("D:/ZSmith/Projects/WQ_Trends/EPA3_Trends/MERGED")
source("param_range_func.R")
source("merge_legacy_wqp_dplyr.R")
#==============================================================================
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT_May", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#==============================================================================
# Aggregate data by Site. 
final.df <- as.data.frame(final.df)
lapply (seq_along(unique(final.df$SITE)), function(i) {
  print(paste(i, "/", length(unique(final.df$SITE)), sep = ""))
  sub.final <- final.df[final.df$SITE %in% unique(final.df$SITE)[i], ]
  sub.final <- sub.final[, !names(sub.final) %in% "EVENT_ID"]
  dbWriteTable(conn = con,
               name = paste0("site_", unique(final.df$SITE)[i]),
               value = sub.final,
               overwrite = TRUE,
               row.names = FALSE)
  #for (j in 1:10) print(j)
})
#==============================================================================

dbWriteTable(con, "param_data", final.df, overwrite = TRUE , row.names = FALSE)
dbWriteTable(con, "gage_info", tab_gage_info, overwrite = TRUE , row.names = FALSE)
param.range <- param_range(final.df)
#Updated on 5/19/17
#(con, "outliers", outliers, overwrite = TRUE , row.names = FALSE)
outliers <- dbReadTable(con, "Outliers")
# Using the outlier range to create the parameter gradient.
param.range <- outliers[, c("PARAMETER", "LOW_FENCE_4.5", "UP_FENCE_4.5")]
names(param.range) <- c("ICPRB_NAME", "MIN", "MAX")
param.range$MIN <- ifelse(param.range$ICPRB_NAME %in% "TEMP", 0, param.range$MIN)
param.range$MIN <- ifelse(param.range$MIN <= 0, 0, param.range$MIN)
# Export the join.df file to the WQT database.
dbWriteTable(con, "param_range", param.range, overwrite = TRUE , row.names = FALSE)
#==============================================================================
huc <- final.df %>% 
  dplyr::select(SITE, ICP_ID, HUC_8, ICPRB_NAME) %>% 
  unique.data.frame()
huc <- unique(final.df[, c("SITE", "ICP_ID", "HUC_8", "ICPRB_NAME")])
# Export the join.df file to the WQT database.
dbWriteTable(con, "huc_8", huc, overwrite = TRUE , row.names = FALSE)
#==============================================================================
# Clear Workspace to import flow data.
remove.vec <- ls()[!ls() %in% c("con")]
rm(list = remove.vec)
#------------------------------------------------------------------------------
# Connect to the MS Acccess database.
#channel <- RODBC::odbcConnect("EPA3_WQT")
# Import flow data from MS Access database.
#tab_gage_flow <- sqlFetch(channel, "LTQ_Data", stringsAsFactors = FALSE)
tab_gage_flow <- data.table::fread("H:/Projects/EPA3Trends/Data/Data_May2017/Data_Flow_nocomposits_May2017.txt")
names(tab_gage_flow) <- names(tab_gage_flow) %>% toupper()
tab_gage_flow <- tab_gage_flow %>% 
  dplyr::select(DATE, GAGE_ID, FLOW, FLAG) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))
# Close the channel.
RODBC::odbcCloseAll()
rm(channel)
#------------------------------------------------------------------------------
# Eport flow data to postgreSQL.
dbWriteTable(con, "gage_flow", tab_gage_flow, overwrite = TRUE , row.names = FALSE)
#------------------------------------------------------------------------------
# End
