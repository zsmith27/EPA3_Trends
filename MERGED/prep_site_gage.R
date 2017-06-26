#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/26/17
# Updated: 6/06/17
# Maintained: Zachary M. Smith
# Purpose: To import and organize site and gage data.
#==============================================================================
#==============================================================================
# Load the necessary packages.
# dplyr and RPostgreSQL aid in the communication between R and Postgresql.
library(dplyr)
library(RPostgreSQL)
# Load the RODBC package to connect to an MS Access database.
library(RODBC)
#==============================================================================
# Search for all odbc connections on the computer.
odbcDataSources()
# Create a connection with the EPA REgaion 3 Water Quality Trends database.
channel <- odbcConnect("EPA3_WQT")
#==============================================================================
# Import the necessary MS Access tabs.
tab_sites <- sqlFetch(channel, "Sites", stringsAsFactors = FALSE)
tab_sites_additional <- sqlFetch(channel, "Sites_AdditionalInfo",
                                 stringsAsFactors = FALSE)
odbcCloseAll()
#-----------------------------------------------------------------------------
channel <- odbcConnect("WQT_June")
tab_gages <- sqlFetch(channel, "Site_Gage_List", stringsAsFactors = FALSE)
#tab_gage_flow <- sqlFetch(channel, "LTQ_Data", stringsAsFactors = FALSE)
tab_gage_info <- sqlFetch(channel, "LTQ_GageInfo", stringsAsFactors = FALSE)
# Close the odbc connection.
odbcCloseAll()
rm(channel)
#==============================================================================
names(tab_sites) <- names(tab_sites) %>% toupper()
tab_sites <- lapply(tab_sites, trimws) %>% data.frame(stringsAsFactors = FALSE)
tab_sites[, c("LATITUDE", "LONGITUDE")] <- tab_sites %>% 
  dplyr::select(LATITUDE, LONGITUDE) %>% 
  sapply(as.numeric) %>% 
  round(5) # six recommended by Nagel but did not merged correctly.
#------------------------------------------------------------------------------
names(tab_sites_additional) <- names(tab_sites_additional) %>% toupper()
tab_sites_additional <- lapply(tab_sites_additional, trimws) %>% 
  data.frame(stringsAsFactors = FALSE)
#------------------------------------------------------------------------------
names(tab_gages) <- names(tab_gages) %>% toupper()
tab_gages <- lapply(tab_gages, trimws) %>% data.frame(stringsAsFactors = FALSE)
#------------------------------------------------------------------------------
names(tab_gage_info) <- names(tab_gage_info) %>% toupper()
tab_gage_info <- lapply(tab_gage_info, trimws) %>%
  data.frame(stringsAsFactors = FALSE)
#------------------------------------------------------------------------------
#sites <- dplyr::full_join(tab_sites, tab_sites_additional[, 1:4],
#               by = c("ICP_ID", "DUP_ID"), all = TRUE)
#------------------------------------------------------------------------------
#gages <- merge(tab_gages, tab_gage_info, by = c("GAGE_ID"), all = TRUE)
#==============================================================================