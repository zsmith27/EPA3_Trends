#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 01/10/17
# Updated: 07/10/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to subset the WQP data to only include
#          data from 1965 to present and long-term monitoring stations 
#          identified in the Legacy STORET. The script has since been updated
#          to simply join the seperate WQP data.
# Output: Creates csv file of all of the WQP data.
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Set working directory.
main.dir <- "//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data"
#------------------------------------------------------------------------------
# Specify columns to be read as class "Character".
char.cols <- "DetectionQuantitationLimitMeasure.MeasureValue"
# Import the data for each state. 
# The data was previously subset to only include potential params of interest.
dc <- data.table::fread(file.path(main.dir, "sub_wqp_dc.csv"),
                        colClasses = list(character = char.cols))
de <- data.table::fread(file.path(main.dir, "sub_wqp_de.csv"),
                        colClasses = list(character = char.cols))
md <- data.table::fread(file.path(main.dir, "sub_wqp_md.csv"),
                        colClasses = list(character = char.cols))
pa <- data.table::fread(file.path(main.dir, "sub_wqp_pa.csv"),
                        colClasses = list(character = char.cols))
va <- data.table::fread(file.path(main.dir, "sub_wqp_va.csv"),
                        colClasses = list(character = char.cols))
wv <- data.table::fread(file.path(main.dir, "sub_wqp_wv.csv"),
                        colClasses = list(character = char.cols))
#------------------------------------------------------------------------------
# Join all of the station information into a single data frame.
bound <- bind_rows(dc, de, md, pa, va, wv)
#------------------------------------------------------------------------------
# Load the WQP station information
station.dir <- "//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data/Site_Data"
wqp.station <- data.table::fread(file.path(station.dir, "sub_site_all.csv"))
#------------------------------------------------------------------------------
# Join the site info with the param data.
by.names <- c("OrganizationIdentifier", "OrganizationFormalName",
              "MonitoringLocationIdentifier") #, "MonitoringLocationName")
final.df <- dplyr::left_join(bound, wqp.station, by = by.names )
#------------------------------------------------------------------------------
# Export all data as a single table.
export.dir <- "//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data"
data.table::fwrite(final.df, file.path(export.dir, "wqp_keep_stations.csv"))
#------------------------------------------------------------------------------



