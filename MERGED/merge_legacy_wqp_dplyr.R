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
#==============================================================================
#setwd("D:/ZSmith/Projects/WQ_Trends/EPA3_Trends/MERGED")
source("MERGED/prep_site_gage.R")
# Set the working directory to the location of the flow data.
#setwd("H:/Projects/EPA3Trends/Data/Data_May2017")
#gage.df <- data.table::fread("Data_Flow_nocomposits_May2017.txt")
#names(gage.df) <- names(gage.df) %>% toupper() %>%  trimws()
#gage.df <- dplyr::rename(gage.df, ICPRB_NAME = PARAMETER)
# Keep only the necessary columns to merge with the legacy and wqp dataframes
# and to be able to identify the appropriate gage.
#sub.gage <- dplyr::select(gage.df, ICP_ID, DUP_ID, ICP_STATION,
#                          SITE, ICPRB_NAME, GAGE_ID) %>% 
#  dplyr::distinct()
# Remove the gage dataframe.
#rm(gage.df)
#==============================================================================
# Set working directory to the location of the legacy and wqp files.
main.dir <- "H:/Projects/EPA3Trends/Data/Data_May2017/merged/output"
legacy.dir <- file.path(main.dir, "prep_merge_legacy_2017-05-22.csv")
# Import Legacy and WQP data.
legacy <- data.table::fread(legacy.dir,
                            colClasses = list(character = "ICPRB_CONVERSION",
                                              "TIME", "END_TIME"))
#------------------------------------------------------------------------------
# Vector of columns to be read as class character.
char.cols <- c("ICPRB_CONVERSION", "LaboratoryName", "PreparationStartDate",
               "MeasureQualifierCode", "SampleAquifer",
               "ResultDepthHeightMeasure.MeasureUnitCode", 
               "ResultDepthAltitudeReferencePointText", "StatisticalBaseCode",
               "ActivityDepthAltitudeReferencePointText",
               "ActivityTopDepthHeightMeasure.MeasureUnitCode",
               "ActivityBottomDepthHeightMeasure.MeasureUnitCode")
wqp.dir <- file.path(main.dir, "prep_merge_wqp_2017-06-29.csv")
wqp <- data.table::fread(wqp.dir, colClasses = list(character = char.cols))
#==============================================================================
wqp$SITE <- paste(unique(wqp$AGENCY), "-", sep = "") %>% 
  paste0(collapse = "|") %>% 
  gsub(., "", wqp$SITE)
# Remove "_WQX" suffix so that the AGENCY column from the wqp will have the
# same format as the AGENCY column in legacy.
wqp$AGENCY <- gsub("_WQX", "", wqp$AGENCY)
#==============================================================================
# Join the two data sets and remove duplicate rows
join.df <- dplyr::bind_rows(legacy, wqp) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(AGENCY, SITE, ICPRB_NAME, DATE, Replicate.Number, DEPTH)
# Remove legacy and wqp dataframes to manage RAM.
rm(legacy, wqp)
#==============================================================================
# Make sure the DATE column is class date and create a MONTH and YEAR column
# from the DATE column.
join.df <- dplyr::mutate(join.df,
                         DATE = as.Date(DATE),
                         MONTH = format(DATE, "%m"),
                         YEAR = format(DATE, "%Y"))
# Remove any data collected prior to 1972. Also, keep only samples collected at
# the surface (0m) or close to the surface (< 1m). NA's were assumed to
# represent surface measurements. However, this assumption could be wrong.
join.df <- join.df %>% 
  dplyr::filter(YEAR >= 1972,
                DEPTH < 1 | is.na(DEPTH))
#==============================================================================
# Parameters of intrest with a low number of reporting values (< 100) where
# present in the data set because they were associated with stations with 
# at least one other paramter with a high number of reporting values(>= 100).
# Previous specifiations were to pull a specific list of stations and a specific
# list of paramters, but parameters were not strictly associated with a 
# particular station. The script below counts the data by Site and Parameter,
# and removes rows associated with Site/paramters with < 100 counts.
sub.df <- join.df %>% 
  dplyr::group_by(SITE, ICPRB_NAME, YEAR) %>% 
  dplyr::summarise(COUNT = length(AGENCY)) %>% 
  dplyr::filter(COUNT >= 10) %>% 
  dplyr::group_by(SITE, ICPRB_NAME) %>% 
  dplyr::summarise(COUNT = length(COUNT)) %>% 
  dplyr::filter(COUNT >= 10) %>% 
  dplyr::select(1:2)
#------------------------------------------------------------------------------
# Round the Lat/Longs to the fifth decimal place so that they merge
# correctly below.
join.df[, c("LATITUDE", "LONGITUDE")] <- join.df %>% 
  dplyr::select(LATITUDE, LONGITUDE) %>% 
  round(5) # six recommended by Nagel but did not merged correctly.
#------------------------------------------------------------------------------
# Removed DUP_ID because it was creating duplicates.
sub.sites <- tab_sites  %>% 
  dplyr::select(-DUP_ID) %>% 
  distinct()
#------------------------------------------------------------------------------
# keep only the Site and Parameter combinations with minimum of 10 observations
# per year for a minimum of 10 years.  Also, merge the gage information.
final.df <- dplyr::left_join(sub.df, join.df, by = c("SITE", "ICPRB_NAME")) %>% 
  dplyr::left_join(., sub.sites, by = c("AGENCY", "SITE", "SITE_NAME",
                                        "LATITUDE", "LONGITUDE")) %>% 
  dplyr::left_join(., unique(tab_gages[, c("ICP_ID", "AGENCY", "SITE", "GAGE_ID")]),
                   by = c("ICP_ID", "AGENCY", "SITE"))
# Remove join.df, sub.df, and sub.gage to manage RAM.
rm(join.df, sub.df)
#==============================================================================
# Replace all blanks with NA
#==============================================================================
final.df <- final.df %>% 
  mutate_if(is.character, funs(replace(., . == "", NA)))
#==============================================================================
#------------------------------------------------------------------------------
sub.param <- final.df %>% 
  select(AGENCY, SITE, ICP_ID, ICPRB_NAME, DATE, CENSORED) %>% 
  unique()
#------------------------------------------------------------------------------
# Remove Sites/Parameters with > 50% of the data represented by Censored values.
#------------------------------------------------------------------------------
# Identify data with > 50% Censored values.
censored.df <- sub.param %>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME, CENSORED) %>% 
  summarise(COUNT = length(CENSORED))%>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  mutate(TOTAL = sum(COUNT)) %>% 
  filter(CENSORED == "Censored") %>% 
  mutate(PERCENT = COUNT / TOTAL * 100) %>% 
  filter(PERCENT > 50)
# Remove data with > 50% Censored values.
final.df <- anti_join(final.df, censored.df,
                       by = c("AGENCY", "SITE", "ICP_ID", "ICPRB_NAME"))
#------------------------------------------------------------------------------
# Remove Quality Control values which are creating approximate duplicates.
final.df <- final.df[!grepl("Quality", final.df$ACTIVITY_TYPE), ]
#------------------------------------------------------------------------------
# Correct time related columns.
# Keep this for potential later use.
time.df <- final.df %>% 
  mutate(END_TIME = case_when(
    nchar(END_TIME, keepNA = FALSE) == 4 ~ 
      paste0(substr(END_TIME, 1, 2), ":", substr(END_TIME, 3,4), ":00"),
    nchar(END_TIME, keepNA = FALSE) == 3 ~ 
      paste0("0", substr(END_TIME, 1, 1), ":", substr(END_TIME, 2,3), ":00"),
    nchar(END_TIME, keepNA = FALSE) != 4 | 
      nchar(END_TIME, keepNA = FALSE) != 3 ~ END_TIME),
    TIME = case_when(
      nchar(TIME, keepNA = FALSE) == 4 ~ 
        paste0(substr(TIME, 1, 2), ":", substr(TIME, 3,4), ":00"),
      nchar(TIME, keepNA = FALSE) == 3 ~ 
        paste0("0", substr(TIME, 1, 1), ":", substr(TIME, 2,3), ":00"),
      nchar(TIME, keepNA = FALSE) != 4 | 
        nchar(TIME, keepNA = FALSE) != 3 ~ TIME))
rm(time.df)
#------------------------------------------------------------------------------
# Remove time columns because they contain many inconsistencies.
# Examples: Does a reported time of 74 mean 7:40AM?
#           Does a reported time of 1 mean 1:00AM?
#           What does 56:40 mean?
#final.df <- final.df %>% 
#  select(-TIME, -END_TIME, -ActivityStartTime.TimeZoneCode,
#         -ActivityEndTime.TimeZoneCode) %>% 
#  distinct()
#==============================================================================
# End

