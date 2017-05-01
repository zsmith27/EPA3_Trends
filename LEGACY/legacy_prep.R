#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/10/17
# Updated: 2/01/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to subset the WQP data to only include
#          data from 1965 to present and long-term monitoring stations 
#          identified in the Legacy STORET.
# Output: Creates individual ".txt" files for each EPA Region 3 State.
#==============================================================================
#==============================================================================
# Load stations and parameters specified by Buchanan.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
stations.df <- read.table("LegacyStationsAll_050316Claire.txt",
                          header = TRUE, stringsAsFactors = FALSE, 
                          strip.white = TRUE)
#==============================================================================
# Load Legacy STORET data.
setwd("//Pike/data/Projects/EPA3Trends/Data/Legacy STORET/Aggregate Raw Data/Legacy Res")
de.df <- read.table("DELAWARE.txt", stringsAsFactors = FALSE)
dc.df <- read.table("DISTRICT.txt", stringsAsFactors = FALSE)
md.df <- read.table("MARYLAND.txt", stringsAsFactors = FALSE)
pa.df <- read.table("PENNSYLVANIA.txt", stringsAsFactors = FALSE)
va.df <- read.table("VIRGINIA.txt", stringsAsFactors = FALSE)
wv.df <- read.table("WEST_VIRGINIA.txt", stringsAsFactors = FALSE)
#==============================================================================
# Load the parameter codes and reporting unit codes.
#setwd("//Pike/data/Projects/EPA3Trends/Data/Legacy STORET/Parameter Code and Reporting Units")
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Claire_Specifications/1_24_2017")
# This file contains the Legacy STORET parameter codes selected by Buchanan.
#keep.params <- read.csv("legacy_select_params.csv", colClasses = "character")
keep.params <- read.csv("Legacy_Specifications_1_24_2017.csv", colClasses = "character")

setwd("//Pike/data/Projects/EPA3Trends/Data/Legacy STORET/Parameter Code and Reporting Units")
# This file contains all of the Legacy STORET parameter codes.
param <- read.csv("legacy_paramters.csv", stringsAsFactors = FALSE,
                  colClasses = c(rep("character", 21)), strip.white = TRUE)
# Subset param to only include the selected parameters.
param <- param[param$PARAMETER_NUMBER %in% keep.params$PARAMETER_NUMBER, 1:17]

# Load reporting unit codes.
r_units <- read.csv("legacy_reporting_units.csv", stringsAsFactors = FALSE,
                    strip.white = TRUE)

# Merge the parameter codes with the reporting unit codes.
param <- merge(param, r_units, by = "REPORTING_CODE")
#==============================================================================

state <- de.df
stations <- stations.df
params <- param
new.params <- keep.params

subset_legacy <- function(state, stations, params, new.params) {
  # Remove leading and trailing white space.
  char.cols <- sapply(state, class) %in% c('character', 'factor')
  # Remove leading and trailing white space from characters and factors.
  state[, char.cols] <- apply(state[, char.cols], 2, trimws)
  #state <- data.frame(lapply(state, trimws))
  names(state) <- trimws(toupper(names(state)))
  
  # Keep only selected stations and parameters.
  state <- state[state$STATION %in% unique(stations.df$Station), ]
  state <- state[state$PARAM %in% params$PARAMETER_NUMBER, ]
  
  # Merge parameter information with state data.
  state <- merge(state, params, by.x = "PARAM", by.y = "PARAMETER_NUMBER")
  
  # Re-organize the columns.
  col.order <- c("STATION", "STATION.NAME", "AGENCY", "AGENCY.NAME",
                 "STATE.NAME", "COUNTY.NAME", "LATITUDE", "LONGITUDE", "HUC",
                 "START.DATE", "START.TIME", "END.DATE", "END.TIME",
                 "SAMPLE.DEPTH", "UMK", "REPLICATE.NUMBER", "COMPOSITE_GRAB_NUMBER",
                 "PARAM", "RESULT.VALUE","REPORTING_UNITS", "REPORTING_CODE",
                 "DECIMAL_POINT_LOCATION", "SHORT_NAME", "FULL_NAME", "GROUP_CODE", 
                 "CHEMICAL_ABSTRACT_NUMBER",
                 "PRIMARY.ACTIVITY.CATEGORY", "SECONDARY.ACTIVITY.CATEGORY",
                 "SAMPLE_MEDIA", "ANALYSIS_TYPE", "PRIORITY_POLLUTANT",
                 "TOXIC_EXTRACTION", "PUBLISH", "SUB_GROUP_CODE",
                 "ENTRY_DATE", "ORIGIN", "REVISION_DATE", "TRACKING_CODE",
                  "CM", "CS", "R")
  
  state <- state[, col.order]
  
  #group.1000 <- c(1070, 82033, 82035, 82034, 980, 1045, 1049, 1051, 1055, 1090,
  #               1092, 1094, 1105, 4170, 1147, 32230)
  group.1000 <- c(1070, 82033, 82035, 82034)
  state$ICPRB_VALUE <- ifelse(state$PARAM %in% 11, (state$RESULT.VALUE - 32) / 1.8,
                                   ifelse(state$PARAM %in% group.1000, state$RESULT.VALUE * 1000,
                                          # meq/l to mq/l multiply by 50.
                                          # ueq/l to mq/l multiply by 50000 (50 * 1000)
                                          ifelse(state$PARAM %in% 409, state$RESULT.VALUE * 50000,
                                                 ifelse(state$PARAM %in% 71846, state$RESULT.VALUE * 0.777,
                                                        ifelse(state$PARAM %in% 71851, state$RESULT.VALUE * 0.226,
                                                               #ifelse(state$PARAM %in% 71886, state$RESULT.VALUE * 0.326,
                                                               ifelse(state$PARAM %in% 32230, state$RESULT.VALUE / 1000,
                                                                      state$RESULT.VALUE))))))
  
  final.df <- merge(new.params[, c("PARAMETER_NUMBER", "ICPRB_NAME", "UNITS")], state,
                    by.x = "PARAMETER_NUMBER", by.y = "PARAM", all.y = TRUE)
  
  return(final.df)
}
#==============================================================================
state.list <- list(de.df, dc.df, md.df, pa.df, va.df, wv.df)

final.list <- lapply(state.list, function(x) subset_legacy(x, stations.df, param, keep.params))

bound <- do.call(rbind, final.list)

#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Merged/Column_Names")
my.cols <- read.csv("column_names.csv", stringsAsFactors = FALSE)
my.cols$Legacy.Columns <- toupper(my.cols$Legacy.Columns)
final.df <- bound
add.cols <- my.cols[!my.cols$Legacy.Columns %in% names(final.df), "ICPRB.Columns"]
final.df[, add.cols] <- NA

sub.cols <- my.cols[my.cols$Legacy.Columns %in% names(final.df), ]
sub.cols <- sub.cols[match(names(final.df), sub.cols$Legacy.Columns),  ]
names(final.df) <- ifelse(names(final.df) %in% sub.cols$Legacy.Columns, sub.cols$ICPRB.Columns, names(final.df))

final.df <- final.df[, match(my.cols$ICPRB.Columns, names(final.df))]
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Output/1_26_2017")
write.csv(final.df, paste0("prep_merge_legacy_", Sys.Date(),".csv"), row.names = FALSE)
#==============================================================================
################################# OLD SCRIPT ################################## 
# 2-01-2017
#==============================================================================
final.df <- bound
final.df$HORIZONTAL_DATUM <- NA
order.cols <- c("AGENCY", "STATION", "LATITUDE", "LONGITUDE", "HORIZONTAL_DATUM",
               "START.DATE","SAMPLE.DEPTH", "REPLICATE.NUMBER", 
               "COMPOSITE_GRAB_NUMBER","ICPRB_NAME", "NEW_RESULT_VALUE",
               "UNITS")
final.df <- final.df[, order.cols]
#==============================================================================
# Rename the columns to bind with Legacy STORET.
new.names <- c("AGENCY", "SITE", "LATITUDE", "LONGITUDE", "HORIZONTAL_DATUM",
               "DATE", "DEPTH", "REPLICATE", "COMPOSITE", "PARAMETER",
               "REPORTED_VALUE", "UNITS")
names(final.df) <- new.names
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Output/1_26_2017")
write.csv(final.df, "prep_merge_legacy_1_30_17.csv", row.names = FALSE)
#==============================================================================
#==============================================================================
#==============================================================================
# Add Station Name for Nagel.
# 2/1/2017
final.df <- bound
final.df$HORIZONTAL_DATUM <- NA
order.cols <- c("AGENCY", "STATION", "STATION.NAME", "LATITUDE",
                "LONGITUDE", "HORIZONTAL_DATUM",
                "START.DATE","SAMPLE.DEPTH", "REPLICATE.NUMBER", 
                "COMPOSITE_GRAB_NUMBER","ICPRB_NAME", "RESULT.VALUE",
                "UNITS")
final.df <- final.df[, order.cols]
#==============================================================================
# Rename the columns to bind with Legacy STORET.
new.names <- c("AGENCY", "SITE", "SITE_NAME", "LATITUDE", "LONGITUDE",
               "HORIZONTAL_DATUM",
               "DATE", "DEPTH", "REPLICATE", "COMPOSITE", "PARAMETER",
               "REPORTED_VALUE", "UNITS")
names(final.df) <- new.names
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Legacy/R_Output")
write.csv(final.df,
          paste0("prep_merge_legacy_", Sys.Date(), ".csv"),
          row.names = FALSE)
#==============================================================================
#==============================================================================
#==============================================================================






