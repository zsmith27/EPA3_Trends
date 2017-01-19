#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/18/17
# Updated: 1/18/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to aggregate and count WQP parameter reports 
#          by year.
# Output: Creates a single .csv file with parameter counts by year.
#==============================================================================
#==============================================================================
# Only need to import the single WQP file containing all of the EPA3 sites
# selected by Buchanan.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
wqp.df <- read.csv("wqp_keep_stations.csv")
#==============================================================================
# Load the parameters of interest.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
params <- read.csv("wqp_params_01_12_17.csv", stringsAsFactors = FALSE)
# Keep only the necessary columns for assigning generalized parameter names.
params <- unique(params[, c("CharacteristicName", "PARAM")])
#==============================================================================
# Merge the data frames to add the generalized parameter names.
wqp <- merge(wqp.df, params, by = "CharacteristicName")
#==============================================================================
# Create a year column and remove any data prior to 1999.
wqp$ActivityStartDate <- as.Date(wqp$ActivityStartDate)
wqp$YEAR <- format(wqp$ActivityStartDate, "%Y")
wqp <- wqp[wqp$YEAR >= 1999, ]
#==============================================================================
# count the number of parameter records per year for each station.
agg.df <- aggregate(ResultMeasureValue ~ MonitoringLocationIdentifier + 
                      OrganizationIdentifier + PARAM + YEAR,
                    data = wqp, FUN = length)
# Transform to a wide format.
wide.df <- tidyr::spread(agg.df,  YEAR, ResultMeasureValue)
# Organize in alphabetical order by PARAM and OrganizationIdentifier.
wide.df <- wide.df[order(wide.df$PARAM, wide.df$OrganizationIdentifier), ]
# Replace all NAs with "blanks."
wide.df[is.na(wide.df)] <- ""
#==============================================================================
# Export as a .csv
write.csv(wide.df, "WQP_by_Year_1_18_17.csv", row.names = F)
