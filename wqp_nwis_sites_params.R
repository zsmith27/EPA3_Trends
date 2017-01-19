#==============================================================================
# Author: Zachary M. Smith
# Created: 1-18-17
# Updated: 1-19-17
# Maintained: Zachary M. Smith
# Purpose: The script was written to explore and select USGS NWIS sites and
#          parameters from the WQP for long-term tred analysis.
# Output: Generates a table of sites/parameter sampling event counts per year
#         (1972-2016).
#==============================================================================
#==============================================================================
# Set the working directory
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Raw Data")
#==============================================================================
# Import the raw state data
dc.df  <- read.csv("wqp_dc_raw.csv", stringsAsFactors = FALSE)
de.df  <- read.csv("wqp_de_raw.csv", stringsAsFactors = FALSE)
# Maryland's data set was very large and had to be downloaded as multiple files
# This script downloads each file and combines them...
md.df1  <- read.csv("wqp_md_raw_1.csv", stringsAsFactors = FALSE)
md.df2  <- read.csv("wqp_md_raw_2.csv", stringsAsFactors = FALSE)
md.df3  <- read.csv("wqp_md_raw_3.csv", stringsAsFactors = FALSE)
md.df4  <- read.csv("wqp_md_raw_4.csv", stringsAsFactors = FALSE)
md.df5  <- read.csv("wqp_md_raw_5.csv", stringsAsFactors = FALSE)
# Append the MD dataframes.
md.df <- rbind(md.df1, md.df2, md.df3, md.df4, md.df5)
# Remove the individual MD dataframes, so that they do not hog ram.
rm(md.df1, md.df2, md.df3, md.df4, md.df5)

pa.df  <- read.csv("wqp_pa_raw.csv", stringsAsFactors = FALSE)
va.df  <- read.csv("wqp_va_raw.csv", stringsAsFactors = FALSE)
wv.df  <- read.csv("wqp_wv_raw.csv", stringsAsFactors = FALSE)
#==============================================================================
# This function selects only rows that were provided by NWIS.
just_nwis <- function(state) state[state$ProviderName %in% "NWIS", ]
# Apply the just_nwis function to each state and merge all of the state data
# into a single dataframe.
bound <- rbind(just_nwis(dc.df), just_nwis(de.df), just_nwis(md.df),
               just_nwis(pa.df), just_nwis(va.df), just_nwis(wv.df))
#==============================================================================
# Create a year column and remove any data prior to 1999.
bound$ActivityStartDate <- as.Date(bound$ActivityStartDate)
bound$YEAR <- format(bound$ActivityStartDate, "%Y")
bound <- bound[bound$YEAR >= 1972, ]
#==============================================================================
# count the number of parameter records per year for each station.
agg.df <- aggregate(ResultMeasureValue ~ MonitoringLocationIdentifier + 
                      OrganizationIdentifier + USGSPCode + YEAR,
                    data = bound, FUN = length)
# Transform to a wide format.
wide.df <- tidyr::spread(agg.df,  YEAR, ResultMeasureValue)
# Organize in alphabetical order by PARAM and OrganizationIdentifier.
wide.df <- wide.df[order(wide.df$USGSPCode, wide.df$OrganizationIdentifier), ]

# This function counts the number of years with >= 10 sampling events for each
# row.
count_cols <- function(wide){
  wide[is.na(wide)] <- 0
  wide[, 4:48] <- ifelse(wide[, 4:48] >= 10, 1, 0)
  return(rowSums(wide[, 4:48]))
}

# Apply the function count_cols to wide.df.
wide.df$COUNT <- count_cols(wide.df)
# Replace all NAs with "blanks."
wide.df[is.na(wide.df)] <- ""
#==============================================================================
#==============================================================================
# Import NWIS paramter code defitions.
# Link: https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/NWIS/NWIS_Params")
nwis.params <- read.csv("nwis_params.csv", stringsAsFactors = FALSE)
# Keep only code and description columns.
np <- nwis.params[, c(1, 3)]
# Merge counts by year with parameter code descriptions.
merged <- merge(np, wide.df, by.x = "parameter_cd", by.y = "USGSPCode", all.y = T)
# Order counts in decsending order.
merged <- merged[order(-merged$COUNT), ]
# Keep only the rows with COUNT >= 10.
review <- unique(merged[merged$COUNT >= 10, 1:2])
review <- review[order(review$parameter_cd), ]

# Export "review" to be review by Buchanan.
setwd("C:/Users/zsmith/Desktop")
write.csv(review, "nwis_params_for_review.csv")
#==============================================================================
#==============================================================================
# Imported the selected NWIS parameters.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/NWIS/NWIS_Params")
nwis.params <- read.csv("nwis_params_reviewed.csv", stringsAsFactors = FALSE)
# Keep only the selected parameters and the necessary columns.
np <- nwis.params[nwis.params$Keep == TRUE, 2:3]

# Merge the parameter descriptions with the yearly counts by station/paramter.
merged <- merge(np, wide.df, by.x = "parameter_cd", by.y = "USGSPCode", all.y = T)
# Remove any parameters without a description.
merged <- merged[!is.na(merged$parameter_nm), ]
# Remove any sites/paramters with less than 10 counts of years with 10 or more
# sampling events reported.
merged <- merged[merged$COUNT >= 10, ]
# Sort in descending order by the COUNT column.
merged <- merged[order(-merged$COUNT), ]
# Replace NAs with "blanks".
merged[is.na(merged)] <- ""
#==============================================================================
# Load the WQP station information
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data/Site_Data")
wqp.station <- read.csv("sub_site_all.csv", stringsAsFactors = FALSE)
#==============================================================================
# Join the site info with the param data.
by.names <- c("OrganizationIdentifier", #"OrganizationFormalName",
              "MonitoringLocationIdentifier") #, "MonitoringLocationName")
final.df <- dplyr::left_join(merged, wqp.station, by = by.names )
# Organize columns.
final.df <- final.df[, c(1:4, 51:57, 5:50)]
#==============================================================================
# Creat a very basic leaflet map for rapid review of NWIS ites and parameters.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/NWIS/NWIS_Sites")
write.csv(final.df, "potential_nwis_sites_1_19_17.csv", row.names = FALSE)

library(leaflet)

final.df <- final.df[order(final.df$parameter_cd), ]
test <- aggregate(parameter_cd ~ LatitudeMeasure + 
                    LongitudeMeasure + MonitoringLocationIdentifier,
                  data = final.df, FUN = c)

leaflet(test) %>% addTiles() %>%
  addCircleMarkers(~LongitudeMeasure, ~LatitudeMeasure, 
    #radius = ~ifelse(type == "ship", 6, 10),
    #color = ~pal(type),
    popup = paste("MonitoringLocationIdentifier:", test$MonitoringLocationIdentifier,
                  "Parameter_cd:", test$parameter_cd, sep = " "),
    stroke = FALSE, fillOpacity = 0.5)

