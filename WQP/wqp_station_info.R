#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/10/17
# Updated: 1/11/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to import all of the WQP site/station
#          information. Only the necessary columns were kept.
# Output: Creates individual ".csv" files for each EPA Region 3 State and a
#         single ".csv" including all site/station information.
#==============================================================================
#==============================================================================
# Set working directory.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Raw Data/Site_Data")
#==============================================================================
# Import the data for each state. 
# The data was previously subset to only include potential params of interest.
dc.df <- read.csv("wqp_site_dc.csv", stringsAsFactors = FALSE)
de.df <- read.csv("wqp_site_de.csv", stringsAsFactors = FALSE)
md.df <- read.csv("wqp_site_md.csv", stringsAsFactors = FALSE)
pa.df <- read.csv("wqp_site_pa.csv", stringsAsFactors = FALSE)
va.df <- read.csv("wqp_site_va.csv", stringsAsFactors = FALSE)
wv.df <- read.csv("wqp_site_wv.csv", stringsAsFactors = FALSE)
#==============================================================================
# This function reduces the site information down to only the necessary columns.
prep_site <- function(org.df){
  org.df <- org.df[org.df$MonitoringLocationTypeName %in% c("Stream", "River/Stream"),]
  keep.cols <- c("OrganizationIdentifier", "OrganizationFormalName",
                 "MonitoringLocationIdentifier", "MonitoringLocationName",
                 "HUCEightDigitCode", "LatitudeMeasure", "LongitudeMeasure",
                 "HorizontalCoordinateReferenceSystemDatumName",
                 "VerticalCoordinateReferenceSystemDatumName")
  final.df <- unique(org.df[, keep.cols])
  return(final.df)
}
#==============================================================================
# Apply the "prep_site" function.
dc <- prep_site(dc.df)
de <- prep_site(de.df)
md <- prep_site(md.df)
pa <- prep_site(pa.df)
va <- prep_site(va.df)
wv <- prep_site(wv.df)
all_sites <- rbind(dc, de, md, pa, va, wv)
#==============================================================================
# Export the smaller tables.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data/Site_Data")
write.csv(dc, "sub_site_dc.csv", row.names = FALSE)
write.csv(de, "sub_site_de.csv", row.names = FALSE)
write.csv(md, "sub_site_md.csv", row.names = FALSE)
write.csv(pa, "sub_site_pa.csv", row.names = FALSE)
write.csv(va, "sub_site_va.csv", row.names = FALSE)
write.csv(wv, "sub_site_wv.csv", row.names = FALSE)
write.csv(all_sites, "sub_site_all.csv", row.names = FALSE)


