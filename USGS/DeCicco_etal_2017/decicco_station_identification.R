#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 06/26/2017
# Updated: 06/26/2017
# Maintained: Zachary M. Smith
# Data Source: De Cicco, L.A., Sprague, L.A., Murphy, J.C., Riskin,
#              M.L., Falcone, J.A., Stets, E.G., Oelsner, G.P., and 
#              Johnson, H.M., 2017, Water-quality and streamflow datasets 
#              used in the Weighted Regressions on Time, Discharge, and Season
#              (WRTDS) models to determine trends in the Nation’s rivers and 
#              streams, 1972-2012: U.S. Geological Survey data release, 
#              http://dx.doi.org/10.5066/F7KW5D4H.
# Purpose: Identify stations in EPA Region 3 from the output of
#          De Cicco et al. (2017).
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Specify the directory containing station info (i.e. Station Name and State).
info.dir <- "D:/ZSmith/Projects/WQ_Trends/USGS_Data/Output/DeCicco_etal_2017/INFO"
# Create a list of all the file names in the specified directory.
all.files <- list.files(info.dir)
# Each csv file represents a unique parameter and station.
# The file names contain the station name between the first and last underscore.
# The regex below extracts the station name from the file name list. 
station.list <- unique(gsub("(^[^_]+)_?|_?([^_]+$)", "", all.files))
# Remove the all.files list to save space.
rm(all.files)
#------------------------------------------------------------------------------
# Each csv file represents a unique parameter and station, however, the 
# location of the station does not change by parameter. Therefore, only one
# csv per station needs to be imported and the state extracted.
station.info <- lapply(station.list, function(station.x) {
  # Identify all files containing the specified station name.
  sub.list <- list.files(path = info.dir, pattern = station.x)
  # Create the file path to the first file name listed in sub.list.
  sub.file <- file.path(info.dir, sub.list[[1]])
  # Import the specified file.
  sub.df <- read.csv(sub.file, stringsAsFactors = FALSE)
  # Make sure columns "Site_no" and "Gage_Number are class "character."
  # Make sure each row is unique.
  final.df <- sub.df %>%  
    mutate(Site_no = as.character(Site_no),
           Gage_number = as.character(Gage_number)) %>% 
    distinct()
  # Return the dataframe.
  return(final.df)
})
#------------------------------------------------------------------------------
# Join the imported station info and filter out any stations not found in
# EPA Region 3.
station.df <- bind_rows(station.info) %>% 
  filter(staAbbrev %in% c("DC", "DE", "PA", "MD", "VA", "WV"))
# Remove station.info, info.dir, and station.list to save space.
rm(station.info, info.dir, station.list)
#==============================================================================
# Export Groomed Data
#==============================================================================
# Specify the export directory.
output.dir <- "D:/ZSmith/Projects/WQ_Trends/USGS_Data/Output/Groomed"
# Name the export file.
output.path <- file.path(output.dir, "decicco_stations.csv")
# Export file as a csv.
write.csv(station.df, output.path, row.names = FALSE)
# Remove unnecessary objects.
rm(output.dir, output.path)

