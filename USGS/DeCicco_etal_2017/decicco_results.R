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
# Purpose: Organize data from the output of De Cicco et al. (2017).
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Run station identification script to identify all stations in EPA Region 3.
if (!exists("station.df")) {
  source("USGS/DeCicco_etal_2017/decicco_station_identification.R")
}
#==============================================================================
# Import Results Table
#==============================================================================
# Import results from De Cicco et al. (2017).
main.dir <- "D:/ZSmith/Projects/WQ_Trends/USGS_Data/Output/DeCicco_etal_2017"
results.file <- "tableResults.csv"
file.dir <- file.path(main.dir, results.file)
results.df <- read.csv(file.dir, stringsAsFactors = FALSE)
# Remove unnecessary objects.
rm(file.dir, results.file)
#------------------------------------------------------------------------------
# Keep only the stations found in EPA Region 3 by using the output from the
# "decicco_station_identification.R" script.
results.df <- results.df %>% 
  filter(Site_no %in% unique(station.df$Site_no))
#==============================================================================
# Import Bootstrap Output
#==============================================================================
# Import boostrap output from De Cicco et al. (2017).
boot.file <- "bootOut.csv"
file.dir <- file.path(main.dir, boot.file)
boot.df <- read.csv(file.dir, stringsAsFactors = FALSE)
# Remove unnecessary objects.
rm(file.dir, boot.file)
#------------------------------------------------------------------------------
# Keep only the stations found in EPA Region 3 by using the output from the
# "decicco_station_identification.R" script.
boot.df <- boot.df %>% 
  filter(Site_no %in% unique(station.df$Site_no))
#==============================================================================
# Export Organized Data
#==============================================================================
# Specify output directory.
output.dir <- "D:/ZSmith/Projects/WQ_Trends/USGS_Data/Output/Groomed"
# Specify the file name.
output.path <- file.path(output.dir, "decicco_results.csv")
# Write csv to specified directory.
write.csv(results.df, output.path, row.names = FALSE)
# Specify the file name.
output.path <- file.path(output.dir, "decicco_bootstrap.csv")
# Write csv to specified directory.
write.csv(boot.df, output.path, row.names = FALSE)
# Remove unnecessary objects.
rm(main.dir, output.dir, output.path)
