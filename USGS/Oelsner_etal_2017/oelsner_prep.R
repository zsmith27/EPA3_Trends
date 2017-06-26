#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 06/26/2017
# Updated: 06/26/2017
# Maintained: Zachary M. Smith
# Data Source: Oelsner, G.P., Sprague, L.A., Murphy, J.C., Zuellig, R.E., 
#              Johnson, H.M., Ryberg, K.R., Falcone, J.A., Stets, E.G., 
#              Vecchia, A.V., Riskin, M.L., De Cicco, L.A., Mills, T.J., 
#              Farmer, W.H., 2017, Water-quality trends in the Nation’s 
#              rivers and streams 1972–2012—Data preparation, statistical 
#              methods, and trend results: U.S. Geological Survey Scientific 
#              Investigations Report, http://dx.doi.org/10.3133/sir20175006.
# Purpose: Organize data from the output of Oelsner et al. (2017).
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Run station identification script to identify all stations in EPA Region 3.
source("USGS/DeCicco_etal_2017/decicco_station_identification.R")
#==============================================================================
# Import Trends Table
#==============================================================================
# Specify the file directory.
main.dir <- "D:/ZSmith/Projects/WQ_Trends/USGS_Data/Output/Oelsner_etal_2017"
# Specify the file name to establish the file path.
file.name <- file.path(main.dir, "TrendsResultsOutAll.txt")
# Import the text file as a csv. Had to specify row.names as NULL because
# the rows contain differning number of columns.
trends.df <- read.csv(file.name, stringsAsFactors = FALSE, row.names = NULL)
#==============================================================================
# Correct Trends Table
#==============================================================================
# When specifying row.names as NULL, all of the columns are shifted right and
# the first column name is "row.names".
# Remove the "row.names" column and shift the column names to the left, 
# which is the correct position. The final column is an extra column, and
# therefore, is named as such.
names(trends.df) <- c(names(trends.df)[2:ncol(trends.df)], "Extra")
#------------------------------------------------------------------------------
# There are multiple rows where many of the values have been shifted right,
# leading to an additional column. This appears to be due to some "parameter"
# column rows containing commas and the data being exported as comma delimited.
# Therefore, when the text file is read as a csv, the "parameter" column is
# incorrectly divided into two columns.
# This script fixes the reading error.
# 1.) Filter only the rows where the column "trendType" is incorrectly 
#     specified as "filtered" or "unfiltered".
# 2.) Add the "filtered" or "unfiltered" string back into the "parameter"
#     column string but this time seperate with semicolon. Additionally,
#     make sure the "dec_coord_datum" is class "numeric". This is a bit 
#     counterintuitive, but at this point the "dec_coord_datum" actually
#     represents "dec_long_va", which needs to be class "numeric".
# 3.) Remove the "trendType" column becuase the values are incorret.
trends.bad <- trends.df %>% 
  filter(trendType %in% c(" filtered", " unfiltered")) %>% 
  mutate(parameter = paste0(parameter, ";", trendType),
         dec_coord_datum_cd = as.numeric(dec_coord_datum_cd)) %>% 
  select(-trendType)
# Rename the columns according to the orginal text file. Make sure to exclude
# the placeholder column, "Extra".
names(trends.bad) <- names(trends.df)[!names(trends.df) %in% "Extra"]
#------------------------------------------------------------------------------
# Join the corrected rows with the rows containing no errors.
# 1) Remove any rows where the "trendType" column contains " filtered" or 
#    " unfiltered".
# 2) Make sure columns "NumYears" and "trendPerc" are class "numeric".
# 3) Remove the "Extra" column.
# 4) Bind the filtered dataframe with the corrected data.
trends.df <- trends.df %>% 
  filter(!trendType %in% c(" filtered", " unfiltered")) %>% 
  mutate(NumYears = as.numeric(NumYears),
         trendPerc = as.numeric(trendPerc)) %>% 
  select(-Extra) %>% 
  bind_rows(trends.bad)
#------------------------------------------------------------------------------
# Remove unnecessary objects.
rm(trends.bad, file.name, main.dir)
#------------------------------------------------------------------------------
trends.df <- trends.df %>% 
  filter(Site_no %in% station.df$Site_no)

