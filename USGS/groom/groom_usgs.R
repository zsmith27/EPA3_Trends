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
# Purpose: Groom USGS National Water Quality Trend data.
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Run station identification script to identify all stations in EPA Region 3.
source("USGS/DeCicco_etal_2017/decicco_station_identification.R")
#------------------------------------------------------------------------------
# Run decicco_results script to clean up results table and boostrap table.
source("USGS/DeCicco_etal_2017/decicco_results.R")
#------------------------------------------------------------------------------
# Run mills_prep script to clean up season kendall trend results.
source("USGS/Mills_etal_2017/mills_prep.R")
#------------------------------------------------------------------------------


