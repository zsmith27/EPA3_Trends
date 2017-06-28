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


usgs.df <- station.df %>% 
  mutate(SITE = 
           case_when(
             substr(Site_no, 1, 2) %in% c("dc", "de", "md", "pa", "va", "wv") ~ 
               substr(Site_no, 3, nchar(Site_no)),
             TRUE ~ Site_no),
         SITE = 
           case_when(
             grepl("MDDNR|PADEP|SRBC", SITE) ~ gsub("_.*", "", SITE),
             TRUE ~ SITE
               ),
         SITE = gsub(".*_", "", SITE))

usgs.sub <- usgs.df %>% 
  select(SITE, Site_no) %>% 
  rename(USGS_SITE = Site_no) %>% 
  distinct()

final.sub <- final.df %>% 
  select(SITE, AGENCY) %>% 
  distinct()

merged.anti <- anti_join(usgs.sub, final.sub, by = "SITE") 
merged.inner <- inner_join(usgs.sub, final.sub, by = "SITE") 


test <- final.df %>%
  filter(grepl("6CSFH075.61", MonitoringLocationIdentifier))
table(test$CharacteristicName)

library(ggplot2)
test2 <- test %>% 
  filter(CharacteristicName == "Temperature, water") %>% 
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue),
         ActivityStartDate = as.Date(ActivityStartDate))

ggplot(test2, aes(ActivityStartDate, ResultMeasureValue)) +
  geom_point()
