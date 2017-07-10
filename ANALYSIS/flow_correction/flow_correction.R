#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 06/29/2017
# Updated: 06/29/2017
# Maintained: Zachary M. Smith
# Purpose: Flow Correction
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Run the script that deals with long term trend sample frequency.
source("Frequency/sample_frequency.R")
#------------------------------------------------------------------------------
# Connect to PostgreSQL database to import flow data.
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT_May", host = "localhost", port = 5432)

#------------------------------------------------------------------------------
# Import flow data.
flow.df <- dbReadTable(con, "gage_flow") %>% 
  mutate(GAGE_ID = as.character(GAGE_ID))
on.exit(dbDisconnect(con), add = TRUE)
#------------------------------------------------------------------------------
# Join the flow data with the long term data.
long.term <- dplyr::left_join(long.term, flow.df, by = c("GAGE_ID", "DATE")) 
#------------------------------------------------------------------------------
# Calculate the residuals of the log parameter value and log flow value.
flow.correct <- ungroup(long.term) %>% 
  filter(!is.na(FLOW),
         FLOW > 0,
         ICPRB_VALUE > 0) %>% 
  mutate(LOG_VALUE = log10(ICPRB_VALUE),
         LOG_FLOW = log10(FLOW),
         RESIDUAL = loess(LOG_VALUE ~ LOG_FLOW, .)$residuals) %>% 
  select(SITE, AGENCY, DATE, ICPRB_NAME, ICPRB_UNITS, GAGE_ID, FLOW,
         LOG_VALUE, LOG_FLOW, RESIDUAL)
#------------------------------------------------------------------------------
# Join the flow corrected data with the primary data.
long.term <- dplyr::left_join(long.term, flow.correct,
                              by = c("SITE", "AGENCY", "DATE", "ICPRB_NAME",
                                     "ICPRB_UNITS", "GAGE_ID", "FLOW")) 



