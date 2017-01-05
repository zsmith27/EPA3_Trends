#==============================================================================
# Author: Zachary M. Smith
# Date: ~ December, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to merge the output from XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#          with Legacy STORET parameter codes and Reporting Units.
#          Additionally, the data was was subset to only include necessary 
#          columns and Sample Media.
# Output: A csv of merged and subset data for each State.
#==============================================================================
#setwd("C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R")
setwd("C:/Users/zsmith/Desktop/ICPRB_PhotoVault/WQ_Trends/R")
#==============================================================================
DE <- read.table("DELAWARE.txt")
DC <- read.table("DISTRICT.txt")
MD <- read.table("MARYLAND.txt")
PA <- read.table("PENNSYLVANIA.txt")
VA <- read.table("VIRGINIA.txt")
WV <- read.table("WEST_VIRGINIA.txt")
#==============================================================================
param <- read.csv("PARMSBAR.csv")
param <- param[, -which(names(param) %in% c("TOTAL_OBSERVATIONS",
                                            "PERCENTS_1972_1977",
                                            "PERCENTS_1967_1971", 
                                            "PERCENTS_PRE_1967"))]

r_units <- read.csv("REPORTING_UNITS.csv")
#==============================================================================
summarize_wq <- function(State, parameter, reporting_units){
  merg1 <- merge(State, parameter, by.x = "Param",
                 by.y = "PARAMETER_NUMBER", all.x = TRUE)
  merg2 <- merge(merg1, reporting_units, by.x = "REPORTING_CODE",
                 by.y = "Reporting_Code", all.x = TRUE )
  
  new.df <- merg2[, c("Station", "Agency", "State.Name", "County.Name",
                      "HUC", "Latitude", "Longitude", "Start.Date", "Start.Time",
                      "End.Date", "End.Time", "Param", "Result.Value",
                      "Reporting_Units", "REPORTING_CODE", "SHORT_NAME",
                      "FULL_NAME", "Sample.Depth", "Replicate.Number",
                      "DECIMAL_POINT_LOCATION", "GROUP_CODE",
                      "SAMPLE_MEDIA", "ANALYSIS_TYPE", "PRIORITY_POLLUTANT",
                      "SUB_GROUP_CODE", "ORIGIN", "ENTRY_DATE", "REVISION_DATE",
                      "TRACKING_CODE", "CHEMICAL_ABSTRACT_NUMBER")]
  
  final.df <- new.df[-which(new.df$SAMPLE_MEDIA == 'T' | 
                              new.df$SAMPLE_MEDIA == 'M' |
                              new.df$SAMPLE_MEDIA == 'L' | 
                              new.df$SAMPLE_MEDIA == 'F'), ]
  return(final.df)
}
#==============================================================================
DE.df <- summarize_wq(DE, param, r_units)
DC.df <- summarize_wq(DC, param, r_units)
MD.df <- summarize_wq(MD, param, r_units)
PA.df <- summarize_wq(PA, param, r_units)
VA.df <- summarize_wq(VA, param, r_units)
WV.df <- summarize_wq(WV, param, r_units)
#==============================================================================
write.csv(DE.df, "DE.txt")
write.csv(DC.df, "DC.txt")
write.csv(MD.df, "MD.txt")
write.csv(PA.df, "PA.txt")
write.csv(VA.df, "VA.txt")
write.csv(WV.df, "WV.txt")
