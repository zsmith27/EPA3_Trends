#==============================================================================
# Author: Zachary M. Smith
# Date: ~ October, 2015
# Maintained: Zachary M. Smith
# Purpose: The script was written to merge the output from "legacy_inv_ross.R"
#          script with the Legacy STORET parameter codes.  Additionally,
#          the script eliminates uncessary Sample Media.
# Output: Creates a parameter ".csv" for each state and a single ".csv"
#         containing parameter data for all states.
#==============================================================================
setwd("C:\\Users\\Zsmith\\Desktop\\WQ_Trends\\R")

DE <- read.table("DE_Params.txt")
DC <- read.table("DC_Params.txt")
MD <- read.table("MD_Params.txt")
PA <- read.table("PA_Params.txt")
VA <- read.table("VA_Params.txt")
WV <- read.table("WV_Params.txt")

param <- read.csv("PARMSBAR.csv")
station_list <- read.table("Station_List.txt")

sample_media <- function(State, Param){
  
  State_merg <- merge(State, Param, by.x = "Code", by.y = "PARAMETER_NUMBER", all.x = TRUE)
  final.df <- State_merg[-which(State_merg$SAMPLE_MEDIA == 'T' | 
                                  State_merg$SAMPLE_MEDIA == 'M' |
                                  State_merg$SAMPLE_MEDIA == 'L' | 
                                  State_merg$SAMPLE_MEDIA == 'S' |
                                  State_merg$SAMPLE_MEDIA == 'A' |
                                  State_merg$SAMPLE_MEDIA == 'F'), ]
  return(final.df)
}

DE.df <- sample_media(DE, param)
DC.df <- sample_media(DC, param)
MD.df <- sample_media(MD, param)
PA.df <- sample_media(PA, param)
VA.df <- sample_media(VA, param)
WV.df <- sample_media(WV, param)



new.df <- rbind(DE.df, DC.df, MD.df, PA.df, VA.df, WV.df)
test <- merge(station_list, new.df, by = "Station")

write.csv(DE.df, "DE_Params.csv")
write.csv(DC.df, "DC_Params.csv")
write.csv(MD.df, "MD_Params.csv")
write.csv(PA.df, "PA_Params.csv")
write.csv(VA.df, "VA_Params.csv")
write.csv(WV.df, "WV_Params.csv")
write.csv(new.df, "All_States_Params.csv")



