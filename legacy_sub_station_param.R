#==============================================================================
# Author: Zachary M. Smith
# Date: 1-4-17
# Maintained: Zachary M. Smith
# Purpose: The script was written to subset the raw Legacy STORET data to 
#          only include the Stations and Parameters selected by Claire
#          Buchanan. The output file are to be added to the MS Access Database.
# Output: Creates individual ".txt" files for each EPA Region 3 State.
#==============================================================================
library(data.table)
library(lubridate)
#==============================================================================
# Load raw Legacy STORET data.
setwd("//pike/data/Projects/EPA3Trends/r_scripts/R_Data/Legacy STORET/Aggregate Raw Data/Legacy Res")
de <- read.table("DELAWARE.txt")
dc <- read.table("DISTRICT.txt")
md <- read.table("MARYLAND.txt")
pa <- read.table("PENNSYLVANIA.txt")
va <- read.table("VIRGINIA.txt")
wv <- read.table("WEST_VIRGINIA.txt")
#==============================================================================
# Load stations and parameters specified by Buchanan.
setwd("//pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy")
stations.df <- read.table("LegacyStationsAll_050316Claire.txt", header = TRUE)
param.df <- read.delim("LegacyParam_byAgency_122915.txt")
#==============================================================================
# A function for cleaning and subsetting raw Legacy STORET data.
# Changes made to make it easier for Nagel to add files to MS Access:
# 1. date format = mm/d/yyyy
# 2. time format = hh:mm
# 3. NAs = "blank"
# Data subset to only include stations and parameters specified by Buchanan.
sub_data <- function(state, stations, params){
  names(state) <- toupper(names(state))
  state.tbl <- data.table::data.table(state)
  state.tbl$AGENCY <- trimws(state.tbl$AGENCY)
  state.tbl$STATION <- trimws(state.tbl$STATION)
  state.tbl$PARAM <- trimws(state.tbl$PARAM)
  
  state.tbl <- state.tbl[AGENCY %in% unique(trimws(stations$Agency)) &
                           STATION %in% unique(trimws(stations$Station)) &
                           PARAM %in% unique(trimws(params$PARAM))]

  
  # Re-format date
  # This helps Andrea add the files to MS Access.
  state.tbl$START.DATE <- ymd(state.tbl$START.DATE)
  state.tbl$START.DATE <- format(state.tbl$START.DATE, "%m/%d/%Y")
  state.tbl$END.DATE <- ymd(state.tbl$END.DATE)
  state.tbl$END.DATE <- format(state.tbl$END.DATE, "%m/%d/%Y")
  
  # Re-format time.
  # This helps Andrea add the files to MS Access.
  convert_time <- function(time.col){
    time.col <- ifelse(nchar(as.character(time.col)) == "3", 
                                paste("0", time.col, sep = ""),
                       ifelse(nchar(as.character(time.col)) == "3", time.col,
                              ""))
    time.col <- as.POSIXct(time.col, format = "%H%M")
    time.col <- format(time.col, "%H:%M")
    return(time.col)
  }
  
  state.tbl$START.TIME <- convert_time(state.tbl$START.TIME)
  state.tbl$END.TIME <- convert_time(state.tbl$END.TIME)
  
  # Replace all NAs with blank.
  # This helps Andrea add the files to MS Access.
  state.tbl[is.na(state.tbl)] <- ""
  
  return(state.tbl)
}
#==============================================================================
# Apply the sub_data function
de.sub <- sub_data(de, stations.df, param.df)
dc.sub <- sub_data(dc, stations.df, param.df)
md.sub <- sub_data(md, stations.df, param.df)
pa.sub <- sub_data(pa, stations.df, param.df)
va.sub <- sub_data(va, stations.df, param.df)
wv.sub <- sub_data(wv, stations.df, param.df)
# Combine all of the sub_data dataframes into one dataframe.
all.sub <- rbind(de.sub, dc.sub, md.sub, pa.sub, va.sub, wv.sub)
#==============================================================================
# Output the sub_data tables.
setwd("//pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Subset Stations and Params")
write.table(de.sub, "de_sub.txt", row.names = FALSE)
write.table(dc.sub, "dc_sub.txt", row.names = FALSE)
write.table(md.sub, "md_sub.txt", row.names = FALSE)
write.table(pa.sub, "pa_sub.txt", row.names = FALSE)
write.table(va.sub, "va_sub.txt", row.names = FALSE)
write.table(wv.sub, "wv_sub.txt", row.names = FALSE)
write.table(all.sub, "all_sub.txt", row.names = FALSE)
#==============================================================================
# Function for checking that all stations specified by Buchanan were correctly
# included in the subset.
check_me <- function(state.df, state.txt, stations){
  state.df <- data.frame(unique(state.df[, c("AGENCY", "STATION")]))
  state.df <- data.frame(lapply(state.df, trimws), stringsAsFactors = FALSE)
  stations <- unique(stations[stations$State %in% state.txt, ])
  stations <- data.frame(lapply(stations, trimws), stringsAsFactors = FALSE)
  final.df <- stations[!stations$Agency %in% state.df$AGENCY |
                         !stations$Station %in% state.df$STATION, ]

  return(final.df)
  
}
#==============================================================================
# Apply the check_me function.
test.de <- check_me(de.sub, "DE", stations.df)
test.dc <- check_me(dc.sub, "DC", stations.df)
test.md <- check_me(md.sub, "MD", stations.df)
test.pa <- check_me(pa.sub, "PA", stations.df)
test.va <- check_me(va.sub, "VA", stations.df)
test.wv <- check_me(wv.sub, "WV", stations.df)
#==============================================================================
#state.df <- pa.sub
#state.txt <- "PA"
#stations <- stations.df

# Function for checking that no extra stations were included in the subset.
check_me2 <- function(state.df, state.txt, stations){
  state.df <- data.frame(unique(state.df[, c("AGENCY", "STATION")]))
  state.df <- data.frame(lapply(state.df, trimws), stringsAsFactors = FALSE)
  stations <- unique(stations[stations$State %in% state.txt, ])
  stations <- data.frame(lapply(stations, trimws), stringsAsFactors = FALSE)
  final.df <- state.df[!state.df$AGENCY %in% stations$Agency |
                         !state.df$STATION %in% stations$Station, ]
  
  return(final.df)
  
}
#==============================================================================
test.de <- check_me2(de.sub, "DE", stations.df)
test.dc <- check_me2(dc.sub, "DC", stations.df)
test.md <- check_me2(md.sub, "MD", stations.df)
test.pa <- check_me2(pa.sub, "PA", stations.df)
test.va <- check_me2(va.sub, "VA", stations.df)
test.wv <- check_me2(wv.sub, "WV", stations.df)
