

#==============================================================================
param_range <- function(site){
  #============================================================================
  # Prepare Date
  #============================================================================
  # Identify column "DATE" as class date
  site$DATE <- as.Date(site$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  site$YEAR <- format(site$DATE, "%Y")
  # Create a new column by extracting the month from date
  site$MONTH <- format(site$DATE, "%m")
  #============================================================================
  # Sequence Dates
  #============================================================================
  site$REPORTED_VALUE <- as.numeric(as.character(site$REPORTED_VALUE))
  site <- site[,c("SITE", "MONTH", "YEAR",# "PARAMETER",
                  "ICPRB_NAME",
                  "REPORTED_VALUE", "ICPRB_UNITS")]
  site <- site[site$YEAR >= 1972, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(site)
  long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR,# PARAMETER,
                               ICPRB_NAME, ICPRB_UNITS)]
  
  
  #============================================================================
  print("Calculating Paramter Min/Max...")
  #max.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, max)
  max.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, max)
  #function(x) quantile(x, 0.95))
  #names(max.df) <- c("PARAMETER", "MAX")
  names(max.df) <- c("ICPRB_NAME", "MAX")
  #min.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, min)
  min.df <- aggregate(REPORTED_VALUE ~ ICPRB_NAME, data = long.df, min)
  #function(x) quantile(x, 0.05))
  #names(min.df) <- c("PARAMETER", "MIN")
  names(min.df) <- c("ICPRB_NAME", "MIN")
  #final.df <- merge(min.df, max.df, by = "PARAMETER")
  final.df <- merge(min.df, max.df, by = "ICPRB_NAME")
  #============================================================================
  
  return(final.df)
}
#==============================================================================