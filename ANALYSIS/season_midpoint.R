#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 5/01/2017
# Updated: 5/01/2017
# Maintained: Zachary M. Smith
# Purpose: Identifies the midpoint of each season for each year.
# Output: The water quality data is merged with the newly identified seasonal
# midpoint.
#==============================================================================
#==============================================================================
season_midpoint <- function(wq.df) {
  # Generate all dates from 1972 to the end of 2016.
  date.df <- data.frame(DATE = seq(lubridate::ymd('1971-12-01'),
                                   lubridate::ymd('2017-02-28'), by = 'day'))
  # Identify the year of the row.
  date.df$YEAR <- lubridate::year(date.df$DATE)
  # Identify the month of the row.
  date.df$MONTH <- lubridate::month(date.df$DATE)
  # December needs to be culled with the following year.
  # When the row month is December add one year to the year column.
  date.df$YEAR <- ifelse(date.df$MONTH %in% 12, lubridate::year(date.df$DATE) + 1, date.df$YEAR)
  # Identify the julian day of the row.
  date.df$JULIAN <- lubridate::yday(date.df$DATE)
  # Identify the season of the row.
  date.df$SEASON <- ifelse(date.df$MONTH %in% c(3, 4, 5), "SPRING",
                           ifelse(date.df$MONTH %in% c(6, 7, 8), "SUMMER",
                                  ifelse(date.df$MONTH %in% c(9, 10, 11), "FALL",
                                         ifelse(date.df$MONTH %in% c(12, 1, 2), "WINTER", "ERROR"))))
  #----------------------------------------------------------------------------
  # Identify the seasonal midpoint by year.
  midpoint.list <- lapply(unique(date.df$YEAR), function(year.x) {
    # Subset by year.
    year.df <- date.df[date.df$YEAR %in% year.x, ]
    season.list <- lapply(unique(year.df$SEASON), function(season.x) {
      # Subset by season.
      season.df <- year.df[year.df$SEASON %in% season.x, ]
      # Identify the row that represents the midpoint.
      mid.day <- nrow(season.df) / 2
      # Create a new dataframe and identify the current season.
      final.season <- data.frame(SEASON = season.x)
      # Identify the current year.
      final.season$YEAR <- year.x
      # Identify the midpoint date.
      final.season$MIDPOINT <- season.df$DATE[mid.day]
      return(final.season)
    })
    # Unlist the seasonal date per year
    final.df <- do.call(rbind, season.list)
  })
  # Unlist the yearly data.
  midpoint.df <- do.call(rbind, midpoint.list)
  #----------------------------------------------------------------------------
  # Merge the midpoint results with the water quality data.
  final.df <- merge(wq.df, midpoint.df, by = c("YEAR", "SEASON"))
  # End season_midpoint function.
  return(final.df)
}




