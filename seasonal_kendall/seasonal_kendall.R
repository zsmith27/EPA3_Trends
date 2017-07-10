#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 06/28/2017
# Updated: 06/29/2017
# Maintained: Zachary M. Smith
# Purpose: Seasonal Kendall Test
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
# Load EnvStats to calculate Seasonal Kendall Test
library(EnvStats)
#------------------------------------------------------------------------------
# Run the script that deals with long term trend sample frequency.
source("ANALYSIS/flow_correction/flow_correction.R")
#------------------------------------------------------------------------------
# Identify all of the unique sampling sites in the long.term dataframe.
sites.vec <- unique(long.term$SITE)
site.i <- ("WQN0820")
param.i <- "AL_TOT"
#------------------------------------------------------------------------------
# Loop through and perform the analysis on the primary data and then the 
# flow corrected data.
flow.list <- lapply(c(FALSE, TRUE), function(flow.i) {
  if (flow.i == TRUE) {
    print("---Flow Corrected---")
    long.df <- long.term %>% 
      mutate(ICPRB_VALUE = RESIDUAL)
  } else {
    print("---Primary Data---")
    long.df <- long.term
  }
  #----------------------------------------------------------------------------
  # Loop through each unique site, subsetting the dataframe each time to only 
  # represent site.i.
  site.list <- lapply(sites.vec, function(site.i) {
    print(site.i)
    # Subset the dataframe to only represent site.i.
    long <- long.df %>% 
      filter(SITE == site.i)
    #--------------------------------------------------------------------------
    # Identify the unique parameters sampled at site.i.
    params.vec <- unique(long$ICPRB_NAME)
    #--------------------------------------------------------------------------
    params.list <- lapply(params.vec, function(param.i) {
      print(paste0("...", param.i))
      # Subset the dataframe to only represent site.i.
      sub.long <- long %>% 
        filter(ICPRB_NAME %in% param.i)
      #------------------------------------------------------------------------
      # Calculate the percentage of samples that are censored.
      pct.censored <- sub.long %>% 
        group_by(SITE) %>% 
        summarise(PCT = case_when(
          "Censored" %in% CENSORED ~ 
            nrow(.[.$CENSORED %in% "Censored", ]) / nrow(.) * 100,
          TRUE ~ as.double(0)
        )) %>% 
        pull(PCT)
      #------------------------------------------------------------------------
      sum.long <- summary(sub.long$ICPRB_VALUE)
      #------------------------------------------------------------------------
      # Use the EnvStats function "kendallSeasonalTrendTest" to calculate 
      # seasonal Kendall test. The "try" function continues the loop if an
      # error has occured. There are many Sites/Parameters that are only 
      # represented by one season, and thus, are not applicable fo the seasonal
      # Kendall test. These instances are later included in the output dataframe
      # but the rows are filled with NA.
      kendall <- try(EnvStats::kendallSeasonalTrendTest(sub.long$ICPRB_VALUE,
                                                        season = sub.long$SEASON,
                                                        year = sub.long$YEAR,
                                                        na.action = "omit"))
      #------------------------------------------------------------------------
      # Create a dataframe to store the data.
      base.df <- data.frame(stat_test = "seasonal_kendall",
                            flow_corrected = flow.i,
                            site = site.i, param = param.i, 
                            pct_censored = pct.censored,
                            stringsAsFactors = FALSE) 
      #------------------------------------------------------------------------
      # If statement to deal with potential errors from the seasonal Kendall test.
      # If an error occured, then the dataframe is created with site.i and 
      # param.i specified but the remaining columns are filled with NAs. If no
      # error occured, then the list output from the kendallSeasonalTrendTest
      # function is organized into a dataframe.
      if (class(kendall) == "try-error") {
        # Count the number of samples by season.
        season.count <- sub.long %>% 
          group_by(SEASON) %>% 
          summarise(COUNT = n())
        #----------------------------------------------------------------------
        kendall.df <- base.df %>% 
          mutate(tau = as.numeric(NA),
                 slope = as.numeric(NA),
                 intercept = as.numeric(NA),
                 fall_n = case_when(
                   "fall" %in% season.count$SEASON ~ 
                     as.integer(season.count[season.count$SEASON %in% "fall", "COUNT"]),
                   TRUE ~ as.integer(0)
                 ),
                 spring_n = case_when(
                   "spring" %in% season.count$SEASON ~ 
                     as.integer(season.count[season.count$SEASON %in% "spring", "COUNT"]),
                   TRUE ~ as.integer(0)
                 ),
                 summer_n = case_when(
                   "summer" %in% season.count$SEASON ~ 
                     as.integer(season.count[season.count$SEASON %in% "summer", "COUNT"]),
                   TRUE ~ as.integer(0)
                 ),
                 winter_n = case_when(
                   "winter" %in% season.count$SEASON ~
                     as.integer(season.count[season.count$SEASON %in% "winter", "COUNT"]),
                   TRUE ~ as.integer(0)
                 ),
                 df = as.integer(0),
                 test_stat_chi = as.numeric(NA),
                 test_stat_z = as.numeric(NA),
                 p_value_chi = as.numeric(NA),
                 p_value_z = as.numeric(NA),
                 slope_lcl = as.numeric(NA),
                 slope_ucl = as.numeric(NA),
                 min = sum.long[1],
                 median = sum.long[3],
                 max = sum.long[6])
        
      } else {
        kendall.df <- base.df %>% 
          mutate(tau = kendall$estimate[1],
                 slope = kendall$estimate[2],
                 intercept = kendall$estimate[3],
                 fall_n = case_when(
                   "fall" %in% names(kendall$sample.size) ~ kendall$sample.size["fall"],
                   TRUE ~ as.integer(0)
                 ),
                 spring_n = case_when(
                   "spring" %in% names(kendall$sample.size) ~ kendall$sample.size["spring"],
                   TRUE ~ as.integer(0)
                 ),
                 summer_n = case_when(
                   "summer" %in% names(kendall$sample.size) ~ kendall$sample.size["summer"],
                   TRUE ~ as.integer(0)
                 ),
                 winter_n = case_when(
                   "winter" %in% names(kendall$sample.size) ~ kendall$sample.size["winter"],
                   TRUE ~ as.integer(0)
                 ),
                 df = kendall$parameters[1],
                 test_stat_chi = kendall$statistic[1],
                 test_stat_z = kendall$statistic[2],
                 p_value_chi = round(kendall$p.value[1], 3),
                 p_value_z = round(kendall$p.value[2], 3),
                 slope_lcl = kendall$interval$limits[1],
                 slope_ucl = kendall$interval$limits[2],
                 min = sum.long[1],
                 median = sum.long[3],
                 max = sum.long[6])
      }
      return(kendall.df)
    }) # End lapply[param.i]
    #----------------------------------------------------------------------------
    # Append the list of dataframes from the param.i output into a single 
    # dataframe.
    params.df <- bind_rows(params.list)
    return(params.df)
  }) # End lapply[site.i]
  #------------------------------------------------------------------------------
  # Append the list of dataframes from the site.i output into a single 
  # dataframe.
  sites.df <- bind_rows(site.list)
})
#------------------------------------------------------------------------------
# Append the list of dataframes from the site.i output into a single 
# dataframe.
kendall.final <- bind_rows(flow.list)
#==============================================================================
# Export Data
#==============================================================================
# Specify output directory.
output.dir <- "D:/ZSmith/Projects/WQ_Trends/EPA3_Trends/ANALYSIS/data"
# Specify the file name.
output.path <- file.path(output.dir, "seasonal_kendall_results.csv")
# Write csv to specified directory.
write.csv(kendall.final, output.path, row.names = FALSE)
# Remove unnecessary objects.
rm(main.dir, output.dir, output.path)


test <- kendall.final %>% 
  filter(site == "WQN0903", param == "AL_TOT")
test$pct_censored
