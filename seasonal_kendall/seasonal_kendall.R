#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 06/28/2017
# Updated: 06/28/2017
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
source("Frequency/sampl_frequency.R")
#------------------------------------------------------------------------------
# Identify all of the unique sampling sites in the long.term dataframe.
sites.vec <- unique(long.term$SITE)
#------------------------------------------------------------------------------
# Loop through each unique site, subsetting the dataframe each time to only 
# represent site.i.
site.list <- lapply(sites.vec, function(site.i) {
  print(site.i)
  # Subset the dataframe to only represent site.i.
  long <- long.term %>% 
    filter(SITE == site.i)
  # Identify the unique parameters sampled at site.i.
  params.vec <- unique(long$ICPRB_NAME)
  #----------------------------------------------------------------------------
  params.list <- lapply(params.vec, function(param.i) {
    print(paste0("...", param.i))
    # Subset the dataframe to only represent site.i.
    sub.long <- long %>% 
      filter(ICPRB_NAME %in% param.i)
    #--------------------------------------------------------------------------
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
    #--------------------------------------------------------------------------
    # If statement to deal with potential errors from the seasonal Kendall test.
    # If an error occured, then the dataframe is created with site.i and 
    # param.i specified but the remaining columns are filled with NAs. If no
    # error occured, then the list output from the kendallSeasonalTrendTest
    # function is organized into a dataframe.
    if (class(kendall) == "try-error") {
      kendall.df <- data.frame(site = site.i, param = param.i,
                               stringsAsFactors = FALSE) %>% 
        mutate(tau = as.numeric(NA),
               slope = as.numeric(NA),
               intercept = as.numeric(NA),
               fall_n = as.numeric(NA),
               spring_n = as.numeric(NA),
               summer_n = as.numeric(NA),
               winter_n = as.numeric(NA),
               df = as.numeric(NA),
               test_stat_chi = as.numeric(NA),
               test_stat_z = as.numeric(NA),
               p_value_chi = as.numeric(NA),
               p_value_z = as.numeric(NA),
               slope_lcl = as.numeric(NA),
               slope_ucl = as.numeric(NA))
    } else {
      kendall.df <- data.frame(site = site.i, param = param.i,
                               stringsAsFactors = FALSE) %>% 
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
               p_value_chi = kendall$p.value[1],
               p_value_z = kendall$p.value[2],
               slope_lcl = kendall$interval$limits[1],
               slope_ucl = kendall$interval$limits[2])
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
kendall.final <- bind_rows(site.list)
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


