#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 06/27/2017
# Updated: 06/27/2017
# Maintained: Zachary M. Smith
# Purpose: Identify frequency of sampling points
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Run the script that joins the legacy data with the wqp data.
source("MERGED/merge_legacy_wqp_dplyr.R")
#------------------------------------------------------------------------------
# Specify seasons based on month.
# Subset the data to only include columns necessary for measuring sampling
# frequency.
# If more than one sample was collected per day, find the mean value for that
# day.
# Specify the julian day, month, and year based on the "DATE" column.
new.df <- final.df %>% 
  mutate(JULIAN = lubridate::yday(DATE),
         MONTH = lubridate::month(DATE),
         YEAR = lubridate::year(DATE),
         SEASON = case_when(
           MONTH %in% c(12, 1, 2) ~ "winter",
           MONTH %in% c(3, 4, 5) ~ "spring",
           MONTH %in% c(6 ,7, 8) ~ "summer",
           MONTH %in% c(9, 10, 11) ~ "fall",
           TRUE ~ "ERROR"),
         ICPRB_VALUE = case_when(
           CENSORED == "Censored" ~ ICPRB_VALUE / 2,
           TRUE ~ ICPRB_VALUE
         )) %>% 
  select(SITE, AGENCY, SEASON, DATE, YEAR, MONTH, JULIAN, ICPRB_NAME,
         ICPRB_VALUE, ICPRB_UNITS, GAGE_ID, CENSORED) %>% 
  group_by(SITE, AGENCY, SEASON, DATE, YEAR, MONTH, JULIAN,
           ICPRB_NAME, ICPRB_UNITS, GAGE_ID) %>% 
  summarise(ICPRB_VALUE = mean(ICPRB_VALUE))
#------------------------------------------------------------------------------
# Identify any site and parameter where the values collected during
# a particular day were censored.
censored.rows<- final.df %>% 
  select(SITE, AGENCY, DATE, ICPRB_NAME, CENSORED) %>% 
  filter(CENSORED == "Censored") %>% 
  distinct()
#------------------------------------------------------------------------------
# Extract any rows that contain censored data and label them as censored.
censored.df <- semi_join(new.df, censored.rows, 
                         by = c("SITE", "AGENCY", "DATE", "ICPRB_NAME")) %>% 
  mutate(CENSORED = "Censored")
# Extract any rows that does not contain censored data and label them as 
# uncensored.
uncensored.df <- anti_join(new.df, censored.rows, 
                         by = c("SITE", "AGENCY", "DATE", "ICPRB_NAME")) %>% 
  mutate(CENSORED = "Uncensored")
#------------------------------------------------------------------------------
# Append the censored and uncensored data back together.
clean.df <- bind_rows(uncensored.df, censored.df) %>% 
  arrange(AGENCY, SITE, ICPRB_NAME, DATE)
#rm(new.df, censored.rows, uncensored.df)
#------------------------------------------------------------------------------
mid.julian <- data.frame(DATE = seq(as.Date("1972/1/1"),
                                 as.Date("2016/1/1"), by = "day")) %>% 
  mutate(YEAR = lubridate::year(DATE),
         MONTH = lubridate::month(DATE),
         JULIAN = lubridate::yday(DATE)) %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(MID = round(median(JULIAN)))
#------------------------------------------------------------------------------
mid.df <- left_join(clean.df, mid.julian, by = c("YEAR", "MONTH")) %>% 
  mutate(DIFF = abs(MID - JULIAN)) %>% 
  group_by(SITE, AGENCY, SEASON, YEAR,  MONTH, ICPRB_NAME,
           ICPRB_UNITS, GAGE_ID) %>%
  filter(DIFF == min(DIFF))
#------------------------------------------------------------------------------
# Seed randomly generated with sample(1:100, 1). Returned 24.
set.seed(24)
# For months that contain two samples collected an equal duration apart from
# the julian day mid-point, randomly select one sample to represent the month.
dups.rm <- mid.df %>% 
  group_by(SITE, AGENCY, SEASON, YEAR, MONTH, ICPRB_NAME, ICPRB_UNITS,
           GAGE_ID) %>% 
  filter(n() > 1) %>% 
  sample_n(1)
#------------------------------------------------------------------------------
# Join the data back togther with the duplicates removed.
bind.df <- anti_join(mid.df, dups.rm,
                     by = c("SITE", "AGENCY", "SEASON","YEAR", "MONTH",
                            "ICPRB_NAME", "ICPRB_UNITS", "GAGE_ID")) %>% 
  bind_rows(dups.rm)
#------------------------------------------------------------------------------
# Assign time periods based on YEAR.
period.df <- bind.df %>% 
  group_by(SITE, AGENCY, SEASON, YEAR, ICPRB_NAME, ICPRB_UNITS, GAGE_ID) %>% 
  summarise(COUNT = n())

period.df$PERIOD <- ifelse(period.df$YEAR %in% 1972:1981, "p1",
                         ifelse(period.df$YEAR %in% 1982:1991, "p2",
                                ifelse(period.df$YEAR %in% 1992:2001, "p3",
                                       ifelse(period.df$YEAR %in% 2002:2011, "p4", "p5"))))
#------------------------------------------------------------------------------
freq.df <- period.df %>% 
  group_by(SITE, AGENCY, SEASON, PERIOD, ICPRB_NAME, ICPRB_UNITS, GAGE_ID) %>% 
  summarise(COUNT = n() / 10 * 100) %>% 
  tidyr::spread(PERIOD, COUNT) %>% 
  tidyr::replace_na(list(p1 = 0, p2 = 0, p3 = 0, p4 = 0, p5 = 0)) %>% 
  filter(p1 >= 70, p4 >= 70) %>% 
  mutate(statistic = case_when(
    p2 >= 70 & p3 >= 70 ~ "continuous",
    p2 < 70 | p3 < 70 ~ "step",
    TRUE ~ "error"
  ))
#------------------------------------------------------------------------------
long.term <- semi_join(bind.df, freq.df,
                       by = c("SITE", "AGENCY", "SEASON",
                              "ICPRB_NAME", "ICPRB_UNITS", "GAGE_ID")) %>% 
  select(-MID, - DIFF)
#------------------------------------------------------------------------------
rm(period.df, dups.rm, mid.df, mid.julian, clean.df)


