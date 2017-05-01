
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data/Site_Data")
wqp.sites <- read.csv("sub_site_all.csv")
#==============================================================================
# Set the working directory
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Raw Data")
#==============================================================================
# Import the raw state data
dc.df  <- read.csv("wqp_dc_raw.csv")
de.df  <- read.csv("wqp_de_raw.csv")

# Maryland's data set was very large and had to be downloaded as multiple files
# This script downloads each file and combines them...
md.df1  <- read.csv("wqp_md_raw_1.csv")
md.df2  <- read.csv("wqp_md_raw_2.csv")
md.df3  <- read.csv("wqp_md_raw_3.csv")
md.df4  <- read.csv("wqp_md_raw_4.csv")
md.df5  <- read.csv("wqp_md_raw_5.csv")
md.df <- rbind(md.df1, md.df2, md.df3, md.df4, md.df5)

pa.df  <- read.csv("wqp_pa_raw.csv")
va.df  <- read.csv("wqp_va_raw.csv")
wv.df  <- read.csv("wqp_wv_raw.csv")
#==============================================================================
join_site_info <- function(state, sites) {
  # Join the site info with the param data.
  by.names <- c("OrganizationIdentifier", "OrganizationFormalName",
                "MonitoringLocationIdentifier") #, "MonitoringLocationName")
  final.df <- dplyr::left_join(state, sites, by = by.names )
  keep.cols <- c("OrganizationIdentifier", "OrganizationFormalName",
                 "MonitoringLocationIdentifier",
                 "LatitudeMeasure", "LongitudeMeasure")
  final.df <- unique(final.df[, keep.cols])
  return(final.df)
}
#==============================================================================
dc <- join_site_info(dc.df, wqp.sites)
de <- join_site_info(de.df, wqp.sites)
md <- join_site_info(md.df, wqp.sites)
pa <- join_site_info(pa.df, wqp.sites)
va <- join_site_info(va.df, wqp.sites)
wv <- join_site_info(wv.df, wqp.sites)
all.wqp <- rbind(dc, de, md, pa, va, wv)
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Match_to_Legacy")
sub.leg <- read.csv("legacy_no_match.csv")
#==============================================================================
approx_lat_long <- function(leg, wqp, r_num){
  leg$LAT <- round(leg$LATITUDE, r_num)
  leg$LONG <- round(leg$LONGITUDE, r_num)
  wqp$LAT <- round(wqp$LatitudeMeasure, r_num)
  wqp$LONG <- round(wqp$LongitudeMeasure, r_num)
  
  final.df <- merge(leg, wqp, by = c("LAT", "LONG"))
  return(final.df)
}
#==============================================================================
test3 <- approx_lat_long(sub.leg, all.wqp, 3)
test4 <- approx_lat_long(sub.leg, all.wqp, 4)
test5 <- approx_lat_long(sub.leg, all.wqp, 5)


