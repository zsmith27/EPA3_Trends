setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output")
write.csv(head(bound), "legacy_example.csv", row.names = FALSE)
write.csv(head(wqp), "wqp_example.csv", row.names = FALSE)



#==============================================================================
# dplyr and RPostgreSQL aid in the communication between R and Postgresql.
library(dplyr)
library(RPostgreSQL)
library(ggplot2)
library(gridExtra)
library(cowplot)
#==============================================================================
#==============================================================================
# Connect to the PostgreSQL database "WQT".
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT", host = "localhost", port = 5432)
#==============================================================================
# Import table from the WQT database.
wqt <- dbReadTable(con, "Trend_Data")

wqt$SITE_AGENCY <- paste(wqt$SITE, wqt$AGENCY.x, sep = "%")
#x <- unique(wqt$SITE_AGENCY)[1]
#x <- "140404%1118ATL8" 
ll.counts <- lapply(unique(wqt$SITE_AGENCY), function(x){
  print(x)
  wqt.sub <- wqt[wqt$SITE_AGENCY %in% x, ]
  wqt.sub$LAT_LONG <- paste(wqt.sub$LATITUDE, wqt.sub$LONGITUDE, sep = "_")
  final.df <- data.frame(table(wqt.sub$LAT_LONG))
  names(final.df) <- c("LAT_LONG", "COUNT")
  final.df$LATITUDE <- gsub("_.*", "", final.df$LAT_LONG)
  final.df$LONGITUDE <- gsub(".*_", "", final.df$LAT_LONG)
  final.df$SITE <- gsub("%.*", "", x)
  final.df$AGENCY <- gsub(".*%", "", x)
  final.df <- final.df[, c("SITE", "AGENCY", "LATITUDE", "LONGITUDE", "COUNT")]
  return(final.df)
})

final.df <- do.call(rbind, ll.counts)
setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output")
write.csv(final.df, paste0("lat_long_counts_", Sys.Date(), ".csv"), row.names = FALSE)


#==============================================================================
# Checks

test <- wqt[wqt$SITE_AGENCY %in% x, ]

setwd("C:/Users/zsmith/Desktop/WQ_Trends/R_Data")

miss.df <- read.csv("missing_sites.csv", stringsAsFactors = FALSE)
names(miss.df)[1:2] <- c("AGENCY", "SITE")
miss.df$LATITUDE <- round(miss.df$LATITUDE, 5)
miss.df$LONGITUDE <- round(miss.df$LONGITUDE, 5)

final.df$LATITUDE <- round(as.numeric(final.df$LATITUDE), 5)
final.df$LONGITUDE <- round(as.numeric(final.df$LONGITUDE), 5)
test <- merge(final.df, miss.df, by = c("AGENCY", "SITE", "LATITUDE", "LONGITUDE"), all.y = TRUE)

i <- "USGS-01596500"
for (i in miss.df$SITE){
  print(i)
  print(table(wqp[wqp$SITE %in% i, ]$ICPRB_NAME))
}

for (i in miss.df$SITE){
  print(i)
  print(table(legacy[legacy$SITE %in% i, ]$ICPRB_NAME))
}

test.sites <- c("USGS-015767545", "USGS-01595300", "USGS-03200500", "USGS-03202400")
