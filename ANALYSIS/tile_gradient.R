#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/26/17
# Updated: 1/26/17
# Maintained: Zachary M. Smith
# Purpose: Merge the Legacy STORET data with the WQP data.
# Output: The merged data is exported to the WQT PostgreSQL database.
#==============================================================================
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
#==============================================================================
station.df <- wqt
station.df <- wqt[wqt$SITE %in% "140403",]
year.int = 1
start.year = 1972

tile_gradient <- function(station.df, year.int = 1, start.year = 1972){
  #============================================================================
  # Prepare Date
  #============================================================================
  # Identify column "DATE" as class date
  station.df$DATE <- as.Date(station.df$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  station.df$YEAR <- format(station.df$DATE, "%Y")
  # Create a new column by extracting the month from date
  station.df$MONTH <- format(station.df$DATE, "%m")
  #============================================================================
  # Sequence Dates
  #============================================================================
  # year.int is specified when calling the function (default = 1). This refers
  # to the yearly intervals of interest.  For example year.int = 5 would aggregate
  # the years into 5 year periods (1905, 1910, 1915, 1920,...).
  
  station.df$REPORTED_VALUE <- as.numeric(as.character(station.df$REPORTED_VALUE))
  print("Aggregating by Month/Year...")
  if (year.int == 1) {
    # Keep only necessary columns
    station.df <- station.df[,c("SITE", "MONTH", "YEAR", "PARAMETER",
                                "REPORTED_VALUE", "UNITS")]
    station.df <- station.df[station.df$YEAR >= start.year, ]
    # Use data.table functions to find the mean of reporting value. More than
    # one value per month/year/parameter would cause this function to fail
    # at a later stage.
    long.df <- data.table::data.table(station.df)
    long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE), by = list(SITE, MONTH, YEAR,
                                                                           PARAMETER, UNITS)]
  } else {
    # start.year is specified when calling the function (default = 1905). This refers
    # to the year that the yearly intervals, defined by year.int, should start.
    # In seq function below the end of the sequence is specified by the most recent 
    # sample date rounded up to nearst whole number divisible by year.int.
    # This will prevent the sequence from stopping too early. For example, 
    # if the end of the sequence was set to 2012 and year.int = 5, 
    # then the sequence would end at 2010; the script below will end at 2015.
    intervals.df <- data.frame(START = seq(start.year,
                                           year.int * ceiling(as.numeric(unique(
                                             max(station.df$YEAR)
                                           )) / year.int),
                                           by = year.int))
    # intervals.df$START creates beginning of a new sampling period. This
    # code creates the end of the sampling period by subtracting one from year.int
    # and adding the result to the START column.  For example, if year.int = 5, then
    # the first sampling period should include 1905 (1), 1906 (2), 1907 (3),
    # 1908 (4), 1909 (5). The sampling period ends at 1909 not 1910, thus, year.int
    # minus one is appropriate.
    intervals.df$END <- intervals.df$START + (year.int - 1)
    
    # Keep only necessary columns
    station.df <- station.df[, c("SITE", "MONTH", "YEAR", "PARAMETER",
                                 "REPORTED_VALUE", "UNITS")]
    
    # A loop is used to systematically check sampling period the row belongs in.
    # I think the easiest way to do this is to create subsets of the data based
    # on each sampling period (i = nrow(interval.df)) and then join the subsets
    # back together.
    data.list <- list()
    for (i in 1:nrow(intervals.df)) {
      new.int <- intervals.df[i, ]
      # Skip loop i if the max year is less than the START column from new.int
      if (max(station.df$YEAR) < new.int[, 1]) next
      # Keep only the rows that fall within the sampling period
      long.sub <- station.df[(station.df$YEAR >= new.int[, 1] &
                                station.df$YEAR <= new.int[, 2]), ]
      
      # Replace "YEAR" with the sampling period
      long.sub$YEAR <- paste(new.int[, 1], new.int[, 2], sep = "_")
      
      
      
      data.list[[i]] <- long.sub # add to list
      
      
    }
    # Join the subsets together
    long.df <- do.call(rbind, data.list)
    long.df <- data.table::data.table(long.df)
    long.df <- unique(long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                              by = list(SITE, MONTH, YEAR, PARAMETER, UNITS)])
  }
  #============================================================================
  # Finalize Format
  #============================================================================
  # Use spread from tidyr package to make the long dataframe into
  # a wide dataframe based on YEAR.
  #wide.df <- tidyr::spread(long.df, YEAR, REPORTED_VALUE)
  #wide.df <- as.data.frame(wide.df)
  # Sort the dataframe by the PARAMETER column.
  #wide.df <- wide.df[order(wide.df$PARAMETER), ]
  # Find the average value for each row.
  #wide.df$AVERAGE <- apply(wide.df[, 4:ncol(wide.df)], 1, mean, na.rm = T)
  #============================================================================
  # Export
  #============================================================================
  #final.df <- wide.df
  #final.df <- sapply(final.df, as.character)
  #final.df[is.na(final.df)] <- ""
  #write.csv(final.df, paste(paste(unique(wide.df$SITE),
  #                                todays_date = format(Sys.time(), "%m_%d_%y"),
  #                                sep = "_"), ".csv", sep = ""), row.names = F)
  #============================================================================
  print("Calculating Paramter Min/Max...")
  max.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, max)
                      #function(x) quantile(x, 0.95))
  names(max.df) <- c("PARAMETER", "MAX")
  min.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, min)
                      #function(x) quantile(x, 0.05))
  names(min.df) <- c("PARAMETER", "MIN")
  range.df <- merge(min.df, max.df, by = "PARAMETER")
  #============================================================================
  setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output")
  #pdf("test.pdf")
  for (i in unique(long.df$SITE)) {

    sub.df <- long.df[long.df$SITE %in% i, ]
    prep.df <- unique(sub.df[, c("SITE", "PARAMETER")])
    split.list <- unique(split(prep.df$PARAMETER, prep.df$SITE))
    for (j in unlist(split.list)) {
      print(paste("Site:", i, "_", j, sep = " "))
      #pdf(paste(i, ".pdf", sep = ""))
     
      print(paste("...Parameter:", j, sep = " "))
      plot.me <- sub.df[sub.df$PARAMETER  %in% j, ]
      plot.me$DATE <- as.Date(paste(plot.me$YEAR, plot.me$MONTH, 01, sep = "-"))
      #========================================================================
      new.df <- data.frame(YEAR = as.character(sort(rep(1972:2016, 12))))
      new.df$MONTH <- c("01", "02", "03", "04", "05", "06",
                        "07", "08", "09", "10", "11", "12")
      merged <- merge(plot.me, new.df, by = c("MONTH", "YEAR"), all = TRUE)
      merged$REPORTED_VALUE <- ifelse(is.na(merged$SITE), "", merged$REPORTED_VALUE)
      merged$REPORTED_VALUE <- as.numeric(merged$REPORTED_VALUE)
      merged$SITE <- unique(plot.me$SITE)
      merged$PARAMETER <- unique(plot.me$PARAMETER)
      merged$UNITS <- unique(plot.me$UNITS)[1]
      merged$YEAR <- as.numeric(as.character(merged$YEAR))
      plot.me <- unique(merged[order(merged$YEAR), ])
      plot.me$YEAR <- as.character(plot.me$YEAR)
      #========================================================================
      
      # Use spread from tidyr package to make the long dataframe into
      # a wide dataframe based on YEAR.
      wide.df <- tidyr::spread(plot.me, YEAR, REPORTED_VALUE)
      wide.df <- as.data.frame(wide.df)
      # Sort the dataframe by the PARAMETER column.
      wide.df <- wide.df[order(wide.df$PARAMETER), ]
      
      #============================================================================
      param.range <- range.df[range.df$PARAMETER %in% j, ]
      # The palette with grey:
      #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
      #               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      
      plot.1 <- ggplot(data = plot.me, aes(x = YEAR, y = MONTH, group = c(SITE))) +
        #labs(title = paste("Site:", long.df$SITE, sep = " ")) +
        labs(title = paste("Site:", long.df$SITE, sep = " "),
             subtitle = paste("Parameter:", j, sep = " "),
             x = "Year",
             y = "Month") +
        geom_tile(aes(fill = REPORTED_VALUE), colour = "white") +
        geom_text(aes(label = ifelse(is.na(REPORTED_VALUE), "", sprintf("%1.0f", REPORTED_VALUE))),
                  size = 3) +
        
        scale_fill_gradientn(#colours = cbPalette,
                             colours = c("#56B4E9", "#E69F00"),
                             breaks = c(param.range$MIN - 0.001,  param.range$MAX),
                             labels = c(param.range$MIN,  param.range$MAX),
                             limits = c(param.range$MIN - 0.001,  param.range$MAX),
                             #guide = "none",
                             na.value = "white"
                             #name = "RANGE"
                             ) +
        theme(plot.title = element_text(hjust = 0.5, size = 10),
              plot.subtitle = element_text(hjust = 0.5, size = 9),
              text = element_text(size = 8),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              #legend.position = "top",
              legend.title = element_blank(),
              legend.position = c(0.95, 1),
              legend.direction = "horizontal",
              legend.justification = c(1, 0), 
              legend.key.width = unit(1, "lines"), 
              legend.key.height = unit(1, "lines"),
              #legend.background = element_rect(size = 0.5,
              #                                 linetype = "solid", 
              #                                 colour = "black"),
              axis.line = element_line(colour = "black"),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              #panel.border = element_blank(),
              #panel.border = element_rect(colour = "black", fill = NA, size = 5),
              panel.background = element_blank()
              ) 
      
      setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output/Tile")
      ggsave(paste("TILE_", i, "_", j, ".png", sep = ""), plot = plot.1, width = 13, height = 3, dpi = 120)
      
      
      
      plot.2 <- ggplot(plot.me, aes(x = DATE, y = REPORTED_VALUE)) + 
        labs(x = "Date",
             y = paste(j, " (", unique(plot.me$UNITS), ")", sep = ""),
             size = 8) +
        #geom_line(color = "#56B4E9", size = 1)  + # "royalblue3"
        geom_point(color = "#56B4E9", size = 2) +
        stat_smooth(method = 'loess', color = "#E69F00", size = 1.2) +
        scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
        theme(text = element_text(size = 8),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              #panel.border = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 5)
              ) 
      
      setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output/Loess")
      ggsave(paste("LOESS_", i, "_", j, ".png", sep = ""), plot = plot.2, width = 13, height = 3, dpi = 120)
      
      #print(
      #cowplot::plot_grid(plot.1, plot.2, align = "v", nrow = 2, rel_heights = c(3/4, 1/4))
      #)
      #setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output/Tile")
      #png(paste("TILE_", i, "_", j, ".png", sep = ""), 
          #units="in", 
          #width=5, 
          #height=4, 
          #pointsize=12, 
       #   res=72)
      #print(plot.1)
      #dev.off()
      #setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output/Loess")
      #png(paste("LOESS_", i, "_", j, ".png", sep = ""), 
      #    units="px", 
      #    width = 400, 
      #    height = 300, 
          #pointsize=12, 
      #    res=72)
      #print(plot.2)
      #dev.off()
    }
    
  }

  
  
  #============================================================================
  
  #return(wide.df)
}




tile_gradient(wqt)
#==============================================================================





#==============================================================================