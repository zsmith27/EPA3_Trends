library(RPostgreSQL)
#==============================================================================
conn <- dbConnect("PostgreSQL",
                  user = "Guest",
                  password = "Guest",
                  dbname = "WQT2",
                  host = "192.168.1.214",
                  port = 5432)
on.exit(dbDisconnect(conn), add = TRUE)

sites <- dbGetQuery(conn, paste("SELECT table_name
    FROM information_schema.tables
    WHERE table_schema='public'
    AND table_type='BASE TABLE';"))
sites <- sites[order(sites$table_name),]
#==============================================================================
con <- dbConnect("PostgreSQL",
                  #user = "Z_Smith",
                  #password = "Hexapoda27",
                 user = "Guest",
                 password = "Guest",
                  dbname = "WQT",
                  host = "192.168.1.214",
                  port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
param.range <- dbReadTable(con, "param_range")
#range_param <- dbReadTable(con, "Trend_Data")
#==============================================================================
prep_plot <- function(long, site, param){
  #============================================================================
  # Prepare Date
  #============================================================================
  long <- long[long$SITE %in% site &
               long$PARAMETER %in% param, ]
  # Identify column "DATE" as class date
  long$DATE <- as.Date(long$DATE, "%Y-%m-%d")
  # Create a new column by extracting the year from date
  long$YEAR <- format(long$DATE, "%Y")
  # Create a new column by extracting the month from date
  long$MONTH <- format(long$DATE, "%m")
  #============================================================================
  # Sequence Dates
  #============================================================================
  long$REPORTED_VALUE <- as.numeric(as.character(long$REPORTED_VALUE))
  long <- long[,c("SITE", "MONTH", "YEAR", "PARAMETER",
                  "REPORTED_VALUE", "UNITS")]
  long <- long[long$YEAR >= 1972, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(long)
  plot.me <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR, PARAMETER, UNITS)]
  
  
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
  final.df <- unique(merged[order(merged$YEAR), ])
  final.df$YEAR <- as.character(final.df$YEAR)
  
  return(final.df)
}
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
  site <- site[,c("SITE", "MONTH", "YEAR", "PARAMETER",
                  "REPORTED_VALUE", "UNITS")]
  site <- site[site$YEAR >= start.year, ]
  # Use data.table functions to find the mean of reporting value. More than
  # one value per month/year/parameter would cause this function to fail
  # at a later stage.
  long.df <- data.table::data.table(site)
  long.df <- long.df[, REPORTED_VALUE := mean(REPORTED_VALUE),
                     by = list(SITE, MONTH, YEAR, PARAMETER, UNITS)]
  
  
  #============================================================================
  print("Calculating Paramter Min/Max...")
  max.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, max)
  names(max.df) <- c("PARAMETER", "MAX")
  ninteyfive.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, function(x) quantile(x, 0.95))
  names(ninteyfive.df) <- c("PARAMETER", "95th")
  min.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, min)
  names(min.df) <- c("PARAMETER", "MIN")
  five.df <- aggregate(REPORTED_VALUE ~ PARAMETER, data = long.df, function(x) quantile(x, 0.05))
  names(five.df) <- c("PARAMETER", "05th")
  final.df <- plyr::join_all(c(min.df, max.df, ninteyfive.df, five.df), by = "PARAMETER")
  #============================================================================
  
  return(final.df)
}
#==============================================================================
loess_plot <- function(plot.me){
  final.plot <- ggplot2::ggplot(plot.me, aes(x = DATE, y = REPORTED_VALUE)) + 
    labs(x = "Date",
         y = paste(unique(plot.me$PARAMETER), " (", unique(plot.me$UNITS), ")", sep = ""),
         size = 12) +
    #geom_line(color = "#56B4E9", size = 1)  + # "royalblue3"
    geom_point(color = "#56B4E9", size = 2) +
    stat_smooth(method = 'loess', color = "#E69F00", size = 1.2) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)
    ) 
  return(final.plot)
}
#==============================================================================
tile_plot <- function(plot.me, param.range){
  param.range <- param.range[param.range$PARAMETER %in% plot.me$PARAMETER, ]
  median.value <- median(plot.me$REPORTED_VALUE, na.rm = TRUE)
  if(median.value >= 5) plot.me$REPORTED_VALUE <- round(plot.me$REPORTED_VALUE, 0)
  if (median.value < 5 & median.value >= 0.09) plot.me$REPORTED_VALUE <- round(plot.me$REPORTED_VALUE, 1)
  if (median.value < 0.09 & median.value >= 0.009) plot.me$REPORTED_VALUE <- round(plot.me$REPORTED_VALUE, 2)
  if (median.value < 0.009 & median.value >= 0.0009) plot.me$REPORTED_VALUE <- round(plot.me$REPORTED_VALUE, 3)
  
  final.plot <- ggplot2::ggplot(data = plot.me, aes(x = YEAR, y = MONTH, group = c(SITE))) +
    #labs(title = paste("Site:", long.df$SITE, sep = " ")) +
    labs(title = paste("Site:", plot.me$SITE, sep = " "),
         subtitle = paste("Parameter:", unique(plot.me$PARAMETER),
                          unique((plot.me$UNITS)), sep = " "),
         x = "Year",
         y = "Month") +
    geom_tile(aes(fill = REPORTED_VALUE), colour = "white") +
    geom_text(aes(label = ifelse(is.na(as.character(REPORTED_VALUE)), "",
                                 sprintf("%s", as.character(REPORTED_VALUE)))),
              size = 3) +
    
    scale_fill_gradientn(#colours = cbPalette,
      colours = c("#56B4E9", "#E69F00"),
      breaks = c(param.range$MIN - 0.001,  param.range$MAX),
      #breaks = c(param.range$'05th',  param.range$'95th'),
      labels = c(param.range$MIN,  param.range$MAX),
      limits = c(param.range$MIN - 0.001,  param.range$MAX),
      guide = "colourbar",
      na.value = "white"
      #name = "RANGE"
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 11),
          text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
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
  return(final.plot)
}
#==============================================================================