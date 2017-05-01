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
outliers <- function(param.df){
  param.df$Q25 <- quantile(param.df$REPORTED_VALUE, 0.25)
  param.df$Q75 <- quantile(param.df$REPORTED_VALUE, 0.75)
  iqr <- IQR(param.df$REPORTED_VALUE)
  param.df$LOW_FENCE_1.5 <- unique(param.df$Q25) - (iqr * 1.5)
  param.df$UP_FENCE_1.5 <- unique(param.df$Q75) + (iqr * 1.5)
  param.df$OUTLIER_1.5 <- ifelse(param.df$REPORTED_VALUE <= param.df$LOW_FENCE_1.5 |
                                   param.df$REPORTED_VALUE >= param.df$UP_FENCE_1.5,
                                 TRUE, FALSE)
  param.df$LOW_FENCE_3 <- unique(param.df$Q25) - (iqr * 3)
  param.df$UP_FENCE_3 <- unique(param.df$Q75) + (iqr * 3)
  param.df$OUTLIER_3 <- ifelse(param.df$REPORTED_VALUE <= param.df$LOW_FENCE_3 |
                                   param.df$REPORTED_VALUE >= param.df$UP_FENCE_3,
                                 TRUE, FALSE)
  param.df$LOW_FENCE_4.5 <- unique(param.df$Q25) - (iqr * 4.5)
  param.df$UP_FENCE_4.5 <- unique(param.df$Q75) + (iqr * 4.5)
  param.df$OUTLIER_4.5 <- ifelse(param.df$REPORTED_VALUE <= param.df$LOW_FENCE_4.5 |
                                   param.df$REPORTED_VALUE >= param.df$UP_FENCE_4.5,
                                 TRUE, FALSE)
  #param.df$OUTLIER <- factor(param.df$OUTLIER, levels = c(TRUE, FALSE))
  return(param.df)
}
#==============================================================================
param_outliers <- function(param.df){
  final.df <- data.frame(PARAMETER = unique(param.df$PARAMETER))
  final.df$Q25 <- quantile(param.df$REPORTED_VALUE, 0.25)
  final.df$Q50 <- quantile(param.df$REPORTED_VALUE, 0.50)
  final.df$Q75 <- quantile(param.df$REPORTED_VALUE, 0.75)
  iqr <- IQR(param.df$REPORTED_VALUE)
  final.df$LOW_FENCE_1.5 <- unique(final.df$Q25) - (iqr * 1.5)
  final.df$UP_FENCE_1.5 <- unique(final.df$Q75) + (iqr * 1.5)
  final.df$LOW_FENCE_3 <- unique(final.df$Q25) - (iqr * 3)
  final.df$UP_FENCE_3 <- unique(final.df$Q75) + (iqr * 3)
  final.df$LOW_FENCE_4.5 <- unique(final.df$Q25) - (iqr * 4.5)
  final.df$UP_FENCE_4.5 <- unique(final.df$Q75) + (iqr * 4.5)

  return(final.df)
}

test.this <- lapply(unique(wqt$PARAMETER), function(x){
  sub.wqt <- wqt[wqt$PARAMETER %in% x, ]
  final.df <- param_outliers(sub.wqt)
  return(final.df)
})
#==============================================================================
param.quant <- do.call(rbind, test.this)
#write.csv(param.quant, "param_quant_2_16_17.csv", row.names = FALSE)
# Connect to the PostgreSQL database "WQT".
con = dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
dbWriteTable(con, "Outliers", param.quant , overwrite = TRUE , row.names = FALSE)
#==============================================================================
test <- wqt[wqt$PARAMETER %in% "DO", ]
test <- test[order(test$REPORTED_VALUE), ]
test2 <- outliers(test)
x <- "AL_TOT"
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Param_Outliers")
lapply(unique(wqt$PARAMETER), function(x){
  print(x)
  sub.wqt <- wqt[wqt$PARAMETER %in% x, ]
  sub.wqt <- sub.wqt[order(sub.wqt$REPORTED_VALUE), ]
  test <- outliers(sub.wqt, 4.5)
  test <- test[test$REPORTED_VALUE < test$UP.FENCE, ]
  ggplot(test, aes(1:nrow(test), REPORTED_VALUE, color = OUTLIER)) +
    geom_point() +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    labs(title = paste0(unique(test$PARAMETER), " (", unique(test$UNITS), ")"),
         subtitle = paste0("Lower Fence: ", test$LOW.FENCE, ", Upper Fence: ", test$UP.FENCE),
         y = paste0(unique(test$PARAMETER), " (", unique(test$UNITS), ")")) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  ggsave(paste0(unique(test$PARAMETER), "outliers_4.png"))
})

