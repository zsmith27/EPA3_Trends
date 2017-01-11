#==============================================================================
# Author: Zachary M. Smith
# Created: 1/10/17
# Updated: 1/10/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to 
# Output: Creates individual ".txt" files for each EPA Region 3 State.
#==============================================================================
#==============================================================================
# Set working directory.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
#==============================================================================
# Import the data for each state. 
# The data was previously subset to only include potential params of interest.
dc.df <- read.csv("sub_wqp_dc.csv", stringsAsFactors = FALSE)
de.df <- read.csv("sub_wqp_de.csv", stringsAsFactors = FALSE)
md.df <- read.csv("sub_wqp_md.csv", stringsAsFactors = FALSE)
pa.df <- read.csv("sub_wqp_pa.csv", stringsAsFactors = FALSE)
va.df <- read.csv("sub_wqp_va.csv", stringsAsFactors = FALSE)
wv.df <- read.csv("sub_wqp_wv.csv", stringsAsFactors = FALSE)
#==============================================================================
# This function counts the occurance of unique params.
organize.wq <- function(org.df){
  final.df <- aggregate(OrganizationIdentifier ~ CharacteristicName +
                      ResultSampleFractionText + ResultMeasure.MeasureUnitCode +
                        ResultAnalyticalMethod.MethodName + ProviderName,
                    data = org.df,  FUN = length)
  final.df <- final.df[order(final.df$CharacteristicName), ]
  final.df[, 1:4] <- apply(final.df[, 1:4], 2, trimws)
  final.df$ResultSampleFractionText <- ifelse(final.df$ResultSampleFractionText %in% "<Blank>",
                                              "", final.df$ResultSampleFractionText)
  final.df$ResultSampleFractionText <- ifelse(final.df$ResultSampleFractionText %in% "Total Recovrble",
                                              "Total Recoverable", final.df$ResultSampleFractionText)
  final.df$ResultMeasure.MeasureUnitCode <- ifelse(final.df$ResultMeasure.MeasureUnitCode %in% "None",
                                                   "", final.df$ResultMeasure.MeasureUnitCode )
  names(final.df)[6] <- "COUNT"
  return(final.df)
}
#==============================================================================
# Apply the organize.wq function to each state.
dc <- organize.wq(dc.df)
de <- organize.wq(de.df)
md <- organize.wq(md.df)
pa <- organize.wq(pa.df)
va <- organize.wq(va.df)
wv <- organize.wq(wv.df)
#==============================================================================
# Bind all of the states together.
bound <- rbind(dc, de, md, pa, va, wv)
# Sum the counts by the unique params.
bound.df <- aggregate(COUNT ~ CharacteristicName +
                        ResultSampleFractionText + ResultMeasure.MeasureUnitCode +
                        ResultAnalyticalMethod.MethodName + ProviderName,
                      data = bound,  FUN = sum)
bound.df <- bound.df[order(bound.df$CharacteristicName), ]

new.df <- bound.df[bound.df$COUNT > 500, ]
#==============================================================================

table(bound.df$ResultMeasure.MeasureUnitCode)
table(bound.df$CharacteristicName)
#==============================================================================



chl <- md.df[md.df$CharacteristicName %in% "chloride", ]
test <- data.frame(table(chl$MonitoringLocationIdentifier))
#==============================================================================








