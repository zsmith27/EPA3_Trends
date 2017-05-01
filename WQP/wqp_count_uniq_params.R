#==============================================================================
# Author: Zachary M. Smith
# Created: 1/10/17
# Updated: 1/12/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to organize the unique WQP parameter inputs
#          and to provide a count of rows associated with each parameter input.
# Output: Creates a single .csv table of all the unique WQP parameter inputs.
#==============================================================================
#==============================================================================
# Set working directory.
#setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
# Import the data for each state. 
# The data was previously subset to only include potential params of interest.
#dc.df <- read.csv("sub_wqp_dc.csv", stringsAsFactors = FALSE)
#de.df <- read.csv("sub_wqp_de.csv", stringsAsFactors = FALSE)
#md.df <- read.csv("sub_wqp_md.csv", stringsAsFactors = FALSE)
#pa.df <- read.csv("sub_wqp_pa.csv", stringsAsFactors = FALSE)
#va.df <- read.csv("sub_wqp_va.csv", stringsAsFactors = FALSE)
#wv.df <- read.csv("sub_wqp_wv.csv", stringsAsFactors = FALSE)
#==============================================================================
# Only need to import the single WQP file containing all of the EPA3 sites
# selected by Buchanan.
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
wqp.df <- read.csv("wqp_keep_stations.csv")
#==============================================================================
# This function counts the occurance of unique params.
# Columns ResultAnalyticalMethod.MethodIdentifier,
# ResultAnalyticalMethod.MethodIdentifierContext, 
# and ResultAnalyticalMethod.MethodName were added by Buchanan on 1-12-17
organize.wq <- function(org.df){
  final.df <- aggregate(OrganizationIdentifier ~ CharacteristicName +
                      ResultSampleFractionText + ResultMeasure.MeasureUnitCode +
                        ResultAnalyticalMethod.MethodName + ProviderName +
                        ResultAnalyticalMethod.MethodIdentifier + 
                        ResultAnalyticalMethod.MethodIdentifierContext + 
                        ResultAnalyticalMethod.MethodName,
                    data = org.df,  FUN = length)
  final.df <- final.df[order(final.df$CharacteristicName), ]
  final.df[, 1:7] <- apply(final.df[, 1:7], 2, trimws)
  final.df$ResultSampleFractionText <- ifelse(final.df$ResultSampleFractionText %in% "<Blank>",
                                              "", final.df$ResultSampleFractionText)
  final.df$ResultSampleFractionText <- ifelse(final.df$ResultSampleFractionText %in% "Total Recovrble",
                                              "Total Recoverable", final.df$ResultSampleFractionText)
  final.df$ResultMeasure.MeasureUnitCode <- ifelse(final.df$ResultMeasure.MeasureUnitCode %in% "None",
                                                   "", final.df$ResultMeasure.MeasureUnitCode )
  names(final.df)[8] <- "COUNT"
  return(final.df)
}
#==============================================================================
# Apply the organize.wq function to each state.
#dc <- organize.wq(dc.df)
#de <- organize.wq(de.df)
#md <- organize.wq(md.df)
#pa <- organize.wq(pa.df)
#va <- organize.wq(va.df)
#wv <- organize.wq(wv.df)
bound <- organize.wq(wqp.df)
#==============================================================================
# Bind all of the states together.
#bound <- rbind(dc, de, md, pa, va, wv)
# Sum the counts by the unique params.
# Columns ResultAnalyticalMethod.MethodIdentifier,
# ResultAnalyticalMethod.MethodIdentifierContext, 
# and ResultAnalyticalMethod.MethodName were added by Buchanan on 1-12-17
bound.df <- aggregate(COUNT ~ CharacteristicName +
                        ResultSampleFractionText + ResultMeasure.MeasureUnitCode +
                        ResultAnalyticalMethod.MethodName + ProviderName +
                        ResultAnalyticalMethod.MethodIdentifier + 
                        ResultAnalyticalMethod.MethodIdentifierContext + 
                        ResultAnalyticalMethod.MethodName,
                      data = bound,  FUN = sum)
bound.df <- bound.df[order(bound.df$CharacteristicName), ]
#==============================================================================
# Parameter generalizations.
grep.params <- c("temperature", "conductance", "conductivity", "oxygen",
                 "ph", "alkalinity", "nitrogen", "nitrite", "nitrate",
                 "phosphorus", "carbon", "hardness", "calcium", "magnesium",
                 "sodium", "potassium", "chloride", "sulfate", "iron",
                 "lead", "manganese", "zinc", "aluminum", "selenium", "chlorophyll",
                 "solids", "acidity", "suspended", "turbidity", "ammonia")

# This loop assigns a common parameter name based on the Charactersitic name.
# For example, if CharacteristicName = "acidity, hydrogen ion (h+)", then 
# "acidity" is assigned.
list.final <- list()
for(i in seq_along(bound.df$CharacteristicName)) {
  list.params <- list()
  for(j in seq_along(grep.params)) {
    list.params[[j]] <- grepl(grep.params[j], bound.df$CharacteristicName[i])
  }
  
  list.final[[i]] <- grep.params[unlist(list.params)]

}
# Convert the loop list to character type and add to the bound.df table.
bound.df$PARAM <- as.character(list.final)
#==============================================================================
# Export the table for further review by Buchanan.
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Jan2017/Legacy/Claire_Specifications")
write.csv(bound.df, "wqp_params_01_12_17.csv", row.names = FALSE)
#==============================================================================









