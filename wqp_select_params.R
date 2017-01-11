#==============================================================================
# Author: Zachary M. Smith
# Date: 2016
# Maintained: Zachary M. Smith
# Purpose: The script was written to subset the raw Water Quality Portal (WQP)
#          dataset based on parameters of interest.
# Output: Creates individual ".txt" files for each EPA Region 3 State.
#==============================================================================
#==============================================================================
# The water Quality Data below was downloaded from the 
# Water Quality Portal (WQP) http://waterqualitydata.us/
#
# The data was refined by searching the CharacteristicName
# (the WQ parameter column) that contianed strings with WQ parameters of 
# interest using the grep function.  For example searching for turbidity 
# returned turbidity and turbidity severity.  This intial search allowed us 
# to further manually specify which parameters should be included in the 
# exported table.
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
# A function for exploring the parameters of interest.
# Use this function to select parameters with the potential for long-term 
# assessment.
explore_params <- function(state.df){
  state.df$CharacteristicName <- tolower(state.df$CharacteristicName)
  # The general names of the WQ parameters of interest
  grep.params <- c("temperature", "conductance", "conductivity", "oxygen",
                   "ph", "alkalinity", "nitrogen", "nitrite", "nitrate",
                   "phosphorus", "carbon", "hardness", "calcium", "magnesium",
                   "sodium", "potassium", "chloride", "sulfate", "iron",
                   "lead", "manganese", "zinc", "aluminum", "selenium", "chlorophyll",
                   "solids", "acidity", "suspended", "turbidity", "ammonia")
  # The general WQ names are used to pull out a subset of the raw data set.
  final.df <- state.df[grepl(paste(grep.params, collapse = "|"), state.df$CharacteristicName), ]
  final.df <- data.frame(unique(final.df$CharacteristicName), stringsAsFactors = FALSE)
  return(final.df)
}
# Test the explore_params function.
test <- explore_params(dc.df)
#==============================================================================
# A function for subsetting the data for specific parameters.
# The output from the explore_params function can be used to select a subset
# of parameters.
subset_params <- function(state.df){
  state.df$CharacteristicName <- tolower(state.df$CharacteristicName)
  #==============================================================================
  # The cummulative parameters from all EPA3 states that may be of interest.
  # These parameters were selected from the primary review process above.
  param.interest <- c("specific conductance", "conductivity", "ph", "ph, lab",
                      "temperature, water",
                      "dissolved oxygen (do)", 
                      "turbidity","turbidity severity", "turbidity, hellige",
                      "chlorophyll a", "chlorophyll", "chlorophyll a (probe)",
                      "chlorophyll a, uncorrected for pheophytin",
                      "organic carbon", "total carbon", "inorganic carbon",
                      "kjeldahl nitrogen", 
                      "nitrate as no3","nitrite", "nitrite as no2", "nitrogen",
                      "nutrient-nitrogen", "organic nitrogen", "nitrate as n",
                      "nitrite as n", 
                      "nitrogen, mixed forms (nh3), (nh4), organic, (no2) and (no3)" ,
                      "inorganic nitrogen (nitrate and nitrite) as n",
                      "inorganic nitrogen (nitrate and nitrite)",
                      "total nitrogen/total phosphorus ratio (tn:tp)",
                      "ammonia-nitrogen", "ammonia-nitrogen as n", "ammonia as nh3",
                      "ammonia and ammonium",
                      "phosphorus", "organic phosphorus as p", "phosphate-phosphorus as P", 
                      "phosphate-phosphorus", "phosphate-phosphorus as po4", 
                      "organic phosphorus", "phosphorus, particulate organic",
                      "calcium",
                      "alkalinity, total", "alkalinity", "alkalinity, hydroxide as caco3",
                      "suspended sediment concentration (ssc)","total suspended solids", 
                      "fixed suspended solids",  "volatile suspended solids",
                      "settleable solids", "total volatile solids", "total solids",
                      "floating solids or foam", "total fixed solids",
                      "volatile dissolved solids", "fixed dissolved solids",
                      "total dissolved solids", 
                      "magnesium", "selenium", "lead", "zinc",
                      "sodium", "sodium, percent total cations", "sodium plus potassium",
                      "potassium",
                      "phosphorus as po4",  
                      "iron",  "iron, ion (fe2+)", "chloride",
                      "sulfate", "sulfate as so4", "sulfate as s",
                      "manganese", 
                      "total hardness -- sdwa npdwr", "hardness, non-carbonate", 
                      "hardness, carbonate", "total hardness", "hardness, ca, mg",
                      "acidity, hydrogen ion (h+)", "acidity, hydrogen ion (h+) as caco3",
                      "free mineral acidity (fma)", "total hardness -- sdwa npdwr", 
                      "aluminum", "inorganic monomeric aluminum",
                      "aluminum, organic + inorganic monomeric (reactive aluminum)",
                      "aluminum, organic monomeric (reactive aluminum)")
  # Create a data frame containing only the WQ paramters of interest.
  final.df <- state.df[state.df$CharacteristicName %in% param.interest, ]
  #==============================================================================
  return(final.df)
}
#==============================================================================
# Subset each of the states to only include the specified parameters.
sub.dc <- subset_params(dc.df)
sub.de <- subset_params(de.df)
sub.md <- subset_params(md.df)
sub.pa <- subset_params(pa.df)
sub.va <- subset_params(va.df)
sub.wv <- subset_params(wv.df)
#==============================================================================
# Export the subsets to a specific folder.
setwd("//pike/data/Projects/EPA3Trends/r_scripts/R_Data/WQP/Subset Raw Data")
write.csv(sub.dc, "sub_wqp_dc.csv", row.names = FALSE)
write.csv(sub.de, "sub_wqp_de.csv", row.names = FALSE)
write.csv(sub.md, "sub_wqp_md.csv", row.names = FALSE)
write.csv(sub.pa, "sub_wqp_pa.csv", row.names = FALSE)
write.csv(sub.va, "sub_wqp_va.csv", row.names = FALSE)
write.csv(sub.wv, "sub_wqp_wv.csv", row.names = FALSE)



