#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Created: 1/25/17
# Updated: 2/22/17
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
#==============================================================================
# Load the parameter codes and reporting unit codes.
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Claire_Specifications/1_24_2017")
# This file contains the NWIS parameter codes selected by Buchanan.
keep.params <- read.csv("WQP_Specifications_1_24_2017.csv", colClasses = "character")
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/WQP/Subset Raw Data")
wqp <- read.csv("wqp_keep_stations.csv", stringsAsFactors = FALSE)
#==============================================================================

wqp.sub <- wqp[wqp$CharacteristicName %in% keep.params$CharacteristicName &
                 wqp$ResultSampleFractionText %in% keep.params$ResultSampleFractionText &
                 wqp$ResultMeasure.MeasureUnitCode %in% keep.params$ResultMeasure.MeasureUnitCode &
                 wqp$ResultAnalyticalMethod.MethodName %in% keep.params$ResultAnalyticalMethod.MethodName &
                 wqp$ProviderName %in% keep.params$ProviderName &
                 wqp$ResultAnalyticalMethod.MethodIdentifier %in% keep.params$ResultAnalyticalMethod.MethodIdentifier &
                 wqp$ResultAnalyticalMethod.MethodIdentifierContext %in% keep.params$ResultAnalyticalMethod.MethodIdentifierContext, ]
#==============================================================================
match.cols <- c("CharacteristicName", "ResultSampleFractionText",
                "ResultMeasure.MeasureUnitCode",
                "ResultAnalyticalMethod.MethodName",
                "ProviderName", "ResultAnalyticalMethod.MethodIdentifier",
                "ResultAnalyticalMethod.MethodIdentifierContext")
#==============================================================================
merged <- merge(wqp.sub,
                keep.params[, c(match.cols, "ICPRB_CODE", "ICPRB_NAME", "UNITS")],
                by = match.cols)
#==============================================================================
# Make sure the Results are classified as numeric.
merged$ResultMeasureValue <- as.numeric(merged$ResultMeasureValue)
# Remove values categorized as "Not Reported" or "Systematic Contamination."
merged <- merged[!merged$ResultDetectionConditionText %in% c("Not Reported", "Systematic Contamination"), ]
# Replace Reported Value with Detection Limit value.
replace.rows <- unique(merged$ResultDetectionConditionText)
# If ResultDetectionConditionText is blank ("") then keep the reported value.
replace.rows <- replace.rows[!replace.rows %in% ""]
merged$ResultMeasureValue <- ifelse(merged$ResultDetectionConditionText %in% replace.rows,
                                    merged$DetectionQuantitationLimitMeasure.MeasureValue,
                                    merged$ResultMeasureValue)
#==============================================================================
merged <- merged[!is.na(merged$ResultMeasureValue), ]
merged$ResultMeasureValue <- as.numeric(merged$ResultMeasureValue)
#==============================================================================
# Remove columns that contain no information.
blank.df <- data.frame(BLANK = sapply(merged, function(x) all(is.na(x) | x %in% "")))
blank.df$RM_COLS <- row.names(blank.df)
rm.cols <- blank.df[blank.df$BLANK == TRUE, "RM_COLS"]
merged <- merged[, !names(merged) %in% rm.cols]
# Remove unwanted Activity Media
merged <- merged[!merged$ActivityMediaSubdivisionName %in% c("Groundwater",
                                                            "Hyporheic zone",
                                                            "Wet Fall Material"), ]
# Remove unwanted sample collection methods.
merged <- merged[!merged$SampleCollectionMethod.MethodName %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE", ]
# Remove unwanted result status.
merged <- merged[!merged$ResultStatusIdentifier %in% "Rejected", ]
# Remove leading/trailing white space from column.
merged$ResultValueTypeName <- trimws(merged$ResultValueTypeName)
names(merged)[names(merged) %in% "ResultValueTypeName"] <- "RESULT_TYPE"
#==============================================================================
# Censored Data
#==============================================================================
merged$CENSORED <- ifelse(merged$ResultDetectionConditionText %in% "" |
                            is.na(merged$ResultDetectionConditionText),
                          "Uncensored", "Censored")
merged$ResultMeasureValue <- ifelse(merged$CENSORED %in% "CENSORED",
                                    merged$DetectionQuantitationLimitMeasure.MeasureValue,
                                    merged$ResultMeasureValue)
#==============================================================================
# ICPRB codes that indicating specific parameter rows that need to be converted.
mult.1000 <- c(36, 195:198, 206, 207, 262, 263, 231, 573, 733, 734)
div.1000 <- c(69:71, 282:284, 384, 507, 550:553, 584, 585)
ca.ueq <- 67:68
cl.ueq <- 91:93
#no23w.n <- 177
mg.ueq <- 280:281
pp.p <- 495
k.ueq <- 548:549
na.ueq <- c(582:583, 586)
so4.ueq <- 635:637
#==============================================================================
# Convert all depths reported as feet to meters.
merged$ActivityDepthHeightMeasure.MeasureValue <- 
  ifelse(
    merged$ActivityDepthHeightMeasure.MeasureUnitCode %in% c("feet", "ft"),
    merged$ActivityDepthHeightMeasure.MeasureValue * 0.3048,
    merged$ActivityDepthHeightMeasure.MeasureValue
  )
#==============================================================================
# Apply the necessary conversions by row.
merged$ICPRB_VALUE <- ifelse(
  merged$ICPRB_CODE %in% mult.1000,
  merged$ResultMeasureValue * 1000,
  ifelse(
    merged$ICPRB_CODE %in% div.1000,
    merged$ResultMeasureValue / 1000,
    ifelse(
      merged$ICPRB_CODE %in% ca.ueq,
      merged$ResultMeasureValue / 1000 * 20,
      ifelse(
        merged$ICPRB_CODE %in% cl.ueq,
        merged$ResultMeasureValue / 1000 * 35.5,
          ifelse(
            merged$ICPRB_CODE %in% mg.ueq,
            merged$ResultMeasureValue / 1000 * 12,
            ifelse(
              merged$ICPRB_CODE %in% pp.p,
              merged$ResultMeasureValue / 1000 * 0.326,
              ifelse(
                merged$ICPRB_CODE %in% k.ueq,
                merged$ResultMeasureValue / 1000 * 39,
                ifelse(
                  merged$ICPRB_CODE %in% na.ueq,
                  merged$ResultMeasureValue / 1000 * 23,
                  ifelse(
                    merged$ICPRB_CODE %in% so4.ueq,
                    merged$ResultMeasureValue / 1000 * 48,
                    merged$ResultMeasureValue
                )
              )
            )
          )
        )
      )
    )
  )
)
#==============================================================================
#==============================================================================
# Identify the conversion applied.
merged$ICPRB_CONVERSION <- ifelse(
  merged$ICPRB_CODE %in% mult.1000,
  "x * 1000",
  ifelse(
    merged$ICPRB_CODE %in% div.1000,
    "x / 1000",
    ifelse(
      merged$ICPRB_CODE %in% ca.ueq,
      "x / 1000 * 20",
      ifelse(
        merged$ICPRB_CODE %in% cl.ueq,
        "x / 1000 * 35.5",
        ifelse(
          merged$ICPRB_CODE %in% mg.ueq,
          "x / 1000 * 12",
          ifelse(
            merged$ICPRB_CODE %in% pp.p,
            "x / 1000 * 0.326",
            ifelse(
              merged$ICPRB_CODE %in% k.ueq,
              "x / 1000 * 39",
              ifelse(
                merged$ICPRB_CODE %in% na.ueq,
                "x / 1000 * 23",
                ifelse(
                  merged$ICPRB_CODE %in% so4.ueq,
                  "x / 1000 * 48",
                  NA
                )
              )
            )
          )
        )
      )
    )
  )
)
#==============================================================================
#setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/Merged/Column_Names")
setwd("H:/Projects/EPA3Trends/Data/Data_May2017/merged")
my.cols <- read.csv("column_names.csv", stringsAsFactors = FALSE)
final.df <- merged
add.cols <- my.cols[!my.cols$WQP.Columns %in% names(final.df), "ICPRB.Columns"]
add.cols <- add.cols[!add.cols %in% ""]
final.df[, add.cols] <- NA

sub.cols <- my.cols[my.cols$WQP.Columns %in% names(final.df), ]
sub.cols <- sub.cols[match(names(final.df), sub.cols$WQP.Columns),  ]
names(final.df) <- ifelse(names(final.df) %in% sub.cols$WQP.Columns, sub.cols$ICPRB.Columns, names(final.df))

icprb.cols <- my.cols$ICPRB.Columns[!my.cols$ICPRB.Columns %in% ""]
final.df <- final.df[, match(icprb.cols, names(final.df))]
#==============================================================================
# Remove samples collected prior to 1972.
# Helps to speed up app.
final.df <- final.df[final.df$DATE >= 1972, ]
#==============================================================================
setwd("H:/Projects/EPA3Trends/Data/Data_May2017/merged/output")
write.csv(final.df, paste0("prep_merge_wqp_", Sys.Date(),".csv"), row.names = FALSE)

#==============================================================================
################################# OLD SCRIPT ################################## 
# 2-01-2017
#==============================================================================
# Keep only the necessary columns.
keep.cols <- c("OrganizationIdentifier", "OrganizationFormalName",
               "ProviderName",
               "MonitoringLocationIdentifier", "MonitoringLocationName",
               "LatitudeMeasure", "LongitudeMeasure",
               "HorizontalCoordinateReferenceSystemDatumName",
               "ActivityStartDate", "ActivityDepthHeightMeasure.MeasureValue",
               "ActivityTypeCode",
               "ActivityDepthHeightMeasure.MeasureUnitCode",
               "ICPRB_NAME", "ResultMeasureValue", "UNITS")
keep.cols[!keep.cols %in% names(merged)]
final.df <- merged[, keep.cols]
#==============================================================================
# Convert all depths reported as feet to meters.
final.df$ActivityDepthHeightMeasure.MeasureValue <- 
  ifelse(
    final.df$ActivityDepthHeightMeasure.MeasureUnitCode %in% c("feet", "ft"),
    final.df$ActivityDepthHeightMeasure.MeasureValue * 0.3048,
    final.df$ActivityDepthHeightMeasure.MeasureValue
  )
# Remove the depth unit code column.
final.df <- final.df[, !names(final.df) %in% "ActivityDepthHeightMeasure.MeasureUnitCode"]
#==============================================================================
# Add columns for merging with Legacy STORET.
final.df$REPLICATE <- NA 
final.df$COMPOSITE <- NA
# Re-order the columns.
order.cols <- c("OrganizationIdentifier", "OrganizationFormalName",
                "ProviderName",
                "MonitoringLocationIdentifier", "MonitoringLocationName",
               "LatitudeMeasure", "LongitudeMeasure",
               "HorizontalCoordinateReferenceSystemDatumName",
               "ActivityStartDate", "ActivityDepthHeightMeasure.MeasureValue",
               "ActivityTypeCode",
               "REPLICATE", "COMPOSITE",
               "ICPRB_NAME", "ResultMeasureValue", "UNITS")
final.df <- final.df[, order.cols]
#==============================================================================
# Rename the columns to bind with Legacy STORET.
new.names <- c("AGENCY_CODE", "AGENCY", "PROVIDER", "SITE", "SITE_DESCRIPTION",
               "LATITUDE", "LONGITUDE", "HORIZONTAL_DATUM",
               "DATE", "DEPTH", "ACTIVITY_CODE", "REPLICATE", "COMPOSITE",
               "PARAMETER", "REPORTED_VALUE", "UNITS")
names(final.df) <- new.names
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Merge_Data/Output/1_26_2017")
write.csv(final.df, "prep_merge_wqp.csv", row.names = FALSE)
#==============================================================================

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Added MonitoringLocationName for Nagel
# 2/1/17
#==============================================================================
#==============================================================================
#==============================================================================
# Added MonitoringLocationName for Nagel
# 2/1/17
# Keep only the necessary columns.
keep.cols <- c("OrganizationIdentifier", "MonitoringLocationIdentifier",
               "MonitoringLocationName",
               "LatitudeMeasure", "LongitudeMeasure",
               "HorizontalCoordinateReferenceSystemDatumName",
               "ActivityStartDate", "ActivityDepthHeightMeasure.MeasureValue",
               "ActivityDepthHeightMeasure.MeasureUnitCode",
               "ICPRB_NAME", "ResultMeasureValue", "UNITS")
keep.cols[!keep.cols %in% names(merged)]
final.df <- merged[, keep.cols]
#==============================================================================
# Convert all depths reported as feet to meters.
final.df$ActivityDepthHeightMeasure.MeasureValue <- 
  ifelse(
    final.df$ActivityDepthHeightMeasure.MeasureUnitCode %in% c("feet", "ft"),
    final.df$ActivityDepthHeightMeasure.MeasureValue * 0.3048,
    final.df$ActivityDepthHeightMeasure.MeasureValue
  )
# Remove the depth unit code column.
final.df <- final.df[, !names(final.df) %in% "ActivityDepthHeightMeasure.MeasureUnitCode"]
#==============================================================================
# Add columns for merging with Legacy STORET.
final.df$REPLICATE <- NA 
final.df$COMPOSITE <- NA
# Re-order the columns.
order.cols <- c("OrganizationIdentifier", "MonitoringLocationIdentifier",
                "MonitoringLocationName",
                "LatitudeMeasure", "LongitudeMeasure",
                "HorizontalCoordinateReferenceSystemDatumName",
                "ActivityStartDate", "ActivityDepthHeightMeasure.MeasureValue",
                "REPLICATE", "COMPOSITE",
                "ICPRB_NAME", "ResultMeasureValue", "UNITS")
final.df <- final.df[, order.cols]
#==============================================================================
# Rename the columns to bind with Legacy STORET.
new.names <- c("AGENCY", "SITE",
               "SITE_NAME", 
               "LATITUDE", "LONGITUDE",
               "HORIZONTAL_DATUM",
               "DATE", "DEPTH", 
               "REPLICATE", "COMPOSITE",
               "PARAMETER", "REPORTED_VALUE", "UNITS")
names(final.df) <- new.names
#==============================================================================
setwd("//Pike/data/Projects/EPA3Trends/Data/Data_Feb2017/WQP/R_Output")
write.csv(final.df,
          paste0("prep_merge_wqp_", Sys.Date(), ".csv"),
          row.names = FALSE)
#==============================================================================
#==============================================================================
#==============================================================================








