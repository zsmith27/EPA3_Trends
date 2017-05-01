library(leaflet)
test <- all.wqp[!is.na(all.wqp$LatitudeMeasure), ]
test$LongitudeMeasure <- ifelse(test$LongitudeMeasure > 0, test$LongitudeMeasure * -1, test$LongitudeMeasure)
test <- test[test$LatitudeMeasure > 10, ]
leaflet(data = test) %>% addTiles() %>%
  addCircleMarkers(~LongitudeMeasure, ~LatitudeMeasure, popup = ~as.character(MonitoringLocationIdentifier))

test <- unique(all.wqp[, c("LongitudeMeasure", "LatitudeMeasure")])
test$LongitudeMeasure <- round(test$LongitudeMeasure, 4)
test$LatitudeMeasure <- round(test$LatitudeMeasure, 4)

