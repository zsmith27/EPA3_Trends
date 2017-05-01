
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(png)
library(DT)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
#wqt <- read.csv("./data/wqt.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {

  
  param.tbl <- reactive({
    conn <- dbConnect("PostgreSQL",
                      user = "Guest",
                      password = "Guest",
                      dbname = "WQT2",
                      host = "localhost",
                      port = 5432)
    on.exit(dbDisconnect(conn), add = TRUE)
    wqt <- dbReadTable(conn, input$SITE)
  })
  
  
  output$tbl <- renderTable({
    final.df <- param.tbl()
    final.df <- final.df[final.df$PARAMETER %in% input$PARAM, ]
    return(final.df)
  })
  

 #============================================================================= 
  # Update the list of parameters to reflect only the parameters observed at
  # the selected site.
  param.react <- reactive({
    if (is.null(input$SITE)) {
      return(NULL)
    }    
    sites <- param.tbl()
    final.df <- sites[sites$SITE %in% input$SITE, ]
    #final.df <- wqt[wqt$SITE %in% input$SITE, ]
    final.vec <- unique(final.df$PARAMETER)
    return(final.vec)
  })
  #============================================================================= 
  # Apply the updated list of parameters to the dropdown menu.
  observeEvent(c(input$SITE), {
    sub.param <- param.react()
    
    final.param <- unique(sort(as.character(sub.param)))
    
    updateSelectInput(session, "PARAM",
                      choices = as.character(final.param))
  })
  #============================================================================= 
  

  #============================================================================= 
  prep.react <- reactive({
    site.char <- as.character(unique(input$SITE)[1])
    param.char <- as.character(unique(input$PARAM)[1])
    final.df <- prep_plot(param.tbl(), site.char, param.char)
    #return(final.df)
  })
  output$this <- renderDataTable({
    test <- prep.react()
    datatable(test, options = list(
      paging = FALSE, color = "black"))
    
  })
  # Import the gradient tile tables.
  output$TILE <- renderPlot({
    prep.df <- prep.react()
    tile_plot(prep.df, param.range)
  })
  
  
  # Import the Loess figures.
  output$LOESS <- renderPlot({
    prep.df <- prep.react()
    loess_plot(prep.df)
    #setwd("C:/Users/zsmith/Desktop/WQ_Trends/Output/Loess")
    #ggsave("LOESS_TEMP_PLOT.png", plot = new.plot, width = 13, height = 3, dpi = 120)
    #png::readPNG("LOESS_TEMP_PLOT.png")
  })
  #============================================================================= 
  # Exract Lat/Long of selected site for plotting.
  points <- eventReactive(input$SITE, {
    sites <- param.tbl()
    sites[sites$SITE %in% input$SITE, c("LATITUDE", "LONGITUDE")]
  }, ignoreNULL = FALSE)
  
  # Exract Lat/Long of selected site for plotting.
  points.gage <- eventReactive(input$SITE, {
    sites <- param.tbl()
    sites <- sites[sites$SITE %in% input$SITE, c("LAT_DD", "LONG_DD")]
    final.df <- data.frame(lapply(sites, as.numeric))
    return(final.df)
  }, ignoreNULL = FALSE)
  #============================================================================= 
  # Plot the Site on the map.
  output$mymap <- renderLeaflet({
    longitude <- mean(points()$LONGITUDE, na.rm = TRUE)
    latitude <- mean(points()$LATITUDE, na.rm = TRUE)
    long.gage <- mean(points.gage()$LONG_DD, na.rm = TRUE)
    lat.gage <- mean(points.gage()$LAT_DD, na.rm = TRUE)
    leaflet() %>%
      setView(lng = longitude, lat = latitude, zoom = 16) %>% 
      #addTiles() %>%
      addProviderTiles("Hydda.Full",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data = points(),
                       fillColor = "#E69F00",
                       stroke = FALSE,
                       color = "black",
                       weight = 3,
                      fillOpacity = 0.5) #%>%
      #addMarkers(data = points.gage(), lng = long.gage, lat = lat.gage)
  })
  #============================================================================= 
  # Subset the data to only represent the selected Site and Parameter.
  input.react <- reactive({
    sites <- param.tbl()
    #final.df <- wqt[wqt$SITE %in% input$SITE, ]
    final.df <- sites[sites$SITE %in% input$SITE, ]
    final.df <- unique(final.df[final.df$PARAMETER %in% input$PARAM, ])
    final.df <- final.df[order(final.df$DATE), ]
    return(final.df)
  })
  #============================================================================= 
  # Generate a table representing the selected Site and Parameter.
  output$param_table <- renderDataTable({
    sel.param <- input$PARAM
    
    if (is.null(sel.param))
      return(NULL)
    
    datatable(input.react(), options = list(
      paging = FALSE, color = "black")) %>%
      formatDate(columns = "DATE", method = 'toLocaleDateString')
    
  }, options = list(
    paging = FALSE, color = "black"))
  #============================================================================= 

})
