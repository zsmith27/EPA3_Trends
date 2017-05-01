

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(leaflet)
library(rsconnect)
library(DT)

shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("EPA Region 3 Water Quality Trends"),
  sidebarLayout(
    #==========================================================================
    sidebarPanel(
      width = 2,
      #selectInput("SITE", "Site", unique(sites$SITE)),
      selectInput("SITE", "Site", choices = unique(sites), selected = FALSE),
      selectInput("PARAM", "Parameter", "DO")
      #selectInput("PARAM", "Parameter", unique(sites$PARAMETER))
    ),
    #==========================================================================
    mainPanel(tabsetPanel(
      #==========================================================================
      tabPanel(
        "Figures",
        fluidRow(plotOutput("TILE", height = 400, width = 1530)),
        fluidRow(plotOutput("LOESS", height = 400, width = 1530)),
        fluidRow(column(
          12, leafletOutput("mymap", height = 500, width = 1530), p()
        ))
      ),
      #==========================================================================
      tabPanel("Data",
               #tableOutput('tbl'))
               dataTableOutput('param_table'))
    ))
    #==========================================================================
  )
  #==========================================================================
  
))
