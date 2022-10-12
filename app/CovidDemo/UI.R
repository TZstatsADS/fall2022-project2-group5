if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}

if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}

if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}

if (!require("rsconnect")) {
  install.packages("rsconnect")
  library(rsconnect)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}


if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}

if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}

if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}

if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require("httr")) {
  install.packages("httr")
  library(httr)
}

if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}

if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}

if (!require("sf")) {
  install.packages("sf")
  library(sf)
}


# Define UI for miles per gallon app ----
ui = navbarPage("Covid-19 Cases, Evictions and Demographics Data - New York City",
                tabPanel("Evictions By Borough",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("borough1", "Borough:",
                                         c("Manhattan" = "MANHATTAN",
                                           "Brooklyn" = "BROOKLYN",
                                           "Queens" = "QUEENS",
                                           "Bronx" = "BRONX",
                                           "Staten Island" = "STATEN ISLAND")),
                             
                             dateRangeInput("dates", 
                                            "Date range",
                                            start = "2020-01-01", 
                                            end = as.character(Sys.Date())),
                             textOutput("DateRange")
                             
                             
                             
                           ),
                           
                           mainPanel(
                             plotlyOutput("plot1")
                             #plotOutput("Plot5"))
                           ))),              
                
                
                tabPanel("Covid Vs. Evictions By Borough and Time",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("borough", "Borough:",
                                         c("Manhattan" = "MANHATTAN",
                                           "Brooklyn" = "BROOKLYN",
                                           "Queens" = "QUEENS",
                                           "Bronx" = "BRONX",
                                           "Staten Island" = "STATEN ISLAND")),
                             
                             sliderInput("years", 
                                         label = "Input Year for Covid Cases", min = 2017, value = 2021, max = 2022)
                             
                             
                           ),
                           
                           mainPanel(
                             plotOutput("plot3"),
                             plotOutput("Plot5"))
                         )),
                
                tabPanel("Zip Code Level Demographic Mapping",
                         mainPanel(
                           #plotlyOutput("plot2"),
                           #plotOutput("plot4"),
                          # plotOutput("plot6"),
                          # plotOutput("plot7"))
                         
                         )
                ))