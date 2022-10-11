#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(leaflet.extras)
library(dplyr)
library(magrittr)
library(mapview)
library(leafsync)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
   # Application title
    title = "NYC Eviction Study: Pre Act vs Post Act",
    theme = shinytheme("cosmo"),
    tabPanel("Maps",
             icon = icon("map-marker-alt"),
             div(class = "outer",
                 fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                      leafletOutput("left",width = "100%",height = 1200),
                                      leafletOutput("right",width = "100%",height = 1200))),
                 absolutePanel(id = "control",class = "panel panel-default",foxed = TRUE, draggable = TRUE,
                               top = 200, left = 40, right = "auto", bottom = "auto", width = 250, height = "auto",
                               tags$h4("NYC Eviction Comparison"),
                               tags$br(),
                               tags$h5("Left:Pre-Act VS Right:Post-Act"),
                               prettyRadioButtons(
                                 inputId = "score",
                                 label = "Eviction Types Choices",
                                 choices = c("Residential Rate",
                                             "Commercial Rate",
                                             "Eviction Count",
                                             "Legal Possesion Count"),
                                 inline = TRUE,
                                 status = "danger",
                                 fill = TRUE),
                  awesomeRadio("borough",
                               label = "Borough Choices",
                               choices = c("ALL BOROUGHS",
                                           "MANHATTAN",
                                           "BRONX",
                                           "BROOKLYN",
                                           "QUEENS",
                                           "STATEN ISLAND"),
                               selected = "ALL BOROUGHS",
                               status = "warning"),
                  style = "opacity :0.8"
                               ),
                 )
             )

))
