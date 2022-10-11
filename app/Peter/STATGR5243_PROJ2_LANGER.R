##STAT GR APPL DS PROJ 2
library(shiny)
library(plotly)
library(gganimate) 
library(ggplot2)
library(tidyverse)
library(httr)
library(rgdal)

#Loading and Cleaning Data
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


evictions = read.csv("/Users/peter/Downloads/eviction_clean.csv", stringsAsFactors = F, header = T)
evictions$dtm = as.Date(evictions$dtm)

covid = read.csv("/Users/peter/Downloads/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv", stringsAsFactors = F, header = T)
covid$date_of_interest = as.Date(covid$date_of_interest, format = "%m/%d/%Y")

# Define UI for miles per gallon app ----
ui = pageWithSidebar(
  
  # App title ----
  headerPanel("Covid-19 Cases and the Eviction Moratorium - New York City"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    dateRangeInput("dates", 
                   "Date range",
                   start = "2020-01-01", 
                   end = as.character(Sys.Date())),
    textOutput("DateRange"),
    
    selectInput("borough", "Borough:",
                c("Manhattan" = "MANHATTAN",
                  "Brooklyn" = "BROOKLYN",
                  "Queens" = "QUEENS",
                  "Bronx" = "BRONX",
                  "Staten Island" = "STATEN ISLAND"))
    
    # Input: Selector for variable to plot against mpg ----
    #selectInput("variable", "Variable:", 
     #           c("Cylinders" = "cyl",
      #            "Transmission" = "am",
       #           "Gears" = "gear")),
    
    # Input: Checkbox for whether outliers should be included ----
    #checkboxInput("outliers", "Show outliers", TRUE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotlyOutput("plot1"
    ),
    verbatimTextOutput("info"),
    plotlyOutput("plot2")
  )
)
# Define server logic to plot various variables against mpg ----
server = function(input, output) {
  output$DateRange = renderText({
    # make sure end date later than start date
    validate(
      need(input$dates[2] > input$dates[1], "end date is earlier than start date"
      )
    )
    
    # make sure greater than 2 week difference
    validate(
      need(difftime(input$dates[2], input$dates[1], "days") > 60, "date range less the 60 days"
      )
    )
    
    paste("Your date range is", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })
  output$plot1 = renderPlotly({
    evictions_c = evictions %>% filter(BOROUGH == input$borough[1]) %>% filter(dtm >= input$dates[1] && dtm <= input$dates[2])
    g = ggplot() + geom_polygon(data = nyc_neighborhoods, aes(x=long, y=lat, group=group)) + 
           geom_point(data = evictions_c, aes(x = Longitude , y = Latitude, color = BOROUGH))
    #g = ggplotly(g,height = 300, width = 700)
    
    
  })
  
  output$plot2 = renderPlotly({
    covid_c = covid %>% filter(date_of_interest >= input$dates[1] && date_of_interest <= input$dates[2])
    if(input$borough[1] == "MANHATTAN"){
      covid_c$target1 = covid_c$MN_CASE_COUNT
    }
    else if(input$borough[1] == "BROOKLYN"){
      covid_c$target1 = covid_c$BK_CASE_COUNT
      }
    else if(input$borough[1] == "BRONX"){
      covid_c$target1 = covid_c$BX_CASE_COUNT
    }
    else if(input$borough[1] == "QUEENS"){
      covid_c$target1 = covid_c$QN_CASE_COUNT
    }
    else{
      covid_c$target1 = covid_c$SI_CASE_COUNT
    }
    #covid_c$target1 = covid_c$MN_CASE_COUNT
    f = ggplot() + geom_line(data = covid_c, aes(x = date_of_interest , y = target1)) +
      scale_x_date(date_breaks = "1 month", 
                   limits = c(input$dates[1],input$dates[2])) + theme(axis.text.x = element_text(angle = 90))
    #f = ggplotly(f,height = 300, width = 570)
   
   })
}

shinyApp(ui, server)


