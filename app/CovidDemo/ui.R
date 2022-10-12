
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


#JENNY DATA
eviction = as.data.frame(read.csv('evictionJenny.csv', stringsAsFactors = F, header = T))

#Loading and Cleaning Data
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

eviction_df <- read.csv("eviction_df.csv")

zip_info <- read.csv("uszips.csv")
zip_info = zip_info[,1:3]
colnames(zip_info) = c("region","LAT","LON")

#pre act:
eviction_pre <- eviction_df[difftime(eviction_df$dtm,"2020-03-07")<0,]
#during act:
eviction_post <- eviction_df[(difftime(eviction_df$dtm,"2020-03-07")>=0 & difftime(eviction_df$dtm,"2022-01-15")<=0),]


evictions = read.csv("eviction_clean.csv", stringsAsFactors = F, header = T)
evictions$dtm = as.Date(evictions$dtm)

covid = read.csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv", stringsAsFactors = F, header = T)
covid$date_of_interest = as.Date(covid$date_of_interest, format = "%m/%d/%Y")

#
death <- read.csv('death_case.csv')
covid_2 <- read.csv('covid_case.csv')
hospital <- read.csv('hospitalized_case.csv')

death<-death %>%
  mutate(date = zoo::as.yearmon(paste(death$year, death$month), "%Y %m"))%>%
  mutate(type = 'death')

covid_2<-covid_2 %>%
  mutate(date = zoo::as.yearmon(paste(covid_2$year, covid_2$month), "%Y %m"))%>%
  mutate(type = 'covid')

hospital<-hospital %>%
  mutate(date = zoo::as.yearmon(paste(hospital$year, hospital$month), "%Y %m"))%>%
  mutate(type = 'hospitalizations')

total <- rbind(death, covid_2, hospital)

#Shapefilework

ny_shapefile = sf::st_read('ZIP_CODE_040114.shp')
demo = as.data.frame(read.csv('Demographic_Statistics_By_Zip_Code.csv'), stringsAsFactors = F, header = T)
demo$perc_minority = ifelse(demo$COUNT.ETHNICITY.TOTAL == 0,0,1-(demo$PERCENT.WHITE.NON.HISPANIC))
demo$perc_non_citizen = ifelse(demo$COUNT.CITIZEN.STATUS.TOTAL == 0,0,1-(demo$PERCENT.US.CITIZEN))
demo$public_assist = ifelse(demo$PERCENT.PUBLIC.ASSISTANCE.TOTAL == 0,0,1-(demo$PERCENT.RECEIVES.PUBLIC.ASSISTANCE))
colnames(demo)[1] = "zip"
demo$zip = as.character(demo$zip)
demo = demo %>% select(zip,perc_minority, perc_non_citizen,public_assist)
#shape_demo = sp::merge(ny_shapefile, demo, by.x ="ZIPCODE", by.y = "zip", duplicateGeoms = TRUE,no.dups = FALSE)
shape_demo = ny_shapefile %>% left_join(demo,by = c("ZIPCODE" = "zip"))


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
                           plotOutput("plot4"),
                           plotOutput("plot6"),
                           plotOutput("plot7"))
                         
                         
                ))