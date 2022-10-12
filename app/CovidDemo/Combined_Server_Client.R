##STAT GR APPL DS PROJ 2

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

ny_shapefile = sf::st_read('ZIP_CODE_040114/ZIP_CODE_040114.shp')
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


# Define server logic to plot various variables against mpg ----
server = function(input, output){
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
    evictions_c = evictions %>% filter(BOROUGH == input$borough1[1]) %>% filter(dtm >= input$dates[1] && dtm <= input$dates[2])
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
  
  output$plot3=renderPlot({
    if (input$borough == "MANHATTAN"){
      bor = "Manhattan"
    }
    else if (input$borough == "BROOKLYN"){
      bor = "Brooklyn"
    }
    else if (input$borough == "QUEENS"){
      bor = "Queens"
    }
    else if (input$borough == "BRONX"){
      bor = "Bronx"
    }
    else{
      bor = "Staten_Island"
    }
    ggplot()+
      geom_line(data=total, aes_string(x='date', y=bor, color = 'type'))+
      ggtitle(paste('Number of Cases in',bor))+
      xlab('Date')+
      ylab('Monthly Cases')+
      theme(plot.title = element_text(size=15, face='bold'))#+
    #scale_y_continuous(labels = comma)
  })
  
  output$plot4 = renderPlot({
    ggplot() +
      geom_sf() +
      geom_sf(data = shape_demo$geometry, aes(fill = shape_demo$perc_non_citizen)) +
      ggtitle("City-wide Demographics") + labs(fill = "Percentage Non-US Citizens")
  })
  
  output$plot6 = renderPlot({
    ggplot() +
      geom_sf() +
      geom_sf(data = shape_demo$geometry, aes(fill = shape_demo$perc_minority)) +
      ggtitle("City-wide Demographics") + labs(fill = "Percentage Minorities")
  })
  
  output$plot7 = renderPlot({
    ggplot() +
      geom_sf() +
      geom_sf(data = shape_demo$geometry, aes(fill = shape_demo$public_assist)) +
      ggtitle("City-wide Demographics") + labs(fill = "Percentage on Public Assistance")
  })
  
  output$left <- renderLeaflet({
    if (input$borough== "ALL BOROUGHS"){
      leafplt_df <- eviction_pre%>%
        group_by(region)%>%
        summarise(residential_perc = sum(Residential.Commercial == "Residential")/n(),
                  commercial_perc= sum(Residential.Commercial == "Commercial")/n(),
                  Eviction_cnt = sum(Eviction.Legal.Possession == "Eviction"),
                  Possesion_cnt = sum(Eviction.Legal.Possession =="Possession"))%>%
        left_join(zip_info,by = "region")
      
    }
    else{
      leafplt_df <- eviction_pre%>%
        filter(BOROUGH==input$borough)%>%
        group_by(region)%>%
        summarise(residential_perc = sum(Residential.Commercial == "Residential")/n(),
                  commercial_perc = sum(Residential.Commercial == "Commercial")/n(),
                  Eviction_cnt= sum(Eviction.Legal.Possession == "Eviction"),
                  Possesion_cnt = sum(Eviction.Legal.Possession =="Possession"))%>%
        left_join(zip_info,by = "region")
      
    }
    map_pre <- leafplt_df%>%
      leaflet(options = leafletOptions(minZoom = 11,maxZoom=13))%>%
      addTiles()%>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE))%>%
      setView(-73.9834,40.7504,zoom = 12)
    if(input$score == "Residential Rate" ){
      map_pre%>%addHeatmap(
        lng = ~LON,
        lat = ~ LAT,
        intensity = ~residential_perc,
        max = 0.1,
        radius = 10,
        blur = 10
      )}
    else if(input$score == "Commercial Rate"){
      map_pre%>%addHeatmap(
        lng = ~LON,
        lat = ~ LAT,
        intensity = ~commercial_perc,
        max = 1,
        radius = 10,
        blur = 10)}
    else if(input$score == "Eviction Count"){
      map_pre%>%addHeatmap(
        lng = ~LON,
        lat = ~ LAT,
        intensity = ~Eviction_cnt,
        max = 200,
        radius = 10,
        blur = 10)
    }
    else if (input$score=="Legal Possesion Count"){
      map_pre%>%addHeatmap(
        lng = ~LON,
        lat = ~ LAT,
        intensity = ~Possesion_cnt,
        max = 500,
        radius = 10,
        blur = 10)
    }
  }
  )
  
  output$right<- renderLeaflet(
    {
      if (input$borough== "ALL BOROUGHS"){
        leafplt_df <- eviction_post%>%
          group_by(region)%>%
          summarise(residential_perc = sum(Residential.Commercial == "Residential")/n(),
                    commercial_perc= sum(Residential.Commercial == "Commercial")/n(),
                    Eviction_cnt = sum(Eviction.Legal.Possession == "Eviction"),
                    Possesion_cnt = sum(Eviction.Legal.Possession =="Possession"))%>%
          left_join(zip_info,by = "region")
        
      }
      else{
        leafplt_df <- eviction_post%>%
          filter(BOROUGH==input$borough)%>%
          group_by(region)%>%
          summarise(residential_perc = sum(Residential.Commercial == "Residential")/n(),
                    commercial_perc = sum(Residential.Commercial == "Commercial")/n(),
                    Eviction_cnt= sum(Eviction.Legal.Possession == "Eviction"),
                    Possesion_cnt = sum(Eviction.Legal.Possession =="Possession"))%>%
          left_join(zip_info,by = "region")
        
      }
      map_post <- leafplt_df%>%
        leaflet(options = leafletOptions(minZoom = 11,maxZoom=13))%>%
        addTiles()%>%
        addProviderTiles("CartoDB.Positron",
                         options = providerTileOptions(noWrap = TRUE))%>%
        setView(-73.9834,40.7504,zoom = 12)
      if(input$score == "Residential Rate" ){
        map_post%>%addHeatmap(
          lng = ~LON,
          lat = ~ LAT,
          intensity = ~residential_perc,
          max = 1,
          radius = 10,
          blur = 10
        )}
      else if(input$score == "Commercial Rate"){
        map_post%>%addHeatmap(
          lng = ~LON,
          lat = ~ LAT,
          intensity = ~commercial_perc,
          max = 1,
          radius = 10,
          blur = 10)}
      else if(input$score == "Eviction Count"){
        map_post%>%addHeatmap(
          lng = ~LON,
          lat = ~ LAT,
          intensity = ~Eviction_cnt,
          max = 200,
          radius = 10,
          blur = 10)
      }
      else if (input$score=="Legal Possesion Count"){
        map_post%>%addHeatmap(
          lng = ~LON,
          lat = ~ LAT,
          intensity = ~Possesion_cnt,
          max = 500,
          radius = 10,
          blur = 10)
      }
    })
  
  data <- reactive({
    data <- eviction %>% filter(grepl(input$years, year))
    return(data)
  })
  
  output$Plot5 = renderPlot({
    data = data()
    ggplot() +
      geom_line(data=data, aes_string(x='month', y=input$borough)) +
      scale_x_continuous("month", data$month, labels=as.character(data$month), breaks=data$month) + 
      ylab('monthly number of evictions') +
      ggtitle(paste("Number of eviction cases in ",  input$borough, " in ", input$years))
    
  })
  
  
  
}

shinyApp(ui, server)


