#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(magrittr)
library(mapview)
library(leafsync)


# DATA processing

setwd("~/Desktop/project2")
eviction_df <- read.csv("output/eviction_df.csv")
zip_info <- read.csv("output/zip_info.csv")
#pre act:
eviction_pre <- eviction_df[difftime(eviction_df$dtm,"2020-03-07")<0,]
#during act:
eviction_post <- eviction_df[(difftime(eviction_df$dtm,"2020-03-07")>=0 & difftime(eviction_df$dtm,"2022-01-15")<=0),]

shinyServer(function(input,output){
  output$left <- renderLeaflet(
    {
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
})



