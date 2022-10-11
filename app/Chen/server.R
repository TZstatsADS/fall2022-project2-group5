library(shiny)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

theme_set(theme_linedraw())

death <- read.csv('death_case.csv')
covid <- read.csv('covid_case.csv')
hospital <- read.csv('hospitalized_case.csv')

death<-death %>%
  mutate(date = zoo::as.yearmon(paste(death$year, death$month), "%Y %m"))%>%
  mutate(type = 'death')

covid<-covid %>%
  mutate(date = zoo::as.yearmon(paste(covid$year, covid$month), "%Y %m"))%>%
  mutate(type = 'covid')

hospital<-hospital %>%
  mutate(date = zoo::as.yearmon(paste(hospital$year, hospital$month), "%Y %m"))%>%
  mutate(type = 'hospitalizations')

total <- rbind(death, covid, hospital)

shinyServer(function(input, output) {
  output$plot=renderPlot({
    ggplot()+
      geom_line(data=total, aes_string(x='date', y=input$bor, color = 'type'))+
      ggtitle(paste('Number of Cases in',input$bor))+
      xlab('Date')+
      ylab('Monthly Cases')+
      theme(plot.title = element_text(size=15, face='bold'))+
      scale_y_continuous(labels = comma)
  })
})

