shinyUI(
  fluidPage(
    titlePanel('COVID-19 Daily Counts of Cases, Hospitalizations, and Deaths'),
    sidebarPanel(
      p('Daily count of NYC residents who tested positive for SARS-CoV-2, 
        who were hospitalized with COVID-19, and deaths among COVID-19 patients'),
      br(),
      selectInput("bor", label = "Choose a Borough",
                  choices = c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten_Island'), selected = 'Manhattan')
    ),
    mainPanel(
      plotOutput('plot')
    )
  ))

