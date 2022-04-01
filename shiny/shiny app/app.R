#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)     # for data cleaning and plotting
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(gplots)        # for col2hex() function
library(sf)            # for working with spatial data
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gifski)        # need the library for creating gifs but don't need to load each time
library(zoo)

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
    separate(state, into = c("dot","state"), extra = "merge") %>% 
    select(-dot) %>% 
    mutate(state = str_to_lower(state))
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid<-covid19 %>% 
    group_by(state) %>% 
    mutate(state=str_to_lower(state))
covid_pop <-covid %>% 
    left_join(census_pop_est_2018,
              by = c("state" = "state")) %>% 
    mutate(covid_per_10000 = (cases/est_pop_2018)*10000)


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
                 sliderInput(inputId = "date", 
                            label = "Time Range",
                            min = min(covid_pop$date),
                            max = max(covid_pop$date),
                            value = c(min(covid_pop$date),max(covid_pop$date))),
                 selectInput("state", 
                             "States", 
                             choices = covid_pop %>%
                             arrange(state) %>%
                             distinct(state)%>%
                             pull(state),
                             multiple = TRUE),    
                  submitButton(text = "Create my plot!")
                 ),
        
         mainPanel(
             plotOutput(outputId = "timeplot")
             )
        )
)

server <- function(input, output){
    output$timeplot <- renderPlot({
        covid_pop%>%
            filter(state%in%input$state) %>%
            ggplot(aes(y=covid_per_10000,x=date,color=state)) +
            geom_line()+
            labs(title="Most recent proportion of cumulative number\nin 10000 people number of COVID-19 cases",
                 subtitle = input$date,
                 y="daily cases per 10,000 people",
                 fill="cases\nper 10000 people")+
            theme(plot.title = element_text(hjust = 0.5,colour = "black", face = "bold",
                                            size = 14, vjust = 1))
    })
}

shinyApp(ui = ui, server = server)
