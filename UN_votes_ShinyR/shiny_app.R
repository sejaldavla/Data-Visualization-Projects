# Required packages
library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)

# Import data
load("~/Desktop/Data_Portfolio/UN_data/UNVotes.RData")
glimpse(completeVotes)

# Data cleaning
Countrywise_vote <- completeVotes |>
  select(vote,Countryname, year) |>
  na.omit() |>
  group_by(Countryname, year, vote) |>
  summarize(total = n(), .groups = "drop") |>
  mutate(votetype = 
           case_when(
             vote == 1 ~ "yes",
             vote == 2 ~ "abstain",
             vote == 3 ~ "no",
             vote == 8 ~ "not participating",
             vote == 9 ~ "not eligible"
           )) |>
  rename("country" = "Countryname")


# Create a shiny app

ui <- fluidPage(
  titlePanel("How the World votes at the UN?"),
  theme = shinythemes::shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country",
                  "Select country",
                  selected = "Canada",
                  choices = unique(Countrywise_vote$country)),
      sliderInput("year",
                  "Select year",
                  min = 1946,
                  max = 2021,
                  value = 2000)
    ),
      mainPanel(
        tabsetPanel(
          tabPanel('plot', plotOutput('plot')),
          tabPanel('table', DT::DTOutput('table'))
        )
      ) 
    )
  )  

  

server <- function(input, output, session){
  output[['plot']] <- renderPlot({
    UN_country_year <- Countrywise_vote |>
      filter(country == input$country) |>
      filter(year == input$year) 
    print(UN_country_year)
    print(ggplot(UN_country_year, aes(x = factor(vote), y = total)) +
      geom_bar(stat = "identity",
               fill = "#4CC9F0",
               alpha = 0.7) +
        labs(x = "Vote type",
             y = "Number of votes",
             title = "Voting in the United Nations General Assembly and \nthe Security Council resolutions") +
      theme_classic() +
        theme(plot.title = element_text(size = 20,
                                        margin = margin(0,0,10,0)),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 15)))
    
  output$table <- DT::renderDT({
    Countrywise_vote |>
      filter(country == input$country) |>
      filter(year == input$year) 
  })
  })
}  
  

shinyApp(ui = ui, server = server)




