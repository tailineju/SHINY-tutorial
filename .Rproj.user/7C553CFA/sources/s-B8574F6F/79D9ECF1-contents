## INCOMPLETOOOO

install.packages("tidyverse")
library(shiny)gg
library(plotly)
library(tidyverse)
  

df <- read.csv("file:///C:/Users/apoio/Desktop/WorldCups.csv")

  
  
ui <- fluidPage(
  
  # Application title
  titlePanel("Copa do Mundo"),
  
  sidebarPanel(
    h3("Ideal Points Estimation"),
    # Select Justices name here
    selectizeInput("name",
                   label = "Country Name(s) of Interest",
                   choices = (df$Winner),
                   multiple = T,
                   options = list(maxItems = 5, placeholder = 'Select a name'),
                   selected = "United States of America"),
    # Term plot
    plotOutput("termPlot", height = 200),
    helpText("Data: Bailey, Michael, Anton  Strezhnev and Erik Voeten. Forthcoming.  'Estimating Dynamic State Preferences from United Nations Voting Data.' Journal of Conflict Resolution. ")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("trendPlot")
  )
)

server <- function(input, output, session) {
  output$trendPlot <- renderPlot({
    
    if (length(input$Winner) == 0) {
      print("Please select at least one country")
    } else {
      df_trend <- df[df$Winner == input$Winner, ]
      ggplot(df_trend) +
        geom_line(aes(x = Year, by = Winner, color = Winner)) +
        labs(x = "Year", y = "Ideology", title = "Ideal Points for Countries") +
        scale_colour_hue("clarity", l = 70, c = 150) + ggthemes::theme_few()
    }
    
  })
}

tamanho <- length()

df_trend <- df[df$Winner == df$Winner, ]
ggplot(df_trend) +
  geom_line(aes(x = Year, y = GoalsScored, by = Winner, color = Winner)) +
  labs(x = "Year", y = "Ideology", title = "Ideal Points for Countries") +
  scale_colour_hue("clarity", l = 70, c = 150) 

shinyApp(ui = ui, server = server)
