# Aqui é só manipulação dos dados pra ficar no formato certo
pacman::p_load(ggplot2, tidyverse, tidyr, dplyr, lubridate, stringr,broom,shinythemes)
dados <- read.csv2("amazon.csv")
dados$number <- as.vector(dados$number)
dados$number <- as.numeric(dados$number)
dados$X <- NULL
dados$X.1 <- NULL
dados <- dados %>%
  mutate(date = unite(dados,year_month,month,year,sep = "-")$year_month)
dados <- dados %>%
  mutate(dias = rep("01",length(dados$date)))
dados <- unite(dados,data,date,dias,sep = "-")
dados$data <- myd(dados$data)

# Aqui começa o shiny
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(    
  shinythemes::themeSelector(),
  # Give the page a title
  titlePanel("Queimadas no Brasil"),
  dateRangeInput('dateRange',
                 label = 'Periodo analizado',
                 start = Sys.Date() - 2, end = Sys.Date() + 2
  ),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("mes", "Mes:", 
                  choices=c("Todos","Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")),
      selectInput("estado","Estado:",
                  choices = c("Todos","Acre","Alagoas","Amapa","Amazonas","Bahia",
                              "Ceara","Distrito Federal","Espirito Santo","Goias",
                              "Maranhao","Mato Grosso","Minas Gerais","Paraiba",
                              "Pernambuco","Piau","Rio","Rondonia","Roraima")),
      hr(),
      helpText("Dados fornecidos em 
               http://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif")
      ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("Grafico")  
    )
    
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$Grafico <- renderPlot({
    if(input$mes == "Todos"){
      if(input$estado == "Todos"){
        ggplot(dados%>%
                 filter(data>=input$dateRange[1])%>%
                 filter(data<=input$dateRange[2]),
                        aes(x=data,y=number)) +
          geom_point(colour="red") +
          geom_jitter(width = 10,height = 0.1,color="red") +
          geom_smooth(method = "lm",formula = y~x,se = T,color="black")+
          xlab("Anos") +
          ylab("Numero de queimadas")+
          theme_classic() +
          ggtitle("Numero de queimadas por ano")
      }else{
      ggplot(dados %>%
               filter(state == input$estado)%>%
               filter(data>=input$dateRange[1])%>%
               filter(data<=input$dateRange[2]),aes(x=data,y=number)) +
        geom_point(colour="red") +
        geom_jitter(width = 10,height = 0.1,color="red") +
        geom_smooth(method = "lm",formula = y~x,se = F,color="black")+
        xlab("Anos") +
        ylab("Numero de queimadas")+
        theme_classic() +
        ggtitle("Numero de queimadas por ano")}}else{
          if(input$estado == "Todos"){
            ggplot(dados %>%
                     filter(month == input$mes)%>%
                     filter(data>=input$dateRange[1])%>%
                     filter(data<=input$dateRange[2]),aes(x=data,y=number)) +
              geom_point(colour="red") +
              geom_jitter(width = 10,height = 0.1,color="red") +
              geom_smooth(method = "lm",formula = y~x,se = F,color="black")+
              xlab("Anos") +
              ylab("Numero de queimadas")+
              theme_classic() +
              ggtitle("Numero de queimadas por ano")
          }else{
    # Render a barplot
    ggplot(dados %>%
             filter(month == input$mes)%>%
             filter(state == input$estado)%>%
             filter(data>=input$dateRange[1])%>%
             filter(data<=input$dateRange[2]),aes(x=data,y=number)) +
      geom_point(colour="red") +
      geom_jitter(width = 10,height = 0.1,color="red") +
      geom_smooth(method = "lm",formula = y~x,se = F,color="black")+
      xlab("Anos") +
      ylab("Numero de queimadas")+
      theme_classic() +
      ggtitle("Numero de queimadas por ano")
  }}})
}
# Run the application 
shinyApp(ui = ui, server = server)
