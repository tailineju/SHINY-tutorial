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

# Definir o UI
ui <- fluidPage(    
  shinythemes::themeSelector(),
  # Dar um titulo
  titlePanel("Queimadas no Brasil"),
  # Gerar a opcao de escolha das datas
  dateRangeInput('dateRange',
                 label = 'Periodo analizado',
                 start = Sys.Date() - 2, end = Sys.Date() + 2
  ),
  
  # Gerar a opcao de escolha dos meses e estados
  sidebarLayout(      
    sidebarPanel(
      # Gerar as opcoes de meses
      selectInput("mes", "Mes:", 
                  choices=c("Todos","Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")),
      # Gerar as opcoes de estados
      selectInput("estado","Estado:",
                  choices = c("Todos","Acre","Alagoas","Amapa","Amazonas","Bahia",
                              "Ceara","Distrito Federal","Espirito Santo","Goias",
                              "Maranhao","Mato Grosso","Minas Gerais","Paraiba",
                              "Pernambuco","Piau","Rio","Rondonia","Roraima")),
      hr(),
      helpText("Dados fornecidos em 
               http://dados.gov.br/dataset/sistema-nacional-de-informacoes-florestais-snif")
      ),
    
    # Criar um espaco para o grafico
    mainPanel(
      plotOutput("Grafico")  
    )
    
    )
)
# Definir o server
server <- function(input, output) {
  
  # Preencher o espaco criado com o grafico
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
# Rodar o shiny
shinyApp(ui = ui, server = server)
