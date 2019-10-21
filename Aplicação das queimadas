# Aqui é só manipulação dos dados pra ficar no formato certo
pacman::p_load(ggplot2, tidyverse, tidyr, dplyr, lubridate, stringr,broom)
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

### Definir UI para a aplicacao
ui <- fluidPage(    
  
  # Titulo
  titlePanel("Queimadas no Brasil"),
  
  # Gerar a tabela de escolha dos meses e estados
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
    
    # Criar um espaco para o grafico
    mainPanel(
      plotOutput("Grafico")  
    )
    
    )
)
# Definir o server
server <- function(input, output) {
  
  # preencher o espaço criado com o grafico
  output$Grafico <- renderPlot({
    if(input$mes == "Todos"){
      if(input$estado == "Todos"){
        ggplot(dados,aes(x=data,y=number)) +
          geom_point() +
          geom_jitter(width = 20,height = 0.2) +
          geom_smooth(method = "lm",formula = y~x,se = F)+
          xlab("Anos") +
          ylab("Numero de queimadas")+
          theme_classic() +
          ggtitle("Numero de queimadas por ano")
      }else{
      ggplot(dados %>%
               filter(state == input$estado),aes(x=data,y=number)) +
        geom_point() +
        geom_jitter(width = 20,height = 0.2) +
        geom_smooth(method = "lm",formula = y~x,se = F)+
        xlab("Anos") +
        ylab("Numero de queimadas")+
        theme_classic() +
        ggtitle("Numero de queimadas por ano")}}else{
          if(input$estado == "Todos"){
            ggplot(dados %>%
                     filter(month == input$mes),aes(x=data,y=number)) +
              geom_point() +
              geom_jitter(width = 20,height = 0.2) +
              geom_smooth(method = "lm",formula = y~x,se = F)+
              xlab("Anos") +
              ylab("Numero de queimadas")+
              theme_classic() +
              ggtitle("Numero de queimadas por ano")
          }else{
    ggplot(dados %>%
             filter(month == input$mes)%>%
             filter(state == input$estado),aes(x=data,y=number)) +
      geom_point() +
      geom_jitter(width = 20,height = 0.2) +
      geom_smooth(method = "lm",formula = y~x,se = F)+
      xlab("Anos") +
      ylab("Numero de queimadas")+
      theme_classic() +
      ggtitle("Numero de queimadas por ano")
  }}})
}
# Rodar o shiny
shinyApp(ui = ui, server = server)
