library(shinydashboard)
library(shiny)
library("plyr")

body <- dashboardBody(

  fluidRow(
    box(
      title = "Carro com o melhor consumo", width = 6, solidHeader = TRUE, status = "success", "Toyota Corolla"
    ), #primary, success, info, warning, danger.
    box(
      title = "Carro com o pior consumo", width = 6, solidHeader = TRUE, status = "danger", "Cadillac Fleetwood"
    )
  ),
  
  fluidRow(
    valueBoxOutput("menorConsumo"),
    valueBoxOutput("consumoMedio"),
    valueBoxOutput("maiorConsumo")
  ),
  
  fluidRow(
      box(
        title = "Cambios utilizados", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("plot")
      ),
      box(
          title = "Histograma", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          plotOutput("plot2")
      )
  ),
  
  fluidRow(
    valueBoxOutput("mediaMPG_aut", width = 2),
    valueBoxOutput("mediaHP_aut", width = 2),
    valueBoxOutput("mediaWT_aut", width = 2),
    valueBoxOutput("mediaMPG_man", width = 2),
    valueBoxOutput("mediaHP_man", width = 2),
    valueBoxOutput("mediaWT_man", width = 2)
  )
  
)

    
ui <- dashboardPage(
  dashboardHeader(title = "MTCARS"),
  dashboardSidebar ( disable = TRUE ),
  body
)


server <- function(input, output){
  mtcars2 <- mtcars
  mtcars2 <- as.data.frame(mtcars2)
  dados <- ddply(mtcars2, .(am),summarize,mediaMPG = mean(mpg), mediaDISP = mean(disp), mediaHp = mean(hp), mediaDRAT = mean(drat), mediaWT = mean(wt), mediaQSEC = mean(qsec), quantMODELO = length(am))
  maiorConsumo <- max(mtcars2$mpg)
  menorConsumo <- min(mtcars2$mpg)
  consumoMedio <- mean(mtcars2$mpg)
  dadosGrafico <- as.data.frame(dados$quantMODELO)
  
  output$menorConsumo <- renderValueBox({
    valueBox(paste0(menorConsumo, " MPG"), "Menor consumo", icon = icon("thumbs-up", lib = "glyphicon"),color = "green")
  })
  
  output$consumoMedio <- renderValueBox({
    valueBox(paste0(round(consumoMedio,2), "MPG"), "Consumo medio", icon = icon("balance-scale"),color = "yellow")
  })
  
  output$maiorConsumo <- renderValueBox({
    valueBox(paste0(maiorConsumo, "MPG"), "Maior consumo", icon = icon("angry"),color = "red")
  })
  
  output$plot <- renderPlot(pie(as.numeric(dados$quantMODELO), main = "Tipo de câmbio",  labels = c("Automático", "Manual")))
  
  output$plot2 <- renderPlot(hist(mtcars2$cyl, xlab = "Quantidade de cilindros", ylab = "Ocorrências", main = "Quantidade de carros por cilindros"))
  
  output$mediaMPG_aut <- renderValueBox({
    valueBox(paste0(round(dados[1,2],2), "MPG"), "Consumo médio automático", color = "navy")
  })
  
  output$mediaMPG_man <- renderValueBox({
    valueBox(paste0(round(dados[2,2],2), "MPG"), "Consumo médio manual")
  })
  
  output$mediaHP_aut <- renderValueBox({
    valueBox(paste0(round(dados[1,4],2), "HP"), "Potência média automático", color = "navy")
  })
  
  output$mediaHP_man <- renderValueBox({
    valueBox(paste0(round(dados[2,4],2), "HP"), "Potência média manual")
  })
  
  output$mediaWT_aut <- renderValueBox({
    valueBox(paste0(round(dados[1,6],2), "WT"), "Peso médio automático", color = "navy")
  })
  
  output$mediaWT_man <- renderValueBox({
    valueBox(paste0(round(dados[2,6],2), "WT"), "Peso médio manual")
  })
}
shinyApp(ui, server)
