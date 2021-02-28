#######################################################################
# Autoria: https://github.com/tailineju                               #
# Última atualização: out/2019                                        #
#######################################################################
# Código sem comentários detalhados sobre a estrutura do app.         #
# Para entender melhor confira os arquivos de tutorial no github.     #
#######################################################################

## CARREGANDO PACOTES E DADOS ----
pacman::p_load(ggplot2, tidyverse, tidyr,dplyr, lubridate, stringr,
               broom, graphics, GGally, fmsb, shinydashboard)
library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(broom)
library(graphics)
library(GGally)
library(fmsb)
library(shinydashboard)

climb <- read.csv("csv/climb.csv")
weat <- read.csv("csv/weather.csv")
colnames(climb) = c("dia","rotas","tentativas","sucessos","sAVG")
colnames(weat) = c("dia", "bateria", "temp", "humid", "vento", "direc","radiac")
rotasn <- as.character(unique(climb$rotas))

## COMEÇANDO O APP ----
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Mount Rainier"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Início", tabName = "intro", icon = icon("home")),
                        menuItem("Rotas", tabName = "aba1", icon = icon("map-marker-alt")),
                        menuItem("Tempo", tabName = "aba2", icon = icon("cloud")),
                        menuItem("Sucessos", tabName = "aba3", icon = icon("check-circle"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # Inicio
                        tabItem(tabName = "intro",
                                fluidRow(
                                  box(title = "SOBRE", status = "primary", solidHeader = TRUE, height = "280px",
                                      div("O Mount Rainier é um vulcão ativo com 4392m de altitude. Para se ter uma noção da magnitude da montanha, as geleiras permanentes que a rodeiam dão origem a 6 grandes rios e o pico pode ser visto da cidade de Seattle, a quase 100km de distância em linha reta. Fazer um roteiro para trilhar no parque é dificílimo. Não pela falta de informações, mas sim pela quantidade de atrações que ele oferece. O parque é dividido em várias áreas, cada uma com um centro de visitantes específico."
                                      )),
                                  box(title = "O QUE ENCONTRAR NA DASHBOARD", status = "warning", solidHeader = TRUE, height = "280px",
                                      div("Nessa dashboard será possível analisar um banco de dados sobre Escaladas e Fatores Climáticos no Mount Rainier. No menu a esquerda é possível escolher de que forma analisar os dados: a partir das 'rotas', do 'tempo' ou dos 'sucessos'. ")
                                  )),
                                fluidRow(
                                  box(title = "RESUMO", status = "success", solidHeader = TRUE, height = "450px",
                                      div("A seguir temos, respectivamente, a rota com maior número de tentativas, a com menor número de tentativas e a com maior sucesso."),
                                      p(),
                                      valueBoxOutput("maiort"),
                                      valueBoxOutput("menort"),
                                      valueBoxOutput("maiors")),
                                  box(title = "IMAGENS", status = "success", solidHeader = TRUE,
                                      sliderInput("imgs", NULL,
                                                  value = 1, min = 1, max = 3),
                                      imageOutput("img1"))
                                ),
                                h6("Fonte dos Textos: Trilhando Montanhas."),
                                h6("Fonte das Imagens: Wikipedia, Multifiles, SeattlePress.")
                        ),
                        # Aba 1
                        tabItem(tabName = "aba1",
                                fluidRow(
                                  box(title = "A PARTIR DAS ROTAS", status = "primary", solidHeader = TRUE, height = "170px",
                                      div("Nessa aba será possível fazer análises estatísticas quanto as trilhas do Mount Rainier.")),
                                  box(title = "O QUE ENCONTRAR", status = "warning", solidHeader = TRUE, height = "170px",
                                      div("No 'Gráfico 1' tem-se a opção de escolher uma rota em específico e observar todas as suas tentativas e sucessos."),
                                      div("No 'Gráfico 2' tem-se a opção de escolher uma data e observar quais trilhas tiveram tentativas e quantas (tentativas)")
                                  )
                                ),
                                fluidRow(
                                  box(title = "GRÁFICO 1", status = "success", solidHeader = TRUE,
                                      selectInput("rotar", "Escolha uma rota:",
                                                  choices = rotasn, selected = rotasn[1]),
                                      plotOutput("plot1")),
                                  box(title = "GRÁFICO 2", status = "success", solidHeader = TRUE,
                                      dateInput("dater", "Escolha uma data:",
                                                value = "2015-07-20",
                                                min = "2014-01-04", max = "2015-11-27"),
                                      plotOutput("plot7"))
                                )
                        ),
                        # Aba 2
                        tabItem(tabName = "aba2",
                                fluidRow(
                                  box(title = "A PARTIR DE FATORES CLIMÁTICOS", status = "primary", solidHeader = TRUE, height = "170px",
                                      div("Nessa aba será possível fazer análises estatísticas quanto aos fatores climáticos no Mount Rainier.")),
                                  box(title = "O QUE ENCONTRAR", status = "warning", solidHeader = TRUE, height = "170px",
                                      div("No 'Gráfico 3' tem-se a opção de escolher um fator climático e observar as correlações entre o fator escolhido e as tentativas e sucessos."),
                                      div("No 'Gráfico 4' tem-se a opção de escolher entre sucessos e tentativas quais para observar de forma mais ampla quais as correlações entre esses e os fatores climáticos.")
                                  )
                                ),
                                fluidRow(
                                  box(title = "GRÁFICO 3", status = "success", solidHeader = TRUE,
                                      selectInput("fator", "Escolha um fator:", 
                                                  choices = c("Temperatura Média","Ventos Médios",
                                                              "Humidade Média","Radiação Solar Média"),
                                                  selected = "Temperatura Média"),
                                      plotOutput("plot5")),
                                  tags$style(
                                  ".nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs 
                                    custom .nav-tabs li.active a {background-color: #fff;
                                        border-color: #fff;                                                      
                                    }
                                    .nav-tabs-custom .nav-tabs li.active {border-top-color: 
                                        #247209;
                                    }"
                                  ),
                                  tabBox(
                                    id = NULL,
                                    title = "GRÁFICO 4",
                                    side = "right", 
                                    height = "250px",
                                    tabPanel("Tentativas", 
                                             NULL,
                                             plotOutput("plot61")),
                                    tabPanel("Sucessos", 
                                             NULL,
                                             plotOutput("plot62")))
                                )
                        ),
                        # Aba 3
                        tabItem(tabName = "aba3",
                                fluidRow(
                                  box(title = "A PARTIR DOS SUCESSOS", status = "primary", solidHeader = TRUE, height = "170px",
                                      div("Nessa aba será possível fazer análises estatísticas quanto as quantidade de sucessos em completar as trilhas do Mount Rainier.")),
                                  box(title = "O QUE ENCONTRAR", status = "warning", solidHeader = TRUE, height = "170px",
                                      div("No 'Gráfico 5' tem-se a opção de escolher um intervalo de datas e observar as médias de sucessos em cada rota que obteve tentativas no período escolhido."),
                                      div("No 'Gráfico 6' tem-se um Gráfico de Radar onde é possível observar a quantidade de sucessos totais em cada rota.")
                                  )
                                ),
                                fluidRow(
                                  box(title = "GRÁFICO 5", status = "success", solidHeader = TRUE,
                                      dateRangeInput("datec", "Escolha um intervalo de datas:",
                                                     start = "2014-01-04", end = "2015-11-27"),
                                      plotOutput("plot4")),
                                  box(title = "GRÁFICO 6", status = "success", solidHeader = TRUE,
                                      plotOutput("plot3"))
                                )
                        )
                      ) #close tabitems
                    ) #close dashboard body
) #close dashboard page

server <- function(input,output){
  ## MANIPULANDO DATAS
  climb$dia <- mdy(climb$dia)
  weat$dia <- mdy(weat$dia)
  
  ##CRIANDO DATA FRAME PARA ROTAS
  ttent <- NULL
  tsuc <-  NULL
  for (i in 1:length(rotasn)){
    x <- str_which(climb$rotas, rotasn[i])
    ttent[i] <-  sum(climb[x, "tentativas"])
    tsuc[i] <- sum(climb[x, "sucessos" ])
  }
  agrup <- data.frame(rotasn, ttent, tsuc)
  
  ## CRIANDO DATA FRAME DATAS
  diasn <- as.character(weat$dia)
  dtent <- NULL
  dsuc <-  NULL
  for (j in 1:length(diasn)){
    w <- which(as.character(climb$dia) == diasn[j])
    dtent[j] <-  sum(climb[w, "tentativas"])
    dsuc[j] <- sum(climb[w, "sucessos" ])
  }
  agrupdias <- data.frame(diasn, dtent, dsuc, weat$temp, weat$humid, weat$vento, weat$radiac)
  colnames(agrupdias) <- c("dias","tentativas","sucessos","temp","humid","vento","radiac")
  
  
  # Inicio
  output$img1 <- renderImage({
    if (input$imgs == 1) {
      return(list(
        src = "img/1.jpg",
        contentType = "image/jpeg",
        width = "512px",
        height = "348px",
        alt = NULL
      ))
    } else if (input$imgs == 2) {
      return(list(
        src = "img/2.jpg",
        filetype = "image/jpeg",
        width = "512px",
        height = "348px",
        alt = NULL
      ))
    } else if (input$imgs == 3) {
      return(list(
        src = "img/3.jpg",
        filetype = "image/jpeg",
        width = "512px",
        height = "348px",
        alt = NULL
      ))
    }
  }, deleteFile = FALSE)
  
  output$maiort <- renderValueBox({
    valueBox(value = max(agrup$ttent), subtitle = agrup[which(agrup$ttent == max(ttent)), 1], 
             icon = icon("angle-up", lib = "font-awesome"), color = "olive")
  })
  output$menort <- renderValueBox({
    valueBox(value = min(agrup$ttent), subtitle =  agrup[which(agrup$ttent == min(ttent))[1], 1], 
             icon = icon("angle-down", lib = "font-awesome"), color = "navy" )
  })
  output$maiors <- renderValueBox({
    valueBox(value = max(agrup$tsuc), subtitle =  agrup[which(agrup$tsuc == max(tsuc)), 1], 
             icon = icon("angle-double-up", lib = "font-awesome"), color = "purple" )
  })
  # Aba 1
  output$plot1 <- renderPlot({
    ggplot(data=climb %>%
             filter(rotas == input$rotar))+
      aes(x=sucessos,y=tentativas) + 
      geom_point(colour="red") +
      xlab("Sucessos") +
      ylab("Tentativas")+
      theme_classic() +
      ggtitle(NULL)
  })
  output$plot7 <- renderPlot({
    ggplot(data=climb %>%
             filter(dia == input$dater)) +
      aes(x=tentativas,y=rotas) + 
      geom_point(colour="red") +
      xlab("Tentativas") +
      ylab("Rotas")+
      theme_classic() +
      ggtitle(NULL)
  })
  
  # Aba 2
  output$plot5 <- renderPlot({ 
    if (input$fator == "Temperatura Média"){
      ggpairs(data = agrupdias,
              columns = c("tentativas", "sucessos", "temp"),
              columnLabels = c("Tentativas",
                               "Sucesso",
                               "Temperatura Média"))
      
    }else if(input$fator == "Ventos Médios"){
      ggpairs(data = agrupdias,
              columns = c("tentativas", "sucessos", "vento"),
              columnLabels = c("Tentativas",
                               "Sucesso",
                               "Ventos Médios"))
      
    }else if(input$fator == "Humidade Média"){
      ggpairs(data = agrupdias,
              columns = c("tentativas", "sucessos", "humid"),
              columnLabels = c("Tentativas",
                               "Sucesso",
                               "Humidade Média"))
      
    }else if(input$fator == "Radiação Solar Média"){
      ggpairs(data = agrupdias,
              columns = c("tentativas", "sucessos", "radiac"),
              columnLabels = c("Tentativas",
                               "Sucesso",
                               "Radiação Média"))
    }
  })
  output$plot61 <- renderPlot({
      agrupdias %>%
        select(tentativas, temp, humid, vento, radiac) %>%
        na.omit() %>%
        ggcorr(data = ., method = c("everything", "spearman"),
               label=TRUE, label_round=2,
               low="#e03531", mid="#f0bd27", high="#51b364") 
  })
  output$plot62 <- renderPlot({
      agrupdias %>%
        select(sucessos, temp, humid, vento, radiac) %>%
        na.omit() %>%
        ggcorr(data = ., method = c("everything", "spearman"),
               label=TRUE, label_round=2,
               low="#e03531", mid="#f0bd27", high="#51b364")
  })
  
  
  # Aba 3
  output$plot3 <- renderPlot({
    g <- as.data.frame(matrix(agrup$tsuc, ncol=length(agrup$tsuc)))
    colnames(g) <-  agrup$rotasn
    g <- rbind(rep(max(agrup$tsuc),length(agrup$tsuc)) , rep(0,length(agrup$tsuc)), g)
    radarchart(g, axistype=1, 
               cglcol="black", 
               cglty=1, 
               axislabcol="white", 
               cglwd=0.5, 
               vlcex=0.6)
  })
  output$plot4 <- renderPlot({
    ggplot(data=climb %>%
             filter(dia>=input$datec[1])%>%
             filter(dia<=input$datec[2])) +
      aes(x=sAVG,y=rotas) + 
      geom_point(colour="red") +
      xlab("Taxa de Sucesso") +
      ylab("Rotas")+
      theme_classic() +
      ggtitle(NULL)
  })
  
  
}

shinyApp(ui=ui, server=server)