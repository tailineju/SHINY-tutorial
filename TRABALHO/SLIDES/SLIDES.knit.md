---
title: "SHINY" 
subtitle: "INTERATIVIDADE COM R"
author: "Davi, Eduardo, Gabriela, Jadson, Tailine"
output: 
  beamer_presentation:
    keep_tex: true
    includes:
     in_header: Estilo.txt
     theme: "fibeamer"
    latex_engine: xelatex
---



## SUMÁRIO

\begin{enumerate}
\item O que é o Shiny?
\item Estrutura básica
\begin{enumerate}
\item User Interface
\item Server
\end{enumerate}
\item Aplicações
\item Anexos
\item Referências
\end{enumerate}

##
\begin{textblock*}{0cm}(0cm,-.0cm)
\includegraphics[width=\paperwidth, height=\paperheight]{imagens/logo}
\end{textblock*}

## O QUE É O SHINY

Estruturar tanto a interface com o usuário quanto o processamento de dados, geração de visualizações e modelagem, isto é, nós programamos tanto o user side quanto o server side numa tacada só. Assim, ao rodarmos o código, criamos um servidor que envia páginas web, recebe informações do usuário e processa os dados, utilizando apenas o R.

## INTERATIVIDADE

\section{Instruções do Servidor (R)}
\begin{center}
\includegraphics[width=13mm]{imagens/trade}
\end{center}
\section{User Interface (UI)}

## REATIVIDADE

\begin{figure}
\includegraphics[width=110mm]{imagens/cxv}
\end{figure}


## ESTRUTURA


```r
library(shiny)

ui <- fluidPage()

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
```

## USER INTERFACE (UI)

\begin{table}
\begin{tabular}{l | l}
Função & Finalidade \\
\hline \hline
library(shiny) & \small Carregar o pacote Shiny. \\
ui <- fluidPage() & \small Criar uma interface com o usuário. \\
titlePanel() & \small Criar um painel contendo um título do aplicativo. \\
sidebarLayout() & \small Criar um layout com uma barra lateral e área principal. \\
sidebarPanel() & \small Criar um painel com barra lateral. \\
mainPanel() & \small Criar um painel principal contendo elementos de saída. \\
\end{tabular}
\end{table}

## WIDGETS

\includegraphics[width=4.4in]{imagens/inpfun}
\begin{center}
\tiny{Fonte: Shiny from RStudio}
\end{center}

## CRIANDO FUNÇÕES DE ENTRADA

\only<2>{
\begin{textblock}{1}[0, .5](1.7, 8.5)
\begin{tikzpicture}
    \draw[fibeamer@red, ultra thick,rounded corners] (0,0) rectangle (6.8,2.1);
\end{tikzpicture}
\end{textblock}
}


```r
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num",
              value = 25, 
              min = 1, max = 100) )
server <- function(input, output) {}
shinyApp(ui = ui, server = server)
```

## PRÓXIMO PASSO

\begin{center}
Para que seja possível \alert{visualizar} o input, é necessário escolher como será o \alert{output.}
Para esse exemplo, queremos que o output gere um \alert{gráfico.}
Mas que \alert{função} precisamos usar agora?
\end{center}

## OUTPUTS

\begin{table}
\begin{tabular}{l | l}
Função & Finalidade \\
\hline \hline
dataTableOutput() &  Tabela Interativa \\
htmlOutput() & HTML puro \\ 
imageOutput() & Imagem \\
plotOutput() & Gráfico \\
tableOutput() & Tabela \\
textOutput() & Texto \\
uiOutput() & Elemento do Shiny UI \\
verbatimTextOutput() & Texto \\
\end{tabular}
\end{table}

## DEFININDO O TIPO DE OUTPUT

\only<2>{
\begin{textblock}{1}[0, .5](1.7, 10.5)
\begin{tikzpicture}
    \draw[fibeamer@red, ultra thick,rounded corners] (0,0) rectangle (5,.7);
\end{tikzpicture}
\end{textblock}
}


```r
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num",
              value = 25, 
              min = 1, max = 100), 
  plotOutput("hist"))
server <- function(input, output) {}
shinyApp(ui = ui, server = server)
```

## RESULTADO
\begin{center}
      Agora foi gerado um \alert{botão de slide} onde o usuário fará a escolha de um número entre 1 e 100.\\\bigskip
    \includegraphics[width=75mm]{imagens/p1}
\end{center}

## PRÓXIMO PASSO

\begin{center}
A próxima etapa é \alert{configurar} o output.
\end{center}
\begin{center} 
Dentro do UI, apenas demos alguns nomes.
\end{center}
\begin{center}
Agora precisamos definir o que realmente vai acontecer.
\end{center}

## SERVER

\begin{table}
\begin{tabular}{l | l}
Função & Finalidade \\
\hline \hline
library(shiny) & \small Carregar o pacote Shiny. \\
shinyServer() & \small Definir a lógica do servidor do aplicativo Shiny. \\ 
function(input,output){} & \small Funções render() \\
\end{tabular}
\end{table}

## RENDER ()

\begin{table}
\begin{tabular}{l | l}
Output (UI) & Render (Server) \\
\hline \hline
dataTableOutput() &  renderDataTable \\
imageOutput() & renderImage \\
plotOutput() & renderPlot \\
tableOutput() & renderTable \\
textOutput() & renderText \\
verbatimTextOutput() & renderPrint \\
uiOutput() & renderUI\\
htmlOutput() & renderUI \\ 
\end{tabular}
\end{table}

## CONFIGURANDO O OUTPUT

\only<2>{
\begin{textblock}{1}[0, .5](1.7, 11.6)
\begin{tikzpicture}
    \draw[fibeamer@red, ultra thick,rounded corners] (0,0) rectangle (5.7,1.5);
\end{tikzpicture}
\end{textblock}
}


```r
ui <- fluidPage(
  sliderInput(inputId = "num", 
    value = 25, 
    min = 1, max = 100), 
  plotOutput("hist"))
server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))})}
shinyApp(ui = ui, server = server)
```

## RESULTADO
\begin{columns}
  \centering
    \column{0.5\textwidth}
      \includegraphics[width=50mm]{imagens/p2}
    \column{0.5\textwidth}
      \includegraphics[width=50mm]{imagens/p3}
\end{columns}

# APLICAÇÕES

# ANEXOS

## APLICAÇÕES I
\tiny{

```r
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
```
}
## APLICAÇÕES II


```r
library(shiny)

ui <- fluidPage()

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
```

## REFERÊNCIAS

1. RSTUDIO INC. \textbf{Shiny from RStudio}. Disponível em: <https://shiny.rstudio.com/tutorial/>. Acesso em: setembro de 2019.
2. PUC MINAS. \textbf{Abakos}, Belo Horizonte,v. 6, n. 2, p. 55-71, maio 2018
3. \textbf{Curso-R}. Disponível em: <http://material.curso-r.com/shiny/>. Acesso em: setembro de 2019.