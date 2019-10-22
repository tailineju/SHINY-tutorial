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
\item Referências
\end{enumerate}

##
\begin{textblock*}{0cm}(0cm,-.0cm)
\includegraphics[width=\paperwidth, height=\paperheight]{imagens/logo}
\end{textblock*}

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

## RESUMINDO

\begin{itemize}
\item Pacote do R
\item Cria de um servidor que envia páginas web, \textbf{recebe} informações do usuário e \textbf{processa} os dados.
\item Permite estruturar a interface do usuário \textbf{e} o processamento de dados.
\item Vantagens para o programador e para o usuário.
\end{itemize}


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
              label = NULL,
              value = 25, min = 1, max = 100) )
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
              label = NULL,
              value = 25, min = 1, max = 100), 
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
\begin{textblock}{1}[0, .5](1.7, 12.2)
\begin{tikzpicture}
    \draw[fibeamer@red, ultra thick,rounded corners] (0,0) rectangle (5.7,1.5);
\end{tikzpicture}
\end{textblock}
}


```r
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = NULL,
              value = 25, min = 1, max = 100), 
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

## REFERÊNCIAS

1. RSTUDIO INC. \textbf{Shiny from RStudio}. Disponível em: <https://shiny.rstudio.com/tutorial/>. Acesso em: setembro de 2019.
2. PUC MINAS. \textbf{Desenvolvimento de Aplicativos Web Com R e Shiny:} inovações no ensino de Estatística. Belo Horizonte,v. 6, n. 2, p. 55-71, maio 2018
3. \textbf{Curso-R}. Disponível em: <http://material.curso-r.com/shiny/>. Acesso em: setembro de 2019.