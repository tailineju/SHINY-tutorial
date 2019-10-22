library(shiny)

ui <- fluidPage(
    titlePanel("Upload de arquivos"),
    sidebarLayout(
        sidebarPanel(
            fileInput("idarquivo", "Selecione o seu arquivo", accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values', '.csv', '.tsv')),
            tags$hr(),
            checkboxInput('header', "Header", TRUE),
            radioButtons('sep', 'Separador de colunas', c("virgula" = ',', "Ponto e virgula" = ';', "Tab"= '\t'), ',')
        ),
        
        mainPanel(
            actionButton("botao", "Ler o arquivo")
        )
    )
)

options(shiny.maxRequestSize = 100*1024^2)
server <- function(input, output){
    observeEvent(input$botao, {
        print(input$header)
        arquivo <- read.csv(input$idarquivo$datapath, header = input$header, sep = input$sep)
        print(arquivo)
    })
}

shinyApp(ui, server)