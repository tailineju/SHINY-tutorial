library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.dataset === "diamonds"',
                checkboxGroupInput("show_vars", "Colunas a serem mostradas:",
                                   names(diamonds), selected = names(diamonds))
            ),
            conditionalPanel(
                'input.dataset === "mtcars"'
            ),
            conditionalPanel(
                'input.dataset === "iris'
            )
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("diamonds", DT::dataTableOutput("mytable1")),
                tabPanel("mtcars", DT::dataTableOutput("mytable2")),
                tabPanel("iris", DT::dataTableOutput("mytable3"))
            )
        )
    )
)

server <- function(input, output){
    
    diamonds2 = diamonds[sample(nrow(diamonds),1000), ]
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
    })
    output$mytable2 <- DT::renderDataTable({
        DT::datatable(mtcars, options = list(orderClasses = TRUE))
    })
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 15))
    })
}

shinyApp(ui, server)


#https://projetoshiny.shinyapps.io/DTshiny/#
