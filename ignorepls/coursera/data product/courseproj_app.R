library(shiny)

vars <- setdiff(names(iris), "Species")

# Define UI 
ui <- fluidPage(
    

    
    # Application title
    titlePanel('Iris k-means clustering'),

    # Sidebar with a inputs
    sidebarLayout(
        sidebarPanel(
            selectInput('xcol', 'X Variable', vars),
            selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
            numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
        ),

        # Show a plot of the k-means clustering
        mainPanel(
           plotOutput("plot1")
        )
    )
)

server <- function(input, output) {

    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        iris[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
