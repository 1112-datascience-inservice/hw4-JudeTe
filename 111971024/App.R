library(shiny)
# library(ggvis)
library(ggbiplot)
library("ggplot2")
library("FactoMineR")
library("factoextra")

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("胡元亨的PCA & CA作業"),

  tabsetPanel(
    tabPanel("PCA",
      # Sidebar layout with a input and output definitions ----
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          # Input: Selector for choosing dataset ----
          selectInput(inputId = "first_pc_axis",
                      label = "First PC axis:",
                      choices = c("PC1", "PC2", "PC3", "PC4")),
          selectInput(inputId = "second_pc_axis",
                      label = "Second PC axis:",
                      choices = c("PC1", "PC2", "PC3", "PC4")),
          # Input: Numeric entry for number of obs to view ----
          # numericInput(inputId = "obs",
          #              label = "Number of observations to view:",
          #              value = 10)
        ),
        # Main panel for displaying outputs ----
        mainPanel(
          plotOutput("pcaplot"),
          # Output: HTML table with requested number of observations ----
          # tableOutput("view")
        )
      )
    ),
    tabPanel("CA",
             sidebarLayout(
               sidebarPanel(
                  sliderInput("point_of_ca", "Number of points",
                    min = 6, max = 150,
                    value = 100, step = 1),
               ),
               mainPanel(
                plotOutput("caplot"),
               )
             )
    ),
    tabPanel("Raw Data",
      HTML("<h4>Summery of raw data:</h4>"),
      mainPanel(
        verbatimTextOutput("summary")
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  #PCA Plot
  output$pcaplot <- renderPlot({
    first_pc_var <- input$first_pc_axis
    if (first_pc_var == "PC1") {
      first_pc_axis <- 1
    } else if (first_pc_var == "PC2") {
      first_pc_axis <- 2
    } else if (first_pc_var == "PC3") {
      first_pc_axis <- 3
    } else if (first_pc_var == "PC4") {
      first_pc_axis <- 4
    } else {
      first_pc_axis <- 1
    }
    second_pc_var <- input$second_pc_axis
    if (second_pc_var == "PC1") {
      second_pc_axis <- 1
    } else if (second_pc_var == "PC2") {
      second_pc_axis <- 2
    } else if (second_pc_var == "PC3") {
      second_pc_axis <- 3
    } else if (second_pc_var == "PC4") {
      second_pc_axis <- 4
    } else {
      second_pc_axis <- 1
    }
    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    ggbiplot(ir.pca, choices=c(first_pc_axis, second_pc_axis), obs.scale = 1, var.scale = 1, groups = ir.species) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal', legend.position = 'top')
  })
  # CA Plot (Correspondence analysis plot)
  output$caplot <- renderPlot({
    # iris.table <- table(iris[, input$ca_row_var], iris[, input$ca_col_var])
    # # ir.ca <- ca(iris.table)
    # ir.ca <- CA(X = iris[1:input$centers(k), 1:4], graph = FALSE)
    # g <- ggbiplot(ir.ca, obs.scale = 1, var.scale = 1, 
    #               groups = iris[, input$ca_row_var], 
    #               choices = c(1, 2), ellipse = TRUE, circle = TRUE)
    # g <- g + scale_color_discrete(name = '')
    # g <- g + theme(legend.direction = 'horizontal', 
    #                legend.position = 'top')
    # print(g)
    res.ca <- CA(X = iris[1:input$point_of_ca, 1:4], graph = FALSE) 
    fviz_ca_biplot(res.ca, repel = TRUE)
  })

  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(iris)
  })

  # Return the requested dataset ----
  # datasetInput <- reactive({
  #   switch(input$first_pc_axis,
  #          "PC1" = 1,
  #          "PC2" = 2,
  #          "PC3" = 3,
  #          "PC4" = 4)
  # })

  # Generate a summary of the dataset ----
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })

  # Show the first "n" observations ----
  # output$view <- renderTable({
  #   head(datasetInput(), n = 150)
  # })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
