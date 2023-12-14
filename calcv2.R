library(shiny)
library(ggplot2)

# Define UI with blue background and button-based number input
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            .shiny-output-error { color: red; }
            body { background-color: #ADD8E6; }
        "))
  ),
  titlePanel("Ben's Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("equation", "Enter equation (in terms of 'x'):", value = "x^2"),
      numericInput("value", "Enter a value for 'x':", value = 0),
      actionButton("add", "Add"),
      actionButton("subtract", "Subtract"),
      actionButton("multiply", "Multiply"),
      actionButton("divide", "Divide"),
      actionButton("calc", "Calculate"),
      actionButton("graph", "Graph"),
      hr(),
      h4("Calculation Result:"),
      verbatimTextOutput("calcOutput"),
      h4("Graph:")
    ),
    mainPanel(
      plotOutput("graphOutput")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  calculate <- function(equation, x) {
    eval(parse(text = equation), envir = list(x = x))
  }
  
  observeEvent(input$add, {
    updateNumericInput(session, "value", value = input$value + 1)
  })
  
  observeEvent(input$subtract, {
    updateNumericInput(session, "value", value = input$value - 1)
  })
  
  observeEvent(input$multiply, {
    updateNumericInput(session, "value", value = input$value * 2)
  })
  
  observeEvent(input$divide, {
    updateNumericInput(session, "value", value = input$value / 2)
  })
  
  output$calcOutput <- renderText({
    input$calc  # Trigger calculation
    isolate({
      equation <- gsub("x", as.character(input$value), input$equation)
      result <- tryCatch({
        calculate(input$equation, input$value)
      }, error = function(e) {
        "Error in calculation"
      })
      paste("Result of", input$equation, "for x =", input$value, "is", result)
    })
  })
  
  output$graphOutput <- renderPlot({
    input$graph  # Trigger graph
    isolate({
      tryCatch({
        ggplot(data = data.frame(x = c(-10, 10)), aes(x = x)) +
          stat_function(fun = function(x) calculate(input$equation, x)) +
          ylim(-10, 10) +
          ggtitle(paste("Graph of", input$equation))
      }, error = function(e) {
        plot.new()
        title(main = "Error in plotting graph")
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

