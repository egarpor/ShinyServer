#
# Shiny web application for illustrating the influence in the regression line of
# the sample size, error variance and predictor variance
#

library(shiny)

# Load predictor's data
load("xData.RData")

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - action buttom for generating a new sample
  # - select input for sample size
  # - the slider inputs for intercept and slope
  # - the slider inputs variance of error and predictor

  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(10, 50, 100, 200, 500), selected = 100),
      sliderInput(inputId = "beta0", label = "Intercept:",
                  min = -3, max = 3, value = 0, step = 0.5),
      sliderInput(inputId = "beta1", label = "Slope:",
                  min = -3, max = 3, value = 0.5, step = 0.5),
      sliderInput(inputId = "sigma2", label = "Error variance:",
                  min = 0, max = 3, value = 1, step = 0.1),
      sliderInput(inputId = "sigma2x", label = "Predictor variance:",
                  min = 0.1, max = 3, value = 1, step = 0.1)

    ),

    plotOutput("regressionPlot")

  )

)

# Server logic
server <- function(input, output) {

  # Manage the first call
  values <- reactiveValues(default = 0)
  observeEvent(input$newSample, {

    values$default <- input$newSample

  })

  # Error sampling
  eps <- eventReactive(input$newSample, {

    rnorm(500)

  })

  output$regressionPlot <- renderPlot({

    # Check if the buttom was clicked
    if (values$default == 0){

      set.seed(423432)
      error <- rnorm(500)

    } else {

      error <- eps()

    }

    # Response's data
    x <- sqrt(input$sigma2x) * xData[1:input$n]
    regX <- input$beta0 + input$beta1 * x
    y <- regX + sqrt(input$sigma2) * error[1:input$n]

    # Plot
    par(mar = c(4, 4, 1, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 16, xlab = "x", ylab = "y")
    abline(a = input$beta0, b = input$beta1, col = 1, lwd = 3)
    abline(lm(y ~ x)$coefficients, col = 2, lwd = 3)
    legend("bottomright", legend = c("True regression", "Fitted regression"),
           lwd = 3, col = 1:2, cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

