#
# Shiny web application for illustrating the choice of distances to minimize in
# linear regression: vertical, horizontal or perpendicular
#

library(shiny)

# Data
set.seed(34567)
x <- rnorm(50)
eps <- rnorm(50)
yLin <- -0.5 + 1.5 * x + eps
yQua <- -0.5 + 1.5 * x^2 + eps
yExp <- -0.5 + 1.5 * 2^x + eps
red <- rgb(1, 0, 0, alpha = 0.75)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - the select inputs for the data pattern and type of distance
  # - the slider inputs for intercept and slope
  verticalLayout(

    inputPanel(

      selectInput(inputId = "dataType", label = "Data pattern:",
                  choices = c("linear", "quadratic", "exponential")),
      selectInput(inputId = "distType", label = "Type of distance:",
                  choices = c("vertical", "horizontal", "perpendicular")),
      sliderInput(inputId = "beta0", label = "Intercept:",
                  min = -3, max = 3, value = 0, step = 0.05),
      sliderInput(inputId = "beta1", label = "Slope:",
                  min = -3, max = 3, value = 0.5, step = 0.05)

    ),

    plotOutput("regressionPlot")

  )

)

# Server logic
server <- function(input, output) {

  output$regressionPlot <- renderPlot({

    # Response
    y <- switch(input$dataType,
                linear = yLin,
                quadratic = yQua,
                exponential = yExp)

    # Projections
    if (input$distType == "vertical") {

      # Vertical projection
      proj <- cbind(x, input$beta0 + input$beta1 * x)

    } else if (input$distType == "horizontal") {

      # Horizontal projection
      proj <- cbind((y - input$beta0) / input$beta1, y)

    } else {

      # Perpendicular projection into a * x + b * y + c = 0
      a <- input$beta1
      b <- -1
      c <- input$beta0
      proj <- cbind(b * (b * x - a * y) - a * c,
                    a * (-b * x + a * y) - b * c) / (a^2 + b^2)

    }

    # Plot
    par(mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 16)
    segments(x0 = x, y0 = y, x1 = proj[, 1], y1 = proj[, 2], col = red, lty = 2)
    title(main = paste("Sum of squared distances:",
                       sprintf("%.2f", sum((proj[, 1] - x)^2 + (proj[, 2] - y)^2))),
          cex.main = 1.5)
    abline(a = input$beta0, b = input$beta1, col = 2, lwd = 3)
    points(proj[, 1], proj[, 2], col = 2, pch = 16)
    legend("bottomright", legend = c("Fitted regression", "Distances"),
           col = c(2, red), lwd = c(3, 1), lty = 1:2, cex = 1.5)
    
  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

