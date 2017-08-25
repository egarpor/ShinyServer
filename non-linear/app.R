#
# Shiny web application for illustrating non-linear transformations of the
# predictor in linear regression
#

library(shiny)

# Data
set.seed(34567)
n <- 100
x <- runif(n, 1, 10)
eps <- rnorm(n)
y1 <- -0.5 + 1.5 * x + eps
y2 <- -0.5 - 0.5 * x^2 + 2 * eps
y3 <- 1 + 5 * sqrt(x) + 0.5 * eps
y4 <- 5 - 10 * log(x) + 2 * eps
y5 <- -0.5 + 0.005 * exp(x) + 2 * eps
y6 <- -0.5 + 20 * exp(-x) + 0.5 * eps

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - the select inputs for the data pattern and type of transformation
  verticalLayout(

    inputPanel(

      selectInput(inputId = "dataType", label = "Dataset pattern:",
                  choices = c("Linear", "Quadratic", "Square root", "Logarithm",
                              "Exponential", "Negative exponential")),
      selectInput(inputId = "transfType", label = "Transform x into:",
                  choices = c("x", "x^2", "sqrt(x)", "log(x)",
                              "exp(x)", "exp(-x)"))

    ),

    plotOutput("transformationPlot")

  )

)

# Server logic
server <- function(input, output) {

  output$transformationPlot <- renderPlot({

    # Response
    y <- switch(input$dataType,
                "Linear" = y1,
                "Quadratic" = y2,
                "Square root" = y3,
                "Logarithm" = y4,
                "Exponential" = y5,
                "Negative exponential" = y6)

    # Transformation of x
    xTransf <- switch(input$transfType,
                      "x" = x,
                      "x^2" = x^2,
                      "sqrt(x)" = sqrt(x),
                      "log(x)" = log(x),
                      "exp(x)" = exp(x),
                      "exp(-x)" = exp(-x))

    # Model regressions
    mod <- lm(y ~ x)
    modTransf <- lm(y ~ xTransf)

    # Plot
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, pch = 16)
    title(main = substitute(expr = "Original predictor. " * R^2 * " = " * R2,
                            list(R2 = sprintf("%.3f", summary(mod)$r.squared))),
          cex.main = 1.25)
    abline(coef(mod), col = 2, lwd = 3)
    plot(xTransf, y, pch = 16, xlab = input$transfType)
    title(main = substitute(expr = "Transformed predictor. " * R^2 * " = " * R2,
                            list(R2 = sprintf("%.3f", summary(modTransf)$r.squared))),
          cex.main = 1.25)
    abline(coef(modTransf), col = 2, lwd = 3)

  }, width = 650, height = 325)

}

# Run the application
shinyApp(ui = ui, server = server)

