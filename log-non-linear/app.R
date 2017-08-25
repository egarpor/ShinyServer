#
# Shiny web application for illustrating non-linear transformations of the
# predictor in logistic regression
#

library(shiny)

# Data
set.seed(34567)
n <- 100
x <- runif(n, -5, 5)
xx <- seq(-6, 6, l = 200)
logistic <- function(z) 1 / (1 + exp(-z))
y1 <- rbinom(n = n, prob = logistic(0 + 0.5 * x), size = 1)
y2 <- rbinom(n = n, prob = logistic(-2 + x + 0.5 * x^2), size = 1)
y3 <- rbinom(n = n, prob = logistic(-2 + 0.25 * x + 0.5 * (x - 1)^2 + 1 * (x + 1)^3), size = 1)
y4 <- rbinom(n = n, prob = logistic(-3 + 12 * x - 2 * x^2 - 4 * x^3 + x^4), size = 1)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - the select inputs for the data pattern and type of transformation
  verticalLayout(

    inputPanel(

      selectInput(inputId = "dataType", label = "Dataset pattern:",
                  choices = c("One break", "Two breaks", "Three breaks",
                              "Four breaks")),
      selectInput(inputId = "transfType", label = "Polynomial transformation:",
                  choices = c("Degree 1", "Degree 2", "Degree 3", "Degree 4"))

    ),

    plotOutput("transformationPlot")

  )

)

# Server logic
server <- function(input, output) {

  output$transformationPlot <- renderPlot({

    # Response
    y <- switch(input$dataType,
                "One break" = y1,
                "Two breaks" = y2,
                "Three breaks" = y3,
                "Four breaks" = y4)

    # Transformation of x and xx
    xTransf <- switch(input$transfType,
                      "Degree 1" = x,
                      "Degree 2" = cbind(x, x^2),
                      "Degree 3" = cbind(x, x^2, x^3),
                      "Degree 4" = cbind(x, x^2, x^3, x^4))
    xxTransf <- switch(input$transfType,
                       "Degree 1" = cbind(xx),
                       "Degree 2" = cbind(xx, xx^2),
                       "Degree 3" = cbind(xx, xx^2, xx^3),
                       "Degree 4" = cbind(xx, xx^2, xx^3, xx^4))

    # Model regressions
    mod <- glm(y ~ x, family = "binomial")
    modTransf <- glm(y ~ xTransf, family = "binomial")

    # Plot
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, pch = 16, ylim = c(0, 1))
    s <- summary(mod)
    title(main = substitute(expr = "Original. " * R^2 * " = " * R2,
                            list(R2 = sprintf("%.3f", 1 - s$deviance / s$null.deviance))),
          cex.main = 1.25)
    lines(xx, logistic(mod$coefficients[1] + mod$coefficients[2] * xx),
          col = 2, lwd = 3)
    plot(x, y, pch = 16)
    sTransf <- summary(modTransf)
    title(main = substitute(expr = "Transformation. " * R^2 * " = " * R2,
                            list(R2 = sprintf("%.3f", 1 - sTransf$deviance /
                                                sTransf$null.deviance))),
          cex.main = 1.25)
    lines(xx, logistic(modTransf$coefficients[1] + xxTransf %*% modTransf$coefficients[-1]),
          col = 2, lwd = 3)

  }, width = 650, height = 325)

}

# Run the application
shinyApp(ui = ui, server = server)

