#
# Shiny web application for illustrating non-linear transformations of the
# predictor in multiple regression
#

library(shiny)

# Data
set.seed(34567)
n <- 100
x <- runif(n, -4, 4)
xx <- seq(-4, 4, l = 200)
eps <- rnorm(n)
y1 <- 0 + 0.5 * x + 0.5 * eps
y2 <- -2 + x + 0.5 * x^2 + eps
y3 <- -2 + 0.25 * x + 0.5 * (x - 1)^2 + 2 * (x + 1)^3 + 10 * eps
y4 <- -3 + 12 * (x + 1) - 2 * (x + 1)^2 - 4 * (x + 1)^3 + (x + 1)^4 + 10 * eps

# UI for application
ui <- fluidPage(align = "center",
  
  # Vertical layout with:
  # - the select inputs for the data pattern and type of transformation
  verticalLayout(
    
    inputPanel(
      
      selectInput(inputId = "dataType", label = "Dataset pattern:",
                  choices = c("Linear", "Quadratic", "Cubic", "Quartic")),
      selectInput(inputId = "transfType", label = "Polynomial fit:",
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
                "Linear" = y1,
                "Quadratic" = y2,
                "Cubic" = y3,
                "Quartic" = y4)
    
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
    mod <- lm(y ~ x)
    modTransf <- lm(y ~ xTransf)
    
    # Plot
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, pch = 16)
    title(main = substitute(expr = "Original. " * R^2 * " = " * R2,
                            list(R2 = sprintf("%.3f", summary(mod)$r.squared))),
          cex.main = 1.25)
    lines(xx, mod$coefficients[1] + mod$coefficients[2] * xx,
          col = 2, lwd = 3)
    plot(x, y, pch = 16)
    title(main = substitute(expr = "Transformation. " * R^2 * " = " * R2,
                            list(R2 = sprintf("%.3f", summary(modTransf)$r.squared))),
          cex.main = 1.25)
    lines(xx, modTransf$coefficients[1] + xxTransf %*% modTransf$coefficients[-1],
          col = 2, lwd = 3)
    
  }, width = 650, height = 325)
  
}

# Run the application
shinyApp(ui = ui, server = server)

