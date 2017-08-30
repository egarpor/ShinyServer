#
# Shiny web application for illustrating the bias and variance of the moving 
# histogram (or naive density estimator)
#

library(shiny)

# Load data
load("xData.RData")

# UI for application
ui <- fluidPage(align = "center",
                
  # Horizontal layout with:
  # - the slider input for the bandwidth
  # - the select input for the distribution
  # - the select input for the sample size n
  verticalLayout(
    
    inputPanel(
      
      sliderInput(inputId = "h", label = "h:",
                  min = 0.01, max = 2, value = 1, step = 0.01),
      selectInput(inputId = "dist", label = "Density:",
                  choices = c("Normal", "Mixture", "Bart Simpson"), 
                  selected = "Normal"),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(50, 100, 250, 500, 1000), selected = 100)
      
    ),
    
    plotOutput("densityPlot")
    
  )

)

# Server logic
server <- function(input, output) {
  
  output$densityPlot <- renderPlot({
    
    # True density and distribution
    fTrue <- switch(input$dist,
                    "Normal" = dens[, 1],
                    "Mixture" = dens[, 2],
                    "Bart Simpson" = dens[, 3])
    FTrue <- switch(input$dist,
                    "Normal" = dist[, 1],
                    "Mixture" = dist[, 2],
                    "Bart Simpson" = dist[, 3])
    
    # Expectation
    h <- as.numeric(input$h)
    step <- h / 0.01
    Ef <- (FTrue[(201 + step):(201 + step + length(xGrid5) - 1)] - 
             FTrue[(201 - step):(201 - step + length(xGrid5) - 1)]) / (2 * h)
    
    
    # Variance
    n <- as.integer(input$n)
    Sdf <- sqrt(Ef * (1 - h * Ef) / (2 * n * h))
      
    # Plot
    plot(xGrid5, Ef, type = "n", xlab = "x", ylab = "Density",
         ylim = c(0, 0.8))
    polygon(x = c(xGrid5, rev(xGrid5)), y = Ef + qnorm(0.975) * c(Sdf, -Sdf), 
            col = "gray", border = NA)
    lines(xGrid5, Ef, lwd = 2)
    lines(xGrid7, fTrue, lwd = 3, col = 2)
    legend("topright", legend = c("True density", 
                                  expression("Expectation of "*hat(f)[N](h)),
                                  "Asymptotic 95% CI"),
           col = c(2, 1, "gray"), lwd = 2)
    
  }, width = 650, height = 650)
  
}

# Run the application
shinyApp(ui = ui, server = server)

# # Data
# library(nor1mix)
# xGrid5 <- seq(-5, 5, by = 0.01)
# xGrid7 <- seq(-7, 7, by = 0.01)
# 
# dens <- cbind(dnorm(xGrid7), 
#               dnorMix(x = xGrid7, obj = MW.nm7),
#               dnorMix(x = xGrid7, obj = MW.nm10))
# dist <- cbind(pnorm(xGrid7), 
#               pnorMix(q = xGrid7, obj = MW.nm7), 
#               pnorMix(q = xGrid7, obj = MW.nm10))
# 
# 
# save(list = c("xGrid5", "dens", "dist"), file = "xData.RData")
