
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
  # - a select input for the sample size
  # - a radio input for the distribution
  # - a slider input for the bandwidth

  verticalLayout(

    inputPanel(

      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(50, 100, 250, 500, 1000), selected = 100),
      radioButtons(inputId = "dist", label = "Density:",
                  choices = c("Normal" = 1, "Mixture" = 2, "Claw" = 3),
                  selected = 1, inline = TRUE),
      sliderInput(inputId = "h", label = "Bandwidth h:",
                  min = 0.01, max = 2, value = 1, step = 0.01)

    ),

    plotOutput("densityPlot")

  )

)

# Server logic
server <- function(input, output) {

  output$densityPlot <- renderPlot({

    # True density and distribution
    d <- as.integer(input$dist)
    fTrue <- dens[, d]
    FTrue <- dist[, d]

    # Expectation
    h <- as.numeric(input$h)
    step <- h / 0.01
    ind <- 201:(201 + 1001 - 1)
    Ef <- (FTrue[ind + step] - FTrue[ind - step]) / (2 * h)

    # Variance
    n <- as.integer(input$n)
    Sdf <- sqrt(Ef * (1 - h * Ef) / (2 * n * h))

    # Plot
    par(mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    plot(xGrid5, Ef, type = "n", xlab = "x", ylab = "Density",
         ylim = c(0, 0.8))
    polygon(x = c(xGrid5, rev(xGrid5)), y = Ef + qnorm(0.975) * c(Sdf, -Sdf),
            col = "gray", border = NA)
    lines(xGrid5, Ef, lwd = 2)
    lines(xGrid7, fTrue, lwd = 3, col = 2)
    legend("topright",
           legend = c(expression(f(x)),
                      expression("Expectation of "*hat(f)[N](x,h)),
                      expression("Asymptotic 95% CI for "*f(x))),
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
# # Save data
# save(list = c("xGrid5", "xGrid7", "dens", "dist"), file = "xData.RData")
