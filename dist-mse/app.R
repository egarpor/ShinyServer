
#
# Shiny web application for illustrating the MSE on estimating a distribution
# function parametrically and nonparametrically
#

library(shiny)

# Load data
load("xData.RData")

# UI for application
ui <- fluidPage(title = "Nonparametric vs parametric estimation",
                align = "center",

  # Vertical layout with:
  # - a radio input for the sample size
  # - a slider input for Gamma's/Exponential's parameter lambda
  # - a slider input for Gamma's parameter p
  verticalLayout(

    inputPanel(

      radioButtons(inputId = "n", label = "Sample size:",
                   choices = c(50, 100, 250, 500), selected = 100,
                   inline = TRUE),
      sliderInput(inputId = "lambda", label = HTML("&lambda;:"),
                  min = 0.5, max = 3, value = 1, step = 0.5),
      sliderInput(inputId = "p", label = "p:",
                  min = 0.25, max = 2, value = 1, step = 0.25)

    ),

    plotOutput("msePlot")

  )

)

# Server logic
server <- function(input, output) {

  output$msePlot <- renderPlot({

    # Sample from a Gamma(a, p) = 1/a * Gamma(1, p)
    samp <- sampGamma[1:as.integer(input$n), , input$p %/% 0.25] / input$lambda

    # Densities
    FTrue <- pgamma(q = xGrid, shape = input$p, rate = input$lambda)
    FPar <- apply(samp[, 1:10], 2, function(s) ecdf(s)(xGrid))
    FNonpar <- apply(samp[, 1:10], 2,
                     function(s) pexp(q = xGrid, rate = 1 / mean(s)))

    # MSEs
    mseNp <- FTrue * (1 - FTrue) / as.integer(input$n)
    # Above expression is exact, no Monte Carlo is needed. Below if we wanted
    # the Monte Carlo way
    # mseNp <- rowMeans(apply(samp, 2, function(s) (ecdf(s)(xGrid) - FTrue)^2))
    mseExp <- rowMeans(apply(samp, 2, function(s) {

      (pexp(q = xGrid, rate = 1 / mean(s)) - FTrue)^2

    }))

    # For plot legends
    expr <- expression(F[n](x) * " (nonparametric)",
                       F(x * "; " * hat(lambda)[ML] * ", " * 1) *
                         " (parametric)")

    # Plots
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    plot(xGrid, FTrue, type = "l", lwd = 3, xlab = "x",
         ylab = expression(plain(P) * group("[", X <= x, "]")), ylim = c(0, 1))
    matlines(xGrid, cbind(FNonpar, FPar),
             col = rep(rgb(0:1, 0, 1:0, alpha = 0.5), each = 10), lty = 1)
    legend("bottomright",
           legend = c(expression(F(x * "; " * lambda * ", "  * p) *
                                   " (reality)"), expr),
           col = c(1, 2, 4), lwd = 2)
    matplot(xGrid, cbind(mseNp, mseExp), type = "l", pch = 1, lty = 1,
            xlab = "x", ylab = "MSE(x)", ylim = c(0, 0.01), col = c(2, 4))
    legend("topright", legend = expr, col = c(2, 4), lwd = 2)

  }, width = 650, height = 325)

}

# Run the application
shinyApp(ui = ui, server = server)

# # Data
# xGrid <- seq(0, 5, l = 5e2)
#
# # Sample
# M <- 5e2
# set.seed(1234567)
# sampGamma <- array(dim = c(500, M, 8))
# for (i in 1:8) {
#   sampGamma[, , i] <- matrix(rgamma(n = 500 * M, shape = 0.25 * i, rate = 1),
#                              nrow = 500, ncol = M)
# }
#
# # Save data
# save(list = c("xGrid", "sampGamma"), file = "xData.RData")
