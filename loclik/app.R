
#
# Shiny web application for illustrating the construction of local likelihood
# estimation for logistic regression
#

library(shiny)

# Load data
load("loclik.RData")
m <- function(x, type) {
  switch(type,
         "1" = 0.5,
         "2" = logistic(1 + 2 * x),
         "3" = 0.75 * logistic(1 + 2 * x) + 0.25 * logistic(2 - 3 * x),
         "4" = cos(x)^2,
         "5" = logistic(3 * sin(x)))
}
getSamp <- function(n, type) {

  X <- runif(n, -4, 4)
  Y <- rbinom(n = n, size = 1, prob = m(X, type = type))
  list("X" = X, "Y" = Y)

}

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - a select input for the sample size
  # - a select input for the regression function
  # - a slider input for the bandwidth
  # - a checkbox input for the degree
  # - a checkbox input for selecting the curves to display
  # - a slider input for the evaluation point x

  verticalLayout(

    inputPanel(

      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(50, 100, 250, 500, 1000), selected = 100),
      selectInput(inputId = "reg", label = "Regression function:",
                  choices = c("Constant" = 1, "Logistic" = 2,
                              "Logistic mixture" = 3, "Cosine" = 4,
                              "Sine" = 5), selected = 3),
      sliderInput(inputId = "h", label = "Bandwidth h:",
                  min = 0.1, max = 1.9, value = 0.5, step = 0.1),
      checkboxGroupInput(inputId = "degree", label = "Degree:",
                         choices = 0:2, selected = 1, inline = TRUE),
      checkboxGroupInput(inputId = "disp", label = "Fit to show:",
                         choices = c("Global", "Local"),
                         selected = c("Global", "Local"), inline = TRUE),
      sliderInput(inputId = "x", label = "x:",
                  min = -4, max = 4, value = 0, step = 0.1)

    ),

    plotOutput("loclikPlot")

  )

)

# Server logic
server <- function(input, output) {

  output$loclikPlot <- renderPlot({

    # Sample
    set.seed(1234567)
    n <- as.integer(input$n)
    samp <- getSamp(n = n, type = input$reg)
    ni <- which(n == c(50, 100, 250, 500, 1000))

    # True regression
    regi <- as.integer(input$reg)
    mTrue <- mGrid[, regi]

    # Bandwidth
    h <- as.numeric(input$h)
    hi <- h / 0.1

    # Degree of the fits
    degree <- 0:3 %in% as.integer(input$degree)

    # Color according to density strength
    strength <- dnorm(samp$X, mean = input$x, sd = h)
    colKde <- gray(level = 1 - pmin(strength, 1))

    # Data and true regression
    par(mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    plot(samp$X, samp$Y, xlab = "x", ylab = "y",
         xlim = c(-4, 4), ylim = c(0, 1.25))
    points(samp$X, samp$Y, pch = 16, col = colKde)
    lines(xGrid, mTrue, lwd = 3, col = 2)

    # points(input$x, m(input$x, type = input$reg),
    #        pch = 19, cex = 1.25, col = 2)

    # Kernel
    kde <- dnorm(xGrid, mean = input$x, sd = h)
    lines(xGrid, kde, col = "gray")

    # Compute the local likelihood estimators

    # Index giving the corresponding point to input$x in xGrid
    xi <- (input$x + 4) / 0.01 + 1

    # Plot the local likelihood estimators. For all of them, we show the
    # local likelihood estimate (with color intensities according to kde)
    # and the global likelihood estimate
    if (degree[1]) {

      reg0 <- reg[ni, regi, hi, 1, xi]
      points(input$x, logistic(reg0), pch = 19, cex = 1.25, col = rgb(0, 1, 0))
      y <- logistic(rep(reg0, lGrid))

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(0, 1, 0, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, logistic(reg[ni, regi, hi, 1, ]), lwd = 2,
              col = rgb(0, 1, 0))

      }

    }
    if (degree[2]) {

      reg1 <- reg[ni, regi, hi, 2:3, xi]
      points(input$x, logistic(reg1[1]), pch = 19, cex = 1.25,
             col = rgb(0, 0, 1))
      y <- logistic(reg1[1] + reg1[2] * (xGrid - input$x))

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(0, 0, 1, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, logistic(reg[ni, regi, hi, 2, ]), lwd = 2,
              col = rgb(0, 0, 1))

      }

    }
    if (degree[3]) {

      reg2 <- reg[ni, regi, hi, 4:6, xi]
      points(input$x, logistic(reg2[1]), pch = 19, cex = 1.25,
             col = rgb(0.63, 0.13, 0.94))
      y <- logistic(reg2[1] + reg2[2] * (xGrid - input$x) +
                      reg2[3] * (xGrid - input$x)^2)

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(0.63, 0.13, 0.94, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, logistic(reg[ni, regi, hi, 4, ]), lwd = 2,
              col = rgb(0.63, 0.13, 0.94))

      }

    }

    # Legend
    legend("topright", legend = c("True regression", "Kernel at x",
                                  c("Local constant", "Local linear",
                                    "Local quadratic", "Local cubic")[degree]),
           col = c(2, "gray", rgb(c(0, 0, 0.63, 1), c(1, 0, 0.13, 0.65),
                                  c(0, 1, 0.94, 0))[degree]),
           lwd = 2)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

# # Load data
# xGrid <- seq(-4, 4, by = 0.01)
# logistic <- function(z) 1 / (1 + exp(-z))
# m <- function(x, type) {
#   switch(type,
#          "1" = 0.5,
#          "2" = logistic(1 + 2 * x),
#          "3" = 0.75 * logistic(1 + 2 * x) + 0.25 * logistic(2 - 3 * x),
#          "4" = cos(x)^2,
#          "5" = logistic(3 * sin(x)))
# }
# mGrid <- cbind(m(xGrid, 1), m(xGrid, 2), m(xGrid, 3),
#                m(xGrid, 4), m(xGrid, 5))
# lGrid <- length(xGrid)
#
# # Samples
# getSamp <- function(n, type) {
#
#   X <- runif(n, -4, 4)
#   Y <- rbinom(n = n, size = 1, prob = m(X, type = type))
#
#   return(list("X" = X, "Y" = Y))
#
# }
#
# # Precompute regressions
# reg <- array(dim = c(5, 5, 19, 6, 801)) # n, scenario, h, degree, xGrid
# n <- c(50, 100, 250, 500, 1000)
# h <- seq(0.1, 1.9, by = 0.1)
# pb <- txtProgressBar(style = 3)
# for (ni in 1:5) {
#   for (regi in 1:5) {
#
#     # Sample
#     set.seed(1234567)
#     samp <- getSamp(n = n[ni], type = regi)
#
#     for (hi in 1:19) {
#
#       # Compute the regressions for all degrees
#       reg[ni, regi, hi, , ] <- sapply(xGrid, function(x) {
#
#         w <- dnorm(samp$X, mean = x, sd = h[hi])
#         X <- cbind(1, samp$X - x, (samp$X - x)^2, (samp$X - x)^3)
#         c(tryCatch(glm.fit(x = X[, 1], y = samp$Y, weights = w,
#                            family = binomial())$coefficients,
#                    error = function(e) rep(NA, 1)),
#           tryCatch(glm.fit(x = X[, 1:2], y = samp$Y, weights = w,
#                            family = binomial())$coefficients,
#                    error = function(e) rep(NA, 2)),
#           tryCatch(glm.fit(x = X[, -4], y = samp$Y, weights = w,
#                            family = binomial())$coefficients,
#                    error = function(e) rep(NA, 3))
#         )})
#
#     }
#
#     setTxtProgressBar(pb = pb, value = (regi + (ni - 1) * 5) / 25)
#
#   }
#
# }
# rm(m, getSamp, n, h, pb)
# save(list = ls(), file = "loclik.RData")
