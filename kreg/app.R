
#
# Shiny web application for illustrating the construction of nonparametric
# kernel regression
#

library(shiny)
library(nor1mix)

# Load data
xGrid <- seq(-4, 4, by = 0.01)
m <- function(x, type) {

  switch(type,
         "1" = 5,
         "2" = 4.5 + 0.75 * x,
         "3" = 3 - 0.75 * x + 0.25 * x^2,
         "4" = 5 + x * sin(x),
         "5" = 4 + 2 * sqrt(abs(x)) * sin(1.1 * pi / (abs(x) + 0.3)))

}
mGrid <- cbind(m(xGrid, 1), m(xGrid, 2), m(xGrid, 3), m(xGrid, 4), m(xGrid, 5))
lGrid <- length(xGrid)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - an action button for generating a new sample
  # - a select input for the regression function
  # - a slider input for the bandwidth
  # - a radio input for the kernel
  # - a checkbox input for the degree
  # - a checkbox input for selecting the curves to display
  # - a slider input for the evaluation point x

  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      selectInput(inputId = "reg", label = "Regression function:",
                  choices = c("Constant" = 1, "Linear" = 2,
                              "Quadratic" = 3, "Non-linear I" = 4,
                              "Non-linear II" = 5), selected = 3),
      sliderInput(inputId = "h", label = "Bandwidth h:",
                  min = 0.1, max = 1.9, value = 0.5, step = 0.1),
      radioButtons(inputId = "kernel", label = "Kernel:",
                   choices = c("Gaussian", "Rectangular"),
                   selected = "Gaussian", inline = TRUE),
      checkboxGroupInput(inputId = "degree", label = "Degree:",
                         choices = 0:3, selected = 1, inline = TRUE),
      checkboxGroupInput(inputId = "disp", label = "Fit to show:",
                         choices = c("Global", "Local"),
                         selected = c("Global", "Local"), inline = TRUE),
      sliderInput(inputId = "x", label = "x:",
                  min = -4, max = 4, value = 0, step = 0.1)

    ),

    plotOutput("kregPlot")

  )

)

# Server logic
server <- function(input, output) {

  # Manage the first call
  values <- reactiveValues(default = 0)
  observeEvent(input$newSample, {

    values$default <- input$newSample

  })

  # Sampling
  getSamp <- function() {

    X <- switch(input$reg,
                "1" = rnorm(n = 200),
                "2" = rnorMix(n = 200, obj = MW.nm7),
                "3" = rnorm(n = 200, sd = 1.5),
                "4" = rnorMix(n = 200, obj = MW.nm7),
                "5" = rnorm(n = 200))
    e <- rnorm(n = 200, sd = 0.75)
    Y <- m(X, type = input$reg) + e
    return(list("X" = X, "Y" = Y))

  }
  getReactSamp <- eventReactive(input$newSample, getSamp())

  output$kregPlot <- renderPlot({

    # Check if the button was clicked
    if (values$default == 0) {

      set.seed(423432)
      samp <- getSamp()

    } else {

      samp <- getReactSamp()

    }

    # True regression
    mTrue <- mGrid[, as.integer(input$reg)]

    # Prepare for plot
    h <- as.numeric(input$h)
    degree <- 0:3 %in% as.integer(input$degree)
    K <- switch(input$kernel,
                "Gaussian" = dnorm,
                "Rectangular" = function(x) -1 < x & x < 1)

    # Color according to density strength
    strength <- K((samp$X - input$x) / h) / h
    colKde <- gray(level = 1 - pmin(strength, 1))

    # Data and true regression
    par(mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    plot(samp$X, samp$Y, xlab = "x", ylab = "y",
         xlim = c(-4, 4), ylim = c(0, 10))
    points(samp$X, samp$Y, pch = 16, col = colKde)
    lines(xGrid, mTrue, lwd = 3, col = 2)
    points(input$x, m(input$x, input$reg), pch = 19, cex = 1.25, col = 2)

    # Kernel
    kde <- K((xGrid - input$x) / h) / h
    lines(xGrid, kde, col = "gray",
          type = ifelse(input$kernel == "Rectangular", "s", "l"))

    # Compute the local regression estimators
    reg <- t(sapply(xGrid, function(x) {

      w <- K((samp$X - x) / h) / h
      X <- cbind(1, samp$X - x, (samp$X - x)^2, (samp$X - x)^3)
      c(weighted.mean(x = samp$Y, w = w),
        lm.wfit(x = X[, 1:2], y = samp$Y, w = w)$coefficients,
        lm.wfit(x = X[, -4], y = samp$Y, w = w)$coefficients,
        lm.wfit(x = X, y = samp$Y, w = w)$coefficients)

    }))

    # Index giving the corresponding point to input$x in xGrid
    xi <- (input$x + 4) / 0.01 + 1

    # Plot the local regression estimators. For all of them, we show the
    # local regression estimate (with color intensities according to kde)
    # and the global regression estimate
    if (degree[1]) {

      reg0 <- reg[xi, 1]
      points(input$x, reg0, pch = 19, cex = 1.25, col = rgb(0, 1, 0))
      y <- rep(reg0, lGrid)

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(0, 1, 0, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, reg[, 1], lwd = 2, col = rgb(0, 1, 0))

      }

    }
    if (degree[2]) {

      reg1 <- reg[xi, 2:3]
      points(input$x, reg1[1], pch = 19, cex = 1.25, col = rgb(0, 0, 1))
      y <- reg1[1] + reg1[2] * (xGrid - input$x)

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(0, 0, 1, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, reg[, 2], lwd = 2, col = rgb(0, 0, 1))

      }

    }
    if (degree[3]) {

      reg2 <- reg[xi, 4:6]
      points(input$x, reg2[1], pch = 19, cex = 1.25,
             col = rgb(0.63, 0.13, 0.94))
      y <- reg2[1] + reg2[2] * (xGrid - input$x) +
        reg2[3] * (xGrid - input$x)^2

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(0.63, 0.13, 0.94, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, reg[, 4], lwd = 2, col = rgb(0.63, 0.13, 0.94))

      }

    }
    if (degree[4]) {

      reg3 <- reg[xi, 7:10]
      points(input$x, reg3[1], pch = 19, cex = 1.25, col = rgb(1, 0.65, 0))
      y <- reg3[1] + reg3[2] * (xGrid - input$x) +
        reg3[3] * (xGrid - input$x)^2 + reg3[4] * (xGrid - input$x)^3

      if ("Local" %in% input$disp) {

        segments(x0 = xGrid[-lGrid], y0 = y[-lGrid],
                 x1 = xGrid[-1L], y1 = y[-1L],
                 col = rgb(1, 0.65, 0, alpha = pmax(kde / max(kde), 0.1)),
                 lwd = 2)

      }
      if ("Global" %in% input$disp) {

        lines(xGrid, reg[, 7], lwd = 2, col = rgb(1, 0.65, 0))

      }

    }

    # Legend
    legend("topright", legend = c("True regression", "Kernel at x",
                                  c("Local constant", "Local linear",
                                    "Local quadratic", "Local cubic")[degree]),
           col = c(2, "gray", rgb(c(0, 0, 0.63, 1), c(1, 0, 0.13, 0.65),
                                  c(0, 1, 0.94, 0))[degree]),
           lwd = 2)
    rug(samp$X, col = "gray")

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)
