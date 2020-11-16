#
# Shiny web application for illustrating the linear regression, principal
# components analysis, and partial least squares
#

library(shiny)
library(pls)
library(mvtnorm)
library(viridis)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - action button for generating a new sample
  # - select input for sample size
  # - the slider inputs for orientation and correlation

  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(100, 500, 1000, 2000), selected = 500),
      sliderInput(inputId = "theta", label = "Orientation vector:",
                  min = 0, max = 6.28, value = pi, step = 0.1),
      sliderInput(inputId = "rho", label = "Correlation:",
                  min = -1, max = 1, value = 0.5, step = 0.01)

    ),

    plotOutput("plsPlot")

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
  samp <- eventReactive(input$newSample, {

    list("x" = rmvnorm(n = 2e3, mean = c(0, 0), sigma = diag(c(1, 1))),
         "eps" = rnorm(n = 2e3))

  })

  output$plsPlot <- renderPlot({

    # Check if the button was clicked
    if (values$default == 0) {

      set.seed(123456)
      x <- rmvnorm(n = 2e3, mean = c(0, 0), sigma = diag(c(1, 1)))
      eps <- rnorm(n = 2e3)

    } else {

      first <- samp()
      x <- first$x
      eps <- first$eps

    }

    # Simulate (X1, X2) from a N(0, Sigma)
    # Standard deviations set to 1 since we standardize the data afterwards
    Sigma <- rbind(c(1, input$rho),
                   c(input$rho, 1))
    eigSigma <- eigen(Sigma, symmetric = TRUE)
    sqrtSigma <- eigSigma$vectors %*% diag(sqrt(eigSigma$values)) %*%
      t(eigSigma$vectors)
    n <- as.integer(input$n)
    x <- x[1:n, ] %*% sqrtSigma

    # Center and standardize the sample
    # x <- scale(x, center = TRUE, scale = TRUE)

    # Simulate the linear response
    beta <- c(cos(input$theta), sin(input$theta))
    y <- x %*% beta + eps[1:n]

    # Compute PC directions, avoiding flipping signs (the first loading is
    # forced to be always positive)
    pc <- princomp(x, fix_sign = TRUE)

    # Compute PLS directions
    pls <- plsr(y ~ x)

    # Avoid flipping signs by forcing the first loading to be always positive
    # and changing the scores
    signs <- sign(pls$loadings[1, ])
    pls$loadings <- t(t(pls$loadings) * signs)
    pls$scores <- t(t(pls$scores) * signs)

    # Plot the (X1, X2) sample, with color gradient according to the linear
    # trend x %*% beta
    col <- viridis(n)[rank(x %*% beta)]
    plot(x = x[, 1], y = x[, 2], col = col, pch = 16,
         xlim = c(-4, 4), ylim = c(-4, 4), xlab = "X1", ylab = "X2")

    # Draw the beta direction indicating the growth direction of the regression
    # plane and the direction of the color gradient
    arrows(x0 = 0, y0 = 0, x1 = 2 * beta[1], y1 = 2 * beta[2], col = 1, lwd = 4)

    # Draw the PC directions
    arrows(x0 = 0, y0 = 0, x1 = 2 * pls$loadings[1, ], y1 = 2 * pls$loadings[2, ],
           col = 4, lwd = 4, lty = 1:2)

    # Draw the PLS directions
    arrows(x0 = 0, y0 = 0, x1 = 2 * pc$loadings[1, ], y1 = 2 * pc$loadings[2, ],
           col = 2, lwd = 4, lty = 1:2)

    # Add legend
    legend("top", legend = expression(beta, "PC 1", "PC 2", "PLS 1", "PLS 2"),
           lwd = 2, col = c(1, 4, 4, 2, 2), lty = c(1, 1, 2, 1, 2))

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)
