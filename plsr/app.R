
#
# Shiny web application for illustrating the linear regression, principal
# components analysis, and partial least squares
#

library(shiny)
library(pls)
library(mvtnorm)
library(viridis)

# UI for application
ui <- fluidPage(title = paste("Partial least squares and",
                              "principal component regression"),
                align = "center",

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
      sliderInput(inputId = "theta", label = "Direction vector:",
                  min = 0, max = 6.28, value = pi, step = 0.1),
      sliderInput(inputId = "rho", label = "Correlation:",
                  min = -0.99, max = 0.99, value = 0.5, step = 0.01)

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

  # Use eventReactive for button-triggered sample regeneration
  samp_event <- eventReactive(input$newSample, {

    list("x" = rmvnorm(n = 2e3, mean = c(0, 0), sigma = diag(c(1, 1))),
         "eps" = rnorm(n = 2e3))

  })

  # Cache sample based on button clicks
  samp_data <- reactive({

    if (values$default == 0) {

      set.seed(123456)
      list("x" = rmvnorm(n = 2e3, mean = c(0, 0), sigma = diag(c(1, 1))),
           "eps" = rnorm(n = 2e3))

    } else {

      samp_event()
    }

  })

  # Cache correlation matrix square root based on rho
  sqrtSigma <- reactive({

    rho_val <- input$rho
    Sigma <- rbind(c(1, rho_val),
                   c(rho_val, 1))
    eigSigma <- eigen(Sigma, symmetric = TRUE)
    eigSigma$vectors %*% diag(sqrt(eigSigma$values)) %*%
      t(eigSigma$vectors)

  })

  # Cache transformed sample data based on samp, sqrtSigma, and n
  x_transformed <- reactive({

    samp_raw <- samp_data()
    sqrtSigma_val <- sqrtSigma()
    n_val <- as.integer(input$n)
    samp_raw$x[1:n_val, ] %*% sqrtSigma_val

  })

  # Cache response based on x, theta, and eps
  y <- reactive({

    x_val <- x_transformed()
    n_val <- as.integer(input$n)
    samp_raw <- samp_data()
    beta <- c(cos(input$theta), sin(input$theta))
    x_val %*% beta + samp_raw$eps[1:n_val]

  })

  # Cache PCA results based on x
  pc <- reactive({

    x_val <- x_transformed()
    princomp(x_val, fix_sign = TRUE)

  })

  # Cache PLS results based on x and y
  pls_raw <- reactive({

    x_val <- x_transformed()
    plsr(y() ~ x_val)

  })

  # Cache processed PLS (with sign adjustments)
  pls <- reactive({

    pls_result <- pls_raw()
    signs <- sign(pls_result$loadings[1, ])
    pls_result$loadings <- t(t(pls_result$loadings) * signs)
    pls_result$scores <- t(t(pls_result$scores) * signs)
    pls_result

  })

  output$plsPlot <- renderPlot({

    # Get cached values
    x_val <- x_transformed()
    y_val <- y()
    pc_result <- pc()
    pls_result <- pls()
    n_val <- as.integer(input$n)
    beta <- c(cos(input$theta), sin(input$theta))

    # Plot the (X1, X2) sample, with color gradient according to the linear
    # trend x %*% beta
    col <- viridis(n_val)[rank(x_val %*% beta)]
    par(mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(x = x_val[, 1], y = x_val[, 2], col = col, pch = 16,
         xlim = c(-4, 4), ylim = c(-4, 4), xlab = "X1", ylab = "X2")

    # Draw the beta direction indicating the growth direction of the regression
    # plane and the direction of the color gradient
    arrows(x0 = 0, y0 = 0, x1 = 2 * beta[1], y1 = 2 * beta[2],
           col = 1, lwd = 4)

    # Draw the PC directions
    arrows(x0 = 0, y0 = 0, x1 = 2 * pc_result$loadings[1, ],
           y1 = 2 * pc_result$loadings[2, ], col = 2, lwd = 4, lty = 1:2)

    # Draw the PLS directions
    arrows(x0 = 0, y0 = 0, x1 = 2 * pls_result$loadings[1, ],
           y1 = 2 * pls_result$loadings[2, ], col = 4, lwd = 4, lty = 1:2)

    # Add legend
    legend("top", legend = expression(beta, "PC1", "PC2", "PLS1", "PLS2"),
           lwd = 2, col = c(1, 2, 2, 4, 4), lty = c(1, 1, 2, 1, 2))

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)
