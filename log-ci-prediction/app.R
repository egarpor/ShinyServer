
#
# Shiny web application for illustrating the Confidence Interval (CI) for the
# mean and response in the logistic model, for different parameters
#

library(shiny)

# Load predictor's data
load("xData.RData")

# x grids
xNew <- seq(-5, 5, l = 210)
coarse <- seq(10, 200, by = 10)
blue <- rgb(0, 0, 1, alpha = 0.75)

# UI for application
ui <- fluidPage(title = "Confidence intervals for prediction",
                align = "center",

  # Vertical layout with:
  # - action button for generating a new sample
  # - select inputs for sample size and significance level
  # - the slider input for the variance of predictor

  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(10, 50, 100, 200, 500), selected = 100),
      selectInput(inputId = "alpha", label = "alpha:",
                  choices = c("0.25", "0.10", "0.05", "0.01"),
                  selected = "0.05"),
      sliderInput(inputId = "sigma2x", label = "Predictor variance:",
                  min = 0.1, max = 3, value = 1, step = 0.1)

    ),

    plotOutput("ciPlot")

  )

)

# Server logic
server <- function(input, output) {

  # Manage the first call
  values <- reactiveValues(default = 0)
  observeEvent(input$newSample, {

    values$default <- input$newSample

  })

  # Cache x data based on sigma2x and n
  x <- reactive({

    sqrt(input$sigma2x) * xData[1:input$n]

  })

  # Cache regX (true probability) based on x
  regX <- reactive({

    x_val <- x()
    regX_val <- 1 + x_val
    1 / (1 + exp(-regX_val))

  })

  # Cache y sample based on regX and newSample button
  y <- reactive({

    regX_val <- regX()
    n_val <- as.integer(input$n)
    if (values$default == 0) {

      set.seed(423432)
      rbinom(n = n_val, size = 1, prob = regX_val)

    } else {

      rbinom(n = n_val, size = 1, prob = regX_val)

    }

  })

  # Cache model fit based on x and y
  mod <- reactive({

    x_val <- x()
    y_val <- y()
    glm(y_val ~ x_val, family = "binomial")

  })

  # Cache predictions and true curve
  xx <- reactive(seq(-5, 5, l = 200))
  real_curve <- reactive({

    xx_val <- xx()
    real_val <- 1 + xx_val
    1 / (1 + exp(-real_val))

  })
  est_curve <- reactive({

    mod_val <- mod()
    xx_val <- xx()
    est_val <- predict(mod_val, newdata = data.frame(x_val = xx_val))
    1 / (1 + exp(-est_val))

  })

  # Cache confidence intervals based on model and alpha
  confs <- reactive({

    mod_val <- mod()
    x_val <- x()
    a_val <- as.numeric(input$alpha)
    confs_result <- predict(mod_val, newdata = data.frame(x_val = xNew),
                            level = 1 - a_val, se.fit = TRUE)
    confs_result$lower <- confs_result$fit + qnorm(p = a_val / 2) *
      confs_result$se.fit
    confs_result$lower <- exp(confs_result$lower) /
      (1 + exp(confs_result$lower))
    confs_result$upper <- confs_result$fit - qnorm(p = a_val / 2) *
      confs_result$se.fit
    confs_result$upper <- exp(confs_result$upper) /
      (1 + exp(confs_result$upper))
    confs_result$se.fit <- exp(confs_result$se.fit) /
      (1 + exp(confs_result$se.fit))
    confs_result

  })

  output$ciPlot <- renderPlot({

    # Get cached values
    x_val <- x()
    y_val <- y()
    mod_val <- mod()
    xx_val <- xx()
    real_val <- real_curve()
    est_val <- est_curve()
    confs_val <- confs()

    # Plot
    par(mar = c(4, 4, 1, 1) + 0.1, oma = rep(0, 4))
    plot(x_val, y_val, xlim = c(-5, 5), ylim = c(-0.15, 1), pch = 16,
         xlab = "x", ylab = "y")
    lines(xx_val, est_val, col = 2, lwd = 3)
    lines(xx_val, real_val, col = 1, lwd = 3)
    segments(x0 = xNew[coarse], y0 = confs_val$lower[coarse], x1 = xNew[coarse],
             y1 = confs_val$upper[coarse], lwd = 2, col = blue)
    points(xNew[coarse], exp(confs_val$fit[coarse]) /
             (1 + exp(confs_val$fit[coarse])),
           col = 2, pch = 16)
    lines(xNew, confs_val$lower, col = blue, lty = 2, lwd = 2)
    lines(xNew, confs_val$upper, col = blue, lty = 2, lwd = 2)
    legend("bottomright", legend = c("True regression", "Fitted regression",
                                     "CI for mean"),
           lwd = 3, col = c(1:2, blue), cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)
