
#
# Shiny web application for illustrating the Confidence Interval (CI) for the
# mean and response in the linear model, for different parameters
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
  # - select inputs for sample size, type of CI and significance level
  # - the slider inputs for the variance of error and predictor

  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(10, 50, 100, 200, 500), selected = 100),
      selectInput(inputId = "ciType", label = "CI type:",
                  choices = c("Mean", "Response"), selected = "Mean"),
      selectInput(inputId = "alpha", label = "alpha:",
                  choices = c("0.25", "0.10", "0.05", "0.01"),
                  selected = "0.05"),
      sliderInput(inputId = "sigma2", label = "Error variance:",
                  min = 0, max = 3, value = 1, step = 0.1),
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

  # Error sampling
  eps <- eventReactive(input$newSample, {

    rnorm(500)

  })

  # Cache error based on button clicks
  error <- reactive({

    if (values$default == 0) {

      set.seed(423432)
      rnorm(500)

    } else {

      eps()

    }

  })

  # Cache x data based on sigma2x and n
  x <- reactive({

    sqrt(input$sigma2x) * xData[1:input$n]

  })

  # Cache y data based on x, error, and sigma2
  y <- reactive({

    x_val <- x()
    error_val <- error()
    n_val <- as.integer(input$n)
    regX <- 0.5 + x_val
    regX + sqrt(input$sigma2) * error_val[1:n_val]

  })

  # Cache model fit based on x and y
  mod <- reactive({

    x_val <- x()
    y_val <- y()
    lm(y_val ~ x_val)

  })

  # Cache confidence intervals based on model, ciType, and alpha
  confs <- reactive({

    mod_val <- mod()
    interval <- switch(input$ciType,
                       Mean = "confidence",
                       Response = "prediction")
    a_val <- as.numeric(input$alpha)
    predict(mod_val, newdata = data.frame(x_val = xNew), level = 1 - a_val,
            interval = interval)

  })

  output$ciPlot <- renderPlot({

    # Get cached values
    x_val <- x()
    y_val <- y()
    mod_val <- mod()
    confs_val <- confs()

    # Plot
    par(mar = c(4, 4, 1, 1) + 0.1, oma = rep(0, 4))
    plot(x_val, y_val, xlim = c(-5, 5), ylim = c(-5, 5), pch = 16,
         xlab = "x", ylab = "y")
    abline(a = 0.5, b = 1, col = 1, lwd = 3)
    abline(mod_val$coefficients, col = 2, lwd = 3)
    segments(x0 = xNew[coarse], y0 = confs_val[coarse, 2], x1 = xNew[coarse],
             y1 = confs_val[coarse, 3], lwd = 2, col = blue)
    points(xNew[coarse], confs_val[coarse, 1], col = 2, pch = 16)
    lines(xNew, confs_val[, 2], col = blue, lty = 2, lwd = 2)
    lines(xNew, confs_val[, 3], col = blue, lty = 2, lwd = 2)
    legend("bottomright", legend = c("True regression", "Fitted regression",
                                     ifelse(input$ciType == "Mean",
                                            "CI for mean", "CI for response")),
           lwd = 3, col = c(1:2, blue), cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)
