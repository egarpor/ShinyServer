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
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - action buttom for generating a new sample
  # - select inputs for sample size and significance level
  # - the slider input for the variance of predictor

  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(10, 50, 100, 200, 500), selected = 100),
      selectInput(inputId = "alpha", label = "alpha:",
                  choices = c("0.25", "0.10", "0.05", "0.01"), selected = "0.05"),
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

  output$ciPlot <- renderPlot({

    # Response's data
    x <- sqrt(input$sigma2x) * xData[1:input$n]
    regX <- 1 + x
    regX <- 1 / (1 + exp(-regX))

    # Check if the buttom was clicked
    if (values$default == 0){

      set.seed(423432)
      y <- rbinom(n = input$n, size = 1, prob = regX)

    } else {

      y <- rbinom(n = input$n, size = 1, prob = regX)

    }

    # Model
    mod <- glm(y ~ x, family = "binomial")
    xx <- seq(-5, 5, l = 200)
    real <- 1 + xx
    real <- 1 / (1 + exp(-real))
    est <- predict(mod, newdata = data.frame(x = xx))
    est <- 1 / (1 + exp(-est))

    # CIs
    a <- as.numeric(input$alpha)
    confs <- predict(mod, newdata = data.frame(x = xNew), level = 1 - a,
                     se.fit = TRUE)
    confs$lower <- confs$fit + qnorm(p = a/2) * confs$se.fit
    confs$lower <- exp(confs$lower) / (1 + exp(confs$lower))
    confs$upper <- confs$fit - qnorm(p = a/2) * confs$se.fit
    confs$upper <- exp(confs$upper) / (1 + exp(confs$upper))
    confs$se.fit <- exp(confs$se.fit) / (1 + exp(confs$se.fit))

    # Plot
    par(mar = c(4, 4, 1, 1) + 0.1, oma = rep(0, 4))
    plot(x, y, xlim = c(-5, 5), ylim = c(-0.15, 1), pch = 16, xlab = "x", ylab = "y")
    lines(xx, est, col = 2, lwd = 3)
    lines(xx, real, col = 1, lwd = 3)
    segments(x0 = xNew[coarse], y0 = confs$lower[coarse], x1 = xNew[coarse],
             y1 = confs$upper[coarse], lwd = 2, col = blue)
    points(xNew[coarse], exp(confs$fit[coarse]) / (1 + exp(confs$fit[coarse])),
           col = 2, pch = 16)
    lines(xNew, confs$lower, col = blue, lty = 2, lwd = 2)
    lines(xNew, confs$upper, col = blue, lty = 2, lwd = 2)
    legend("bottomright", legend = c("True regression", "Fitted regression",
                                     "CI for mean"),
           lwd = 3, col = c(1:2, blue), cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

