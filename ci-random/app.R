#
# Shiny web application for illustrating the Confidence Interval (CI) for the
# intercept in the linear model, when the error variance is unkwnown
#

library(shiny)

# Settings experiment
M <- 100
n <- 100

# Predictor's data
x <- rnorm(n)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - the slider inputs for the intercept
  # - the slider inputs variance of error and predictor
  # - the sample options
  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get 100 new<br> samples!</h5>")),
      selectInput(inputId = "alpha", label = "alpha:",
                  choices = c("0.25", "0.10", "0.05", "0.01"), selected = "0.05"),
      sliderInput(inputId = "beta0", label = "Intercept:",
                  min = -1, max = 1, value = 0, step = 0.25)

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

    matrix(rnorm(M * n), nrow = M, ncol = n)

  })

  output$ciPlot <- renderPlot({

    # Check if the buttom was clicked
    if (values$default == 0) {

      set.seed(423432)
      error <- matrix(rnorm(M * n), nrow = M, ncol = n)

    } else {

      error <- eps()

    }

    # Response's data (slope = 1, sigma2 = 1)
    regX <- matrix(input$beta0 + x, nrow = M, ncol = n, byrow = TRUE)
    y <- regX + error

    # CIs
    a <- as.numeric(input$alpha)
    confs <- t(sapply(1:M, function(i) confint(object = lm(y[i, ] ~ x),
                                               parm = "(Intercept)",
                                               level = 1 - a)))
    out <- (input$beta0 < confs[, 1]) | (input$beta0 > confs[, 2])

    # Plot
    par(mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    plot(1:M, rep(input$beta0, M), xlab = "Confidence intervals",
         ylab = expression(beta[0]), type = "n", ylim = c(-1.5, 1.5),
         main = substitute(expr = "Times " * beta[0]
                           * " intercept is inside CI: " * m
                           * ". Expected: " * e * ".",
                           env = list(m = M - sum(out), e = M * (1 - a))),
                           cex.main = 1.5)
    segments(x0 = 1:M, y0 = confs[, 1], x1 = 1:M, y1 = confs[, 2],
             col = out + 1, lwd = out + 1)
    abline(h = input$beta0, lwd = 2, col = 3)
    legend("topright", legend = expression("True "*beta[0],
                                           "CI, "*beta[0]*" inside",
                                           "CI, "*beta[0]*" outside"),
           lwd = 2, col = c(3, 1, 2), cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

