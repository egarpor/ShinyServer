#
# Shiny web application for illustrating the ANOVA decomposition and its
# dependence on the error standard deviation
#

library(shiny)

# Data
n <- 10
x <- seq(-3, 3, l = n)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - action buttom for generating a new sample
  # - the slider inputs for the error standard deviation
  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample", label = HTML("<h5>Get a new<br> sample!</h5>")),
      sliderInput(inputId = "sigma", label = "Error standard deviation:",
                  min = 0, max = 3, value = 1, step = 0.1)

    ),

    plotOutput("anovaPlot")

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

    rnorm(n)

  })

  output$anovaPlot <- renderPlot({

    # Check if the buttom was clicked
    if (values$default == 0) {

      set.seed(423432)
      error <- rnorm(n)

    } else {

      error <- eps()

    }

    # Generate linear model
    y <- 0.25 * x + input$sigma * error
    mod <- lm(y ~ x)
    my <- mean(y)

    # Plot
    plot(x, y, xlim = c(-3.5, 3.5), ylim = c(-5, 5), pch = 16, type = "n")
    abline(h = my, col = 2, lty = 2)
    d <- 0.05
    segments(x0 = x - d, y0 = my, x1 = x - d, y1 = y, col = 4, lwd = 3)
    segments(x0 = x + d, y0 = my, x1 = x + d, y1 = mod$fitted.values,
             col = "forestgreen", lwd = 3)
    segments(x0 = x, y0 = y, x1 = x, y1 = mod$fitted.values,
             col = "orange", lwd = 3)
    abline(mod$coefficients, col = 2, lwd = 3)
    points(x, y, pch = 16, cex = 1.5)
    legend("topleft", legend = expression("Fitted line", "Sample mean " * bar(Y),
                                          (Y[i] - bar(Y))^2, (hat(Y)[i] - bar(Y))^2,
                                          (hat(Y)[i] - Y[i])^2),
           col = c(2, 2, 4, "forestgreen", "orange"), lty = c(1, 2, 1, 1, 1),
           lwd = c(2, 1, 3, 3, 3), cex = 1.5)

    # Info for title
    sst <- sum((y - my)^2)
    ssr <- sum((mod$fitted.values - my)^2)
    sse <- sum((y - mod$fitted.values)^2)
    R2 <- sprintf("%.2f", ssr / sst)
    sst <- paste("SST(", sprintf("%.2f", sst), ")", sep = "")
    ssr <- paste("SSR(", sprintf("%.2f", ssr), ")", sep = "")
    sse <- paste("SSE(", sprintf("%.2f", sse), ")", sep = "")

    # Coloured title
    title(main = substitute(expr = sst * phantom(" = " * ssr * " + " * sse) *
                              phantom(", " * R^2 * " = " * R2),
                            list(sst = sst, ssr = ssr, sse = sse, R2 = R2)),
          col.main = 4, cex.main = 1.5)
    title(main = substitute(expr = phantom(sst * " = ") * ssr *
                              phantom(" + " * sse * ", " * R^2 * " = " * R2),
                            list(sst = sst, ssr = ssr, sse = sse, R2 = R2)),
          col.main = "forestgreen", cex.main = 1.5)
    title(main = substitute(expr = phantom(sst * " = " * ssr * " + ") * sse *
                              phantom(", " * R^2  * " = " * R2),
                            list(sst = sst, ssr = ssr, sse = sse, R2 = R2)),
          col.main = "orange", cex.main = 1.5)
    title(main = substitute(expr = phantom(sst) * " = " * phantom(ssr) * " + " *
                              phantom(sse) * ", " * R^2  * " = " * R2,
                            list(sst = sst, ssr = ssr, sse = sse, R2 = R2)),
          col.main = "black", cex.main = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)








