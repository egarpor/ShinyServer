#
# Shiny web application for illustrating the construction of the transformed
# kernel density estimator
#

library(shiny)

# UI for application
ui <- fluidPage(align = "center",
                
  # Horizontal layout with:
  # - an action buttom for generating a new sample
  # - a slider input for the sample size
  # - a radio input for the distribution
  # - a slider input for the bandwidth
  # - a select input for the transformation
  
  verticalLayout(
    
    inputPanel(
      
      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      sliderInput(inputId = "n", label = "Sample size:",
                  min = 1, max = 300, value = 9, step = 2),
      selectInput(inputId = "dist", label = "Density:",
                   choices = c("Lognormal" = 1, "Lognormal mixt." = 2, 
                               "Beta" = 3, "Beta mixt." = 4), 
                   selected = 1),
      sliderInput(inputId = "h", label = "Bandwidth h:",
                  min = 0.01, max = 2, value = 0.5, step = 0.05),
      uiOutput(outputId = "transf")
      
    ),
    
    plotOutput("kdeTransfPlot")
    
  )

)

# Server logic
server <- function(input, output) {
  
  # Manage the first call
  values <- reactiveValues(default = 0)
  observeEvent(input$newSample, {
    
    values$default <- input$newSample
    
  })
  
  # Ban certain transformations
  output$transf <- renderUI({
    
    # Select data and caption
    choices <- switch(input$dist,
                    "1" = c("Log"),
                    "2" = c("Log"),
                    "3" = c("Log", "Probit"),
                    "4" = c("Log", "Probit"))
    selectInput(inputId = "transf", label = "Transformation:",
                choices = c("None", choices), selected = "None")
    
  })
  
  # Sampling
  getSamp <- function() switch(input$dist,
                               "1" = rlnorm(n = 300),
                               "2" = rlnorm(n = 300, meanlog = 0:1,
                                            sdlog = c(1, 0.5)),
                               "3" = rbeta(n = 300, shape1 = 1, shape2 = 2),
                               "4" = rbeta(n = 300, shape1 = 1, shape2 = 2:1))
  getReactSamp <- eventReactive(input$newSample, getSamp())
  
  output$kdeTransfPlot <- renderPlot({
    
    # Check if the buttom was clicked
    if (values$default == 0) {
      
      set.seed(423432)
      samp <- getSamp()
      
    } else {
      
      samp <- getReactSamp()
      
    }
    
    # True density
    fTrue <- switch(input$dist,
                    "1" = function(x) dlnorm(x),
                    "2" = function(x) 0.5 * dlnorm(x) + 
                      0.5 * dlnorm(x, meanlog = 1, sdlog = 0.5),
                    "3" = function(x) dbeta(x, shape1 = 1, shape2 = 2),
                    "4" = function(x) 0.5 * dbeta(x, shape1 = 1, shape2 = 2) +
                      0.5 * dbeta(x, shape1 = 2, shape2 = 2))
    # Load data
    xGrid <- switch(input$transf,
                    "None" = seq(-4, 4, length.out = 1e3),
                    "Log" = seq(exp(-4), exp(4), length.out = 1e3),
                    "Probit" = seq(pnorm(-4), pnorm(4), length.out = 1e3))
    
    # Transformation
    transf <- switch(input$transf,
                     "None" = function(x) x,
                     "Log" = function(x) log(x),
                     "Probit" = function(x) qnorm(x))
    transfInv <- switch(input$transf,
                        "None" = function(x) x,
                        "Log" = function(x) exp(x),
                        "Probit" = function(x) pnorm(x))
    transfDer <- switch(input$transf,
                        "None" = function(x) 1,
                        "Log" = function(x) 1 / x,
                        "Probit" = function(x) 1 / dnorm(qnorm(x)))
    
    # Prepare for plot
    n <- as.integer(input$n)
    h <- as.numeric(input$h)
    samp <- samp[1:n]
    
    # Plot
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    
    # Untransform kde$x so the grid is in (0, infty)
    kde <- density(transf(samp), bw = h, from = -5, to = 5, n = 1024)
    kdeTransf <- kde
    kdeTransf$x <- transfInv(kdeTransf$x)
    kdeTransf$y <- kdeTransf$y * transfDer(kdeTransf$x)
    
    # Transformed data
    plot(transf(xGrid), fTrue(xGrid) / transfDer(xGrid), type = "l", 
         xlab = "x", ylab = "Density", col = 2, lwd = 3, 
         xlim = range(transf(xGrid)[(fTrue(xGrid) / transfDer(xGrid)) > 0], 
                      na.rm = TRUE), 
         ylim = c(0, 1.5 * max(fTrue(xGrid) / transfDer(xGrid), na.rm = TRUE)), 
         main = "Usual kde for transformed data")
    matlines(kde$x, sapply(1:n, function(i) 
      density(x = transf(samp[i]), bw = h, from = -5, to = 5, n = 1024)$y) / n, 
      lty = 1, col = "gray", type = "l")
    lines(kde, lwd = 2)
    legend("topright", legend = c("True density", "Kde", "Kernels"),
           col = c(2, 1, "gray"), lwd = 2)
    rug(transf(samp), col = "gray")
    
    # Original data and transformed estimator
    plot(xGrid, fTrue(xGrid), type = "l", xlab = "x", ylab = "Density", col = 2, 
         lwd = 3, xlim = range(xGrid[fTrue(xGrid) > 1e-3], na.rm = TRUE), 
         ylim = c(0, 1.5 * max(fTrue(xGrid), na.rm = TRUE)), 
         main = "Transformed kde for original data")
    matlines(kdeTransf$x, sapply(1:n, function(i)
      density(x = transf(samp[i]), bw = h, from = -4, to = 4, n = 1024)$y) * 
        transfDer(kdeTransf$x) / n, lty = 1, col = "gray", type = "l")
    lines(kdeTransf$x, kdeTransf$y, lwd = 2)
    legend("topright", legend = c("True density", "Kde", "Kernels"),
           col = c(2, 1, "gray"), lwd = 2)
    rug(samp, col = "gray")
    
  }, width = 650, height = 325)
  
}

# Run the application
shinyApp(ui = ui, server = server)
