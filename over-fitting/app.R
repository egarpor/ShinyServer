#
# Shiny web application for illustrating over-fitting in a regression model
#

library(shiny)

# Load data
xGrid <- seq(-4, 4, by = 0.01)
m <- function(x, type) {
  
  switch(type, 
         "1" = 5,
         "2" = 4.5 + 0.75 * x,
         "3" = 3 - 0.75 * x + 0.25 * x^2,
         "4" = 5 + x * sin(x))
  
}
mGrid <- cbind(m(xGrid, 1), m(xGrid, 2), m(xGrid, 3), m(xGrid, 4), m(xGrid, 5))
lGrid <- length(xGrid)
n <- 10

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - an action buttom for generating a new sample
  # - a radio input for the regression function
  # - a slider input for the degree of the polynomial fit
  verticalLayout(

    inputPanel(
      
      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      radioButtons(inputId = "reg", label = "True model:",
                  choices = c("Constant" = 1, "Linear" = 2, 
                              "Quadratic" = 3, "Non-linear" = 4), 
                  selected = 3, inline = TRUE),
      sliderInput(inputId = "p", label = "Degree of polynomial fit:",
                  min = 1, max = n - 1, value = 3, step = 1)

    ),

    plotOutput("overfittingPlot")

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
    
    X <- rnorm(n = 2 * n)
    e <- rnorm(n = 2 * n, sd = 1)
    Y <- m(X, type = input$reg) + e
    return(list("X" = X, "Y" = Y))
    
  }
  getReactSamp <- eventReactive(input$newSample, getSamp())
  
  output$overfittingPlot <- renderPlot({
    
    # Check if the buttom was clicked
    if (values$default == 0) {
      
      set.seed(4244321)
      samp <- getSamp()
      
    } else {
      
      samp <- getReactSamp()
      
    }
    X <- samp$X[1:n]
    Y <- samp$Y[1:n]
    XNew <- samp$X[(n + 1):(2 * n)]
    YNew <- samp$Y[(n + 1):(2 * n)]
    
    # True regression
    mTrue <- mGrid[, as.integer(input$reg)]
    
    # Model regressions
    p <- as.integer(input$p)
    XDesign <- cbind(1, poly(x = X, degree = p, raw = TRUE))
    XDesignInv <- svd(crossprod(XDesign))
    d <- rep(0, p + 1)
    d[XDesignInv$d > 1e-10] <- 1 / XDesignInv$d[XDesignInv$d > 1e-10]
    XDesignInv <- XDesignInv$v %*% diag(d) %*% t(XDesignInv$u)
    betaHat <- XDesignInv %*% t(XDesign) %*% Y
    modGrid <- poly(x = xGrid, degree = p, raw = TRUE) %*% betaHat[-1] + 
      betaHat[1]
    YHat <- poly(x = X, degree = p, raw = TRUE) %*% betaHat[-1] + 
      betaHat[1]
    YHatNew <- poly(x = XNew, degree = p, raw = TRUE) %*% betaHat[-1] + 
      betaHat[1]
    
    # Plots
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1, oma = rep(0, 4))
    
    plot(X, Y, pch = 16, xlab = "x", ylab = "y", xlim = c(-4, 4), 
         ylim = c(0, 10))
    title(main = paste("Training error =", 
                       sprintf("%-6.4g", sum((YHat - Y)^2))), 
          family = "mono", cex.main = 1.25)
    lines(xGrid, mTrue, col = 1, lwd = 2)
    lines(xGrid, modGrid, col = 2, lwd = 2)
    points(X, YHat, col = 2)
    
    plot(XNew, YNew, pch = 16, xlab = "x", ylab = "y", xlim = c(-4, 4), 
         ylim = c(0, 10))
    title(main = paste("Predictive error =", 
                       sprintf("%-6.4g", sum((YHatNew - YNew)^2))),
          family = "mono", cex.main = 1.25)
    lines(xGrid, mTrue, col = 1, lwd = 2)
    lines(xGrid, modGrid, col = 2, lwd = 2)
    points(XNew, YHatNew, col = 2)
    
  }, width = 650, height = 325)
  
}

# Run the application
shinyApp(ui = ui, server = server)

