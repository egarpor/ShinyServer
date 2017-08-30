#
# Shiny web application for illustrating the construction of the kernel density
# estimator
#

library(shiny)
library(nor1mix)

# Load data
load("xData.RData")

# UI for application
ui <- fluidPage(align = "center",
                
  # Horizontal layout with:
  # - action buttom for generating a new sample
  # - the select input for the sample size n
  # - the select input for the distribution
  # - the slider input for the bandwidth
  
  verticalLayout(
    
    inputPanel(
      
      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      sliderInput(inputId = "n", label = "Sample size:",
                  min = 1, max = 200, value = 10, step = 1),
      selectInput(inputId = "dist", label = "Density:",
                  choices = c("Normal", "Mixture", "Bart Simpson"), 
                  selected = "Normal"),
      sliderInput(inputId = "h", label = "Bandwidth h:",
                  min = 0.01, max = 2, value = 0.5, step = 0.05),
      selectInput(inputId = "kernel", label = "Kernel:",
                  choices = c("Gaussian", "Epanechnikov", "Rectangular", 
                              "Triangular", "Biweight", "Cosine", 
                              "Optcosine"), 
                  selected = "Normal")
      
    ),
    
    plotOutput("kdePlot")
    
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
  getSamp <- eventReactive(input$newSample, {
    
    switch(input$dist,
           "Normal" = rnorm(n = 200),
           "Mixture" = rnorMix(n = 200, obj = MW.nm7),
           "Bart Simpson" = rnorMix(n = 200, obj = MW.nm10))
    
  })
  
  output$kdePlot <- renderPlot({
    
    # Check if the buttom was clicked
    if (values$default == 0) {
      
      set.seed(423432)
      samp <- switch(input$dist,
                     "Normal" = rnorm(n = 200),
                     "Mixture" = rnorMix(n = 200, obj = MW.nm7),
                     "Bart Simpson" = rnorMix(n = 200, obj = MW.nm10))
      
    } else {
      
      samp <- getSamp()
      
    }
    
    # True density and distribution
    n <- as.integer(input$n)
    samp <- samp[1:n]
    h <- as.numeric(input$h)
    fTrue <- switch(input$dist,
                    "Normal" = dens[, 1],
                    "Mixture" = dens[, 2],
                    "Bart Simpson" = dens[, 3])
    kernel <- tolower(input$kernel)
    
    # Plot
    plot(xGrid, fTrue, type = "l", xlab = "x", ylab = "Density", col = 2, 
         lwd = 3, ylim = c(0, 0.65))
    kde <- density(x = samp, bw = h, from = -4, to = 4, kernel = kernel)
    matlines(kde$x, sapply(1:n, function(i) density(x = samp[i], bw = h, 
                                                  from = -4, to = 4, 
                                                  kernel = kernel)$y) / n,
             lty = 1, col = "gray")
    lines(kde, lwd = 2)
    legend("topright", legend = c("True density", 
                                  "Kernel density estimator",
                                  "Kernels centered at data"),
           col = c(2, 1, "gray"), lwd = 2)
    rug(samp, lwd = 2)
    
  }, width = 650, height = 650)
  
}

# Run the application
shinyApp(ui = ui, server = server)

# # Data
# library(nor1mix)
# xGrid <- seq(-4, 4, by = 0.01)
# 
# dens <- cbind(dnorm(xGrid),
#               dnorMix(x = xGrid, obj = MW.nm7),
#               dnorMix(x = xGrid, obj = MW.nm10))
# 
# 
# save(list = c("xGrid", "dens"), file = "xData.RData")
