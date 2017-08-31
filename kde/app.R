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
  # - an action buttom for generating a new sample
  # - a slider input for the sample size
  # - a radio input for the distribution
  # - a slider input for the bandwidth
  # - a select input for the kernel
  
  verticalLayout(
    
    inputPanel(
      
      actionButton(inputId = "newSample",
                   label = HTML("<h5>Get a new<br> sample!</h5>")),
      sliderInput(inputId = "n", label = "Sample size:",
                  min = 1, max = 200, value = 10, step = 1),
      radioButtons(inputId = "dist", label = "Density:",
                   choices = c("Normal" = 1, "Mixture" = 2, "Claw" = 3), 
                   selected = 1, inline = TRUE),
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
  
  # Sampling
  getSamp <- function() switch(input$dist,
                               "1" = rnorm(n = 200),
                               "2" = rnorMix(n = 200, obj = MW.nm7),
                               "3" = rnorMix(n = 200, obj = MW.nm10))
  getReactSamp <- eventReactive(input$newSample, getSamp())
  
  output$kdePlot <- renderPlot({
    
    # Check if the buttom was clicked
    if (values$default == 0) {
      
      set.seed(423432)
      samp <- getSamp()
      
    } else {
      
      samp <- getReactSamp()
      
    }
    
    # True density
    fTrue <- dens[, as.integer(input$dist)]
    
    # Prepare for plot
    n <- as.integer(input$n)
    h <- as.numeric(input$h)
    kernel <- tolower(input$kernel)
    samp <- samp[1:n]
    
    # Plot
    par(mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
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
    rug(samp, col = "gray")
    
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
# # Save data
# save(list = c("xGrid", "dens"), file = "xData.RData")
