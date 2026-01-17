
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

  # Vertical layout with:
  # - an action button for generating a new sample
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
                  selected = "Gaussian")

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

  # Sampling function
  getSamp <- function() switch(input$dist,
                               "1" = rnorm(n = 200),
                               "2" = rnorMix(n = 200, obj = MW.nm7),
                               "3" = rnorMix(n = 200, obj = MW.nm10))

  # Cache sample based on newSample button and dist
  # Sample regenerates when either the button is clicked or dist changes
  samp_full <- reactive({

    # Track both button clicks and dist changes
    input$newSample
    input$dist

    if (values$default == 0) {

      set.seed(423432)
      getSamp()

    } else {

      getSamp()

    }

  })

  # Cache sample subset based on n
  samp_subset <- reactive({

    n_val <- as.integer(input$n)
    samp_full()[1:n_val]

  })

  # Cache density estimate based on samp, h, and kernel
  kde <- reactive({

    samp_val <- samp_subset()
    h_val <- as.numeric(input$h)
    kernel_val <- tolower(input$kernel)
    density(x = samp_val, bw = h_val, from = -4, to = 4, n = 1024,
            kernel = kernel_val)

  })

  # Cache kernel density matrix for visualization
  kernel_matrix <- reactive({

    samp_val <- samp_subset()
    h_val <- as.numeric(input$h)
    kernel_val <- tolower(input$kernel)
    n_val <- as.integer(input$n)
    sapply(1:n_val, function(i) {
      density(x = samp_val[i], bw = h_val, from = -4, to = 4, n = 1024,
              kernel = kernel_val)$y
    }) / n_val

  })

  output$kdePlot <- renderPlot({

    # Get cached values
    samp_val <- samp_subset()
    kde_val <- kde()
    kernel_matrix_val <- kernel_matrix()
    kernel_val <- tolower(input$kernel)

    # True density
    fTrue <- dens[, as.integer(input$dist)]

    # Plot
    par(mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    plot(xGrid, fTrue, type = ifelse(kernel_val == "rectangular", "s", "l"),
         xlab = "x", ylab = "Density", col = 2, lwd = 3, ylim = c(0, 0.65))
    matlines(kde_val$x, kernel_matrix_val, lty = 1, col = "gray",
             type = ifelse(kernel_val == "rectangular", "s", "l"))
    lines(kde_val, lwd = 2)
    legend("topright", legend = c("True density",
                                  "Kernel density estimator",
                                  "Kernels centered at data"),
           col = c(2, 1, "gray"), lwd = 2)
    rug(samp_val, col = "gray")

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

# # Data
# library(nor1mix)
# xGrid <- seq(-4, 4, by = 0.005)
#
# dens <- cbind(dnorm(xGrid),
#               dnorMix(x = xGrid, obj = MW.nm7),
#               dnorMix(x = xGrid, obj = MW.nm10))
#
# # Save data
# save(list = c("xGrid", "dens"), file = "xData.RData")
