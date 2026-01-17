
#
# Shiny web application for illustrating the construction of the transformed
# kernel density estimator
#

library(shiny)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - an action button for generating a new sample
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
      uiOutput(outputId = "transf_selector")

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
  output$transf_selector <- renderUI({

    # Select data and caption
    choices <- switch(input$dist,
                    "1" = c("Log"),
                    "2" = c("Log"),
                    "3" = c("Log", "Probit"),
                    "4" = c("Log", "Probit"))
    selectInput(inputId = "transf", label = "Transformation:",
                choices = c("None", choices), selected = "None")

  })

  # Sampling function
  getSamp <- function() switch(input$dist,
                               "1" = rlnorm(n = 300),
                               "2" = rlnorm(n = 300, meanlog = 0:1,
                                            sdlog = c(1, 0.5)),
                               "3" = rbeta(n = 300, shape1 = 1, shape2 = 2),
                               "4" = rbeta(n = 300, shape1 = 1:2, shape2 = 2:1))
  getReactSamp <- eventReactive(input$newSample, getSamp())

  # Cache sample based on newSample button and dist
  samp_full <- reactive({

    if (values$default == 0) {

      set.seed(423432)
      getSamp()

    } else {

      getReactSamp()

    }
  })

  # Cache transformation selection
  iTransf <- reactive({

    ifelse(length(input$transf) == 0, "None", input$transf)

  })

  # Cache transformation functions based on iTransf
  transf <- reactive({

    switch(iTransf(),
           "None" = function(x) x,
           "Log" = function(x) log(x),
           "Probit" = function(x) qnorm(x))

  })
  transfInv <- reactive({

    switch(iTransf(),
           "None" = function(x) x,
           "Log" = function(x) exp(x),
           "Probit" = function(x) pnorm(x))

  })
  transfDer <- reactive({

    switch(iTransf(),
           "None" = function(x) 1,
           "Log" = function(x) 1 / x,
           "Probit" = function(x) 1 / dnorm(qnorm(x)))

  })

  # Cache xGrid based on transformation
  xGrid <- reactive({

    switch(iTransf(),
           "None" = seq(-4, 4, length.out = 1e3),
           "Log" = seq(exp(-4), exp(4), length.out = 1e3),
           "Probit" = seq(pnorm(-4), pnorm(4), length.out = 1e3))

  })

  # Cache sample subset and transformed sample
  samp_subset <- reactive({

    n_val <- as.integer(input$n)
    samp_full()[1:n_val]

  })

  samp_transformed <- reactive({

    transf_func <- transf()
    samp_subset_val <- samp_subset()
    transf_func(samp_subset_val)

  })

  # Cache density estimate based on transformed sample and bandwidth
  kde <- reactive({

    samp_transf <- samp_transformed()
    h_val <- as.numeric(input$h)
    density(samp_transf, bw = h_val, from = -5, to = 5, n = 1024)

  })

  # Cache transformed density
  kdeTransf <- reactive({

    kde_val <- kde()
    transfInv_func <- transfInv()
    transfDer_func <- transfDer()
    kdeTransf_result <- kde_val
    kdeTransf_result$x <- transfInv_func(kdeTransf_result$x)
    kdeTransf_result$y <- kdeTransf_result$y * transfDer_func(kdeTransf_result$x)
    kdeTransf_result

  })

  # Cache kernel density matrix for visualization (left plot)
  kernel_matrix_left <- reactive({

    n_val <- as.integer(input$n)
    h_val <- as.numeric(input$h)
    samp_transf <- samp_transformed()
    kde_val <- kde()
    sapply(1:n_val, function(i)
      density(x = samp_transf[i], bw = h_val, from = -5, to = 5, n = 1024)$y) / n_val

  })

  # Cache kernel density matrix for visualization (right plot)
  kernel_matrix_right <- reactive({

    n_val <- as.integer(input$n)
    h_val <- as.numeric(input$h)
    samp_transf <- samp_transformed()
    kdeTransf_val <- kdeTransf()
    transfDer_func <- transfDer()
    sapply(1:n_val, function(i)
      density(x = samp_transf[i], bw = h_val, from = -4, to = 4, n = 1024)$y) *
      transfDer_func(kdeTransf_val$x) / n_val

  })

  output$kdeTransfPlot <- renderPlot({

    # Get cached values
    samp_subset_val <- samp_subset()
    samp_transf_val <- samp_transformed()
    transf_func <- transf()
    transfInv_func <- transfInv()
    transfDer_func <- transfDer()
    xGrid_val <- xGrid()
    kde_val <- kde()
    kdeTransf_val <- kdeTransf()
    kernel_matrix_left_val <- kernel_matrix_left()
    kernel_matrix_right_val <- kernel_matrix_right()

    # True density
    fTrue <- switch(input$dist,
                    "1" = function(x) dlnorm(x),
                    "2" = function(x) 0.5 * dlnorm(x) +
                      0.5 * dlnorm(x, meanlog = 1, sdlog = 0.5),
                    "3" = function(x) dbeta(x, shape1 = 1, shape2 = 2),
                    "4" = function(x) 0.5 * dbeta(x, shape1 = 1, shape2 = 2) +
                      0.5 * dbeta(x, shape1 = 2, shape2 = 1))

    # Plot
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))

    # Transformed data
    plot(transf_func(xGrid_val), fTrue(xGrid_val) / transfDer_func(xGrid_val), type = "l",
         xlab = "x", ylab = "Density", col = 2, lwd = 3,
         xlim = range(transf_func(xGrid_val)[(fTrue(xGrid_val) / transfDer_func(xGrid_val)) > 0],
                      na.rm = TRUE),
         ylim = c(0, 1.5 * max(fTrue(xGrid_val) / transfDer_func(xGrid_val),
                               na.rm = TRUE)),
         main = "Usual kde for transformed data")
    matlines(kde_val$x, kernel_matrix_left_val,
             lty = 1, col = "gray", type = "l")
    lines(kde_val, lwd = 2)
    legend("topright", legend = c("True density", "Kde", "Kernels"),
           col = c(2, 1, "gray"), lwd = 2)
    rug(samp_transf_val, col = "gray")

    # Original data and transformed estimator
    plot(xGrid_val, fTrue(xGrid_val), type = "l", xlab = "x", ylab = "Density",
         col = 2, lwd = 3,
         xlim = range(xGrid_val[fTrue(xGrid_val) > 1e-3], na.rm = TRUE),
         ylim = c(0, 1.5 * max(fTrue(xGrid_val), na.rm = TRUE)),
         main = "Transformed kde for original data")
    matlines(kdeTransf_val$x, kernel_matrix_right_val,
             lty = 1, col = "gray", type = "l")
    lines(kdeTransf_val$x, kdeTransf_val$y, lwd = 2)
    legend("topright", legend = c("True density", "Kde", "Kernels"),
           col = c(2, 1, "gray"), lwd = 2)
    rug(samp_subset_val, col = "gray")

  }, width = 650, height = 325)

}

# Run the application
shinyApp(ui = ui, server = server)
