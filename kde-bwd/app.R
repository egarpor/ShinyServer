#
# Shiny web application for illustrating the construction of the kernel density
# estimator
#

library(shiny)
library(nor1mix)
library(ks)

# Load data
load("xData.RData")
bwdSels <- c("RT", "SJ", "UCV", "BCV", "SCV")
choices1x1 <- 1:16
choices2x2 <- 1:4
names(choices1x1) <- paste(paste("MW", 1:16, sep = ""), 
                           c("Gaussian", "Skewed", "Strong skewed", "Kurtotic", 
                             "Outlier", "Bimodal", "Separated", 
                             "Asym. bimodal", "Trimodal", "Claw",
                             "Double claw", "Asym. claw", "Asym. double claw", 
                             "Smooth comb", "Discrete comb", "Distant bimodal"), 
                           sep = " - ")
names(choices2x2) <- paste("MW", seq(1, 13, by = 4), 
                           "-", seq(4, 16, by = 4), sep = "")

#' @title Biased and unbased cross-validation with grid search
#'
#' @description Cross-validation bandwidth selectors for Gaussian kernels in \code{\link[stats]{density}} with reliable minima.
#'
#' @inheritParams bw.ucv
#' @param h.grid vector for serching the bandwith minimizing the CV loss function.
#' @param plot.cv flag to indicate the plot of the CV loss function.
#' @return
#' A bandwidth on a scale suitable for the \code{bw} argument of \code{density}.
#' @examples
#' library(nor1mix)
# set.seed(987654)
# samp <- rnorMix(n = 100, obj = MW.nm11)
# bw.ucv.mod(x = samp, plot.cv = TRUE)
# abline(v = bw.ucv(samp), col = 3)
# bw.bcv.mod(x = samp, plot.cv = TRUE)
# abline(v = bw.bcv(samp), col = 3)
#' @author Code modified from \code{\link[stats]{bw.cv}} and \code{\link[stats]{bw.ucv}} by Eduardo García-Portugués (\email{edgarcia@est-econ.uc3m.es}).
#' @export
bw.ucv.mod <- function(x, nb = 1000L, 
                       h.grid = (seq(sqrt(0.01), sqrt(2), l = 200))^2, 
                       plot.cv = FALSE) {
  if ((n <- length(x)) < 2L) 
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n)) 
    stop("invalid length(x)")
  if (!is.numeric(x)) 
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L) 
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fucv <- function(h) .Call(stats:::C_bw_ucv, n, d, cnt, h)
  # h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol) 
  #   warning("minimum occurred at one end of the range")
  obj <- sapply(h.grid, function(h) fucv(h))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

#' @rdname bw.ucv.mod
#' @export
bw.bcv.mod <- function(x, nb = 1000L, 
                       h.grid = (seq(sqrt(0.01), sqrt(2), l = 200))^2,
                       plot.cv = FALSE) {
  if ((n <- length(x)) < 2L) 
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n)) 
    stop("invalid length(x)")
  if (!is.numeric(x)) 
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L) 
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fbcv <- function(h) .Call(stats:::C_bw_bcv, n, d, cnt, h)
  # h <- optimize(fbcv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol) 
  #   warning("minimum occurred at one end of the range")
  obj <- sapply(h.grid, function(h) fbcv(h))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# UI for application
ui <- fluidPage(align = "center",
                
    # Horizontal layout with:
    # - an action buttom for generating a new sample
    # - a select input for the sample size
    # - a radio input for the plots layout
    # - a select input for the distribution
    # - a dynamic selector 
    # - a checkbox input for the bandwidth
    
    verticalLayout(
      
      inputPanel(
        
        actionButton(inputId = "newSample",
                     label = HTML("<h5>Get a new<br> sample!</h5>")),
        selectInput(inputId = "n", label = "Sample size:",
                    choices = c(50, 100, 250, 500, 1000), selected = 100),
        radioButtons(inputId = "layout", label = "Layout plots:", 
                     choiceNames = c("1 x 1", "2 x 2"), 
                     choiceValues = 1:2, selected = 1),
        uiOutput(outputId = "dist"),
        checkboxGroupInput(inputId = "bwds", label = "Bandwidth selectors:", 
                           choiceNames = bwdSels, choiceValues = 1:5, 
                           selected = 1:3, inline = TRUE)
        
      ),
      
      plotOutput("kdeBwdPlot")
      
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
  getSamp <- function() sapply(1:16, function(i) 
      rnorMix(n = 1e3, obj = get(paste("MW.nm", i, sep = ""))))
  getReactSamp <- eventReactive(input$newSample, getSamp())
  
  # Update dist
  output$dist <- renderUI({
    
    # Select data and caption
    choices <- switch(input$layout, 
                      "1" = choices1x1, 
                      "2" = choices2x2)
    selectInput(inputId = "dist", label = "Densities:",
                choices = choices, selected = 1)
    
  })
  
  output$kdeBwdPlot <- renderPlot({
    
    # Check if the buttom was clicked
    if (values$default == 0) {
      
      set.seed(423432)
      samp <- getSamp()
      
    } else {
      
      samp <- getReactSamp()
      
    }
    
    # Prepare for plots
    arrange <- switch(input$layout, "1" = c(1, 1), "2" = c(2, 2))
    nArrange <- prod(arrange)
    d <- as.integer(input$dist)
    d <- ifelse(length(d) == 0, 1, d)
    ise <- rep(NA, 5)
    
    # Plots
    par(mfrow = arrange, mar = c(4, 4, 3, 1) + 0.2, oma = rep(0, 4))
    for (i in 1:nArrange) {
      
      # Density
      id <- i + (d - 1) * nArrange
      plot(xGrid, dens[, id], type = "l", xlab = "x", ylab = "Density", col = 1, 
           lwd = 3, ylim = c(0, 0.8), main = "")
      
      # Particular sample
      sampleI <- samp[1:input$n, id]
      rug(sampleI)
      
      # Kdes
      indBdw <- as.integer(input$bwds)
      for (k in indBdw) {
        
        # Select bandwidth and plot kde
        h <- switch(k, 
                    "1" = bw.nrd(x = sampleI),
                    "2" = hpi(x = sampleI, binned = TRUE, bgridsize = 512),
                    "3" = bw.ucv.mod(x = sampleI, nb = 512),
                    "4" = bw.bcv.mod(x = sampleI, nb = 512),
                    "5" = hscv(x = sampleI, binned = TRUE, bgridsize = 512))
        dkde <- kde(x = sampleI, eval.points = xGrid, h = h)$estimate
        lines(xGrid, dkde, col = as.integer(k) + 1, lwd = 2)
        legend("topright", legend = c("Density", bwdSels[as.integer(indBdw)]),
               col = c(1, as.integer(indBdw) + 1), lwd = 1)
        
        # ISE
        ise[k] <- mean((dens[, id] - dkde)^2) * 1e3
        
      }

      # Info
      ord <- order(ise, na.last = NA)
      iseText <- paste(bwdSels[ord], 
                       paste("(", sprintf("%.2f", ise[ord]), ")", sep = ""),
                       collapse = ", ")
      title(main = paste("MW", id, ": ", iseText, sep = ""), 
            cex.main = ifelse(nArrange == 1, 1.5, 0.9))
      
    }
    
  }, width = 650, height = 650)
  
}

# Run the application
shinyApp(ui = ui, server = server)

# # Data
# library(nor1mix)
# xGrid <- seq(-4, 4, by = 0.01)
# 
# # Densities
# dens <- sapply(1:16, function(k) 
#   dnorMix(x = xGrid, obj = get(paste("MW.nm", k, sep = ""))))
# 
# # Save data
# save(list = c("xGrid", "dens"), file = "xData.RData")
