#
# Shiny web application for illustrating the choice of distances to minimize in
# linear regression: vertical, horizontal or perpendicular
#

library(shiny)
library(rgl)
library(plot3Drgl)

#' @title Project points into a plane
#'
#' @description Projects a matrix of points into the plane defined by $n\cdot x=c$.
#'
#' @param x matrix of points (observations by rows) of dimension \code{c(n, p)}.
#' @param coefsPlane normal vector $n$ definying the plane. It has length \code{p}.
#' @param interceptPlane intercept $c$ of the plane.
#' @return
#' A matrix of the same size of \code{x} containing the projected points.
#' @examples
#' x <- rbind(c(1, 0, 0), c(1, 1, 1))
#' projPlane(x = x, coefs = c(1, 0, 0), intercept = 0)
#' @author Eduardo García-Portugués (\email{edgarcia@est-econ.uc3m.es}).
#' @export
projPlane = function(x, coefs, intercept) {
  
  # Compute a point on the plane (i.e., point %*% coefs = intercept)
  ind <- which.max(abs(coefs))
  p <- length(coefs)
  point <- rep(0, p)
  point[ind] <- intercept / coefs[ind]
  
  # Projection
  if (!is.null(dim(x))) {
    
    n <- dim(x)[1]
    point <- matrix(point, nrow = n, ncol = p, byrow = TRUE) # For proper sum of vector and matrix by rows
    tt <- (point - x) %*% coefs / sum(coefs ^ 2)
    projx <- x + drop(tt) * matrix(coefs, nrow = n, ncol = p, byrow = TRUE)
    
  } else{
    
    tt <- coefs %*% (point - x) / sum(coefs ^ 2)
    projx <-  x + tt * coefs
    
  }
  
  return(projx)
  
}

# Data
set.seed(34567)
x1 <- rnorm(50)
x2 <- rnorm(50)
x3 <- x1 + rnorm(50, sd = 0.05)
eps <- rnorm(50)
yLin <- -0.5 + 0.5 * x1 + 0.5 * x2 + eps
yQua <- -0.5 + x1^2 + 0.5 * x2 + eps
yExp <- -0.5 + 0.5 * exp(x2) + x3 + eps

# Choices for type of distance
choices <- 1:4
names(choices) <- c("vertical (Y)", "horizontal (X1)", "horizontal (X2)", 
                    "perpendicular")

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - the select inputs for the data pattern and type of distance
  # - the check boxes for displaying segments and predictions
  verticalLayout(

    inputPanel(

      selectInput(inputId = "dataType", label = "Data pattern:",
                  choices = c("linear", "quadratic", "exponential")),
      selectInput(inputId = "distType", label = "Type of distance:",
                  choices = choices),
      checkboxInput("predicted", "Predictions", value = TRUE),
      checkboxInput("segments", "Distances", value = TRUE)
      
    ),

    rglwidgetOutput("regressionPlot")

  )

)

# Server logic
server <- function(input, output) {
  
  output$regressionPlot <- renderRglwidget({

    # Response
    y <- switch(input$dataType,
                linear = yLin,
                quadratic = yQua,
                exponential = yExp)

    ## Fit plane and compute projections
    
    # Limits plot
    lim <- range(c(x1, x2, y))
    
    # Vertical projection (Y)
    modY <- lm(y ~ x1 + x2)
    projY <- predict(modY, newdata = data.frame(x1 = x1, x2 = x2))
    projY <- cbind(x1, x2, projY)

    # Horizontal projection (X1)
    modX1 <- lm(x1 ~ x2 + y)
    projX1 <- predict(modX1, newdata = data.frame(x2 = x2, y = y))
    projX1 <- cbind(projX1, x2, y)
  
    # Horizontal projection (X2)
    modX2 <- lm(x2 ~ x1 + y)
    projX2 <- predict(modX2, newdata = data.frame(x1 = x1, y = y))
    projX2 <- cbind(x1, projX2, y)
    
    # Perpendicular projection (PCA)
    pca <- princomp(cbind(x1, x2, y))
    d <- pca$center %*% pca$loadings[, 3]
    
    # Perpendicular projection into a * x + b * y + c * z = d
    projPCA <- projPlane(x = cbind(x1, x2, y), coefs = pca$loadings[, 3], 
                         intercept = d)

    # Plot plane
    persp3Drgl(x1, x2, median(y), xlim = lim, ylim = lim, zlim = lim,
               xlab = "x1", ylab = "x2", zlab = "y", colkey = FALSE,
               type = "n", ticktype = "detailed")
    
    # Coefficients plane
    if (input$distType == 1) {
      
      proj <- projY
      a <- -modY$coefficients[2]
      b <- -modY$coefficients[3]
      c <- 1
      d <- -modY$coefficients[1]
      
    } else if (input$distType == 2) {
      
      proj <- projX1
      a <- -1 / modX1$coefficients[3]
      b <- modX1$coefficients[2] / modX1$coefficients[3]
      c <- 1
      d <- modX1$coefficients[1] / modX1$coefficients[3]
      
    } else if (input$distType == 3) {
      
      proj <- projX2
      a <- modX2$coefficients[2] / modX2$coefficients[3]
      b <- -1 / modX2$coefficients[3]
      c <- 1
      d <- modX2$coefficients[1] / modX2$coefficients[3]
      
    } else if (input$distType == 4) {
      
      proj <- projPCA
      a <- pca$loadings[1, 3] / pca$loadings[3, 3]
      b <- pca$loadings[2, 3] / pca$loadings[3, 3]
      c <- 1
      d <- -d
      
    }
    
    # Plot plane
    planes3d(a = a, b = b, c = c, d = d, alpha = 0.75, lit = FALSE,
             col = "lightblue")
    
    # Add data
    spheres3d(cbind(x1, x2, y), col = 2, radius = 0.02)
    
    # Segments
    if (input$segments) {
      
      segments3d(x = c(rbind(x1, proj[, 1])), y = c(rbind(x2, proj[, 2])), 
                 z = c(rbind(y, proj[, 3])), col = 3)
    
    }
    
    # Fitted points
    if (input$predicted) {
      
      spheres3d(proj, col = 1, radius = 0.02)
      
    }
    
    rglwidget()

  })

}

# Run the application
shinyApp(ui = ui, server = server)

