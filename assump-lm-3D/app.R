
#
# Shiny web application for illustrating the assumptions in multiple
# linear regression
#

library(shiny)
library(rgl)
library(plot3Drgl)

# Load data
load("assumptions3D-TF.RData")

# Choices
choices <- 1:4
namesTrue <- c("X1 and X2 normal",
               "X1 and X2 related",
               "X1 discrete and X2 continuous",
               "High dispersion")
namesFalse <- c("No linearity",
                "No homoscedasticity",
                "No normality",
                "No independence")

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
projPlane <- function(x, coefs, intercept) {

  # Compute a point on the plane (i.e., point %*% coefs = intercept)
  ind <- which.max(abs(coefs))
  p <- length(coefs)
  point <- rep(0, p)
  point[ind] <- intercept / coefs[ind]

  # Projection
  if (!is.null(dim(x))) {

    n <- dim(x)[1]
    # For proper sum of vector and matrix by rows
    point <- matrix(point, nrow = n, ncol = p, byrow = TRUE) 
    tt <- (point - x) %*% coefs / sum(coefs ^ 2)
    projx <- x + drop(tt) * matrix(coefs, nrow = n, ncol = p, byrow = TRUE)

  } else{

    tt <- coefs %*% (point - x) / sum(coefs ^ 2)
    projx <-  x + tt * coefs

  }

  return(projx)

}

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - a radio input for choosing whether the assumptions hold or not
  # - a dynamic selector for specifying cases where the assumptions hold / 
  #   do not hold
  verticalLayout(

    inputPanel(

      radioButtons(inputId = "assump", label = "Assumptions satisfied?",
                   choices = c("Yes", "No")),
      uiOutput(outputId = "cases")

    ),

    rglwidgetOutput("assumpPlot")

  )

)

# Server logic
server <- function(input, output) {

  # Update cases
  output$cases <- renderUI({

    # Select data and caption
    names(choices) <- switch(input$assump, "Yes" = namesTrue, "No" = namesFalse)
    selectInput(inputId = "case", label = "Case:", choices = choices)

  })

  output$assumpPlot <- renderRglwidget({

    # Case
    i <- as.integer(input$case)
    i <- ifelse(length(i) == 0, 1, i)
    main <- ifelse(input$assump == "Yes", namesTrue[i], namesFalse[i])

    # Select data and caption
    x1 <- x1T[i, ]
    x2 <- x2T[i, ]
    y <- yT[i, ]
    if (input$assump == "Yes") {

      x1 <- x1T[i, ]
      x2 <- x2T[i, ]
      y <- yT[i, ]

    } else {

      x1 <- x1F[i, ]
      x2 <- x2F[i, ]
      y <- yF[i, ]

    }

    # Fit plane and predict data
    mod <- lm(y ~ x1 + x2)
    l <- 10
    xx1 <- seq(min(x1), max(x1), l = l)
    xx2 <- seq(min(x2), max(x2), l = l)
    xx12 <- expand.grid(x1 = xx1, x2 = xx2)
    pred <- predict(mod, newdata = xx12)

    # Limits plot
    xlim <- range(x1)
    ylim <- range(x2)
    zlim <- range(c(y, pred))

    # Open plot
    persp3Drgl(xx1, xx2, median(pred), xlim = xlim, ylim = ylim, zlim = zlim,
               xlab = "x1", ylab = "x2", zlab = "y", colkey = FALSE,
               type = "n", ticktype = "detailed", main = main)

    # Plot plane
    a <- -mod$coefficients[2]
    b <- -mod$coefficients[3]
    c <- 1
    d <- -mod$coefficients[1]
    planes3d(a = a, b = b, c = c, d = d, alpha = 0.75, lit = FALSE,
             col = "lightblue")

    # Add data
    spheres3d(cbind(x1, x2, y), radius = 0.02)

    rglwidget()

  })

}

# Run the application
shinyApp(ui = ui, server = server)

# # Generate data
# n <- 200
# x1F <- x2F <- yF <- x1T <- x2T <- yT <- matrix(nrow = 4, ncol = n)
# set.seed(234678)
#
# # Assumptions TRUE
#
# # 1
# eps <- rnorm(n, sd = 0.75)
# x1T[1, ] <- rnorm(n)
# x2T[1, ] <- rnorm(n)
# yT[1, ] <- 1 * x1T[1, ] - 0.5 * x2T[1, ] + eps
#
# # 2
# eps <- rnorm(n, sd = 0.75)
# x1T[2, ] <- rnorm(n)
# x2T[2, ] <- x1T[2, ]^2 + rnorm(n)
# yT[2, ] <- 1 * x1T[2, ] - 0.5 * x2T[2, ] + eps
#
# # 3
# eps <- rnorm(n)
# x1T[3, ] <- c(rnorm(n/2, mean = -2, sd = 0.75), rnorm(n/2, mean = 2, sd = 0.75))
# x2T[3, ] <- rpois(n, lambda = 4)
# yT[3, ] <- 1 * x1T[3, ] - 0.5 * x2T[3, ] + eps
#
# # 4
# eps <- rnorm(n, sd = 2)
# x1T[4, ] <- rnorm(n)
# x2T[4, ] <- rnorm(n)
# yT[4, ] <- 1 * x1T[4, ] - 0.5 * x2T[4, ] + eps
#
# # Assumptions FALSE
#
# # 1
# eps <- rnorm(n)
# x1F[1, ] <- rnorm(n)
# x2F[1, ] <- rnorm(n)
# yF[1, ] <- 1 * x1F[1, ]^2 - 0.5 * x2F[1, ]^3 + eps
#
# # 2
# eps <- rnorm(n, sd = 0.75)
# x1F[2, ] <- rnorm(n)
# x2F[2, ] <- rnorm(n)
# eps <- rnorm(n, sd = 0.1 + 0.5 * x2F[2, ]^2)
# yF[2, ] <- 1 * x1F[2, ] - 0.5 * x2F[2, ] + eps
#
# # 3
# x1F[3, ] <- rnorm(n)
# x2F[3, ] <- rnorm(n)
# eps <- rexp(n) - 1
# yF[3, ] <- 1 * x1F[3, ] - 0.5 * x2F[3, ] + eps
#
# # 4
# eps <- c(arima.sim(model = list(ar = 0.95, ma = 0), n = n, sd = 0.5))
# x1F[4, ] <- rnorm(n)
# x2F[4, ] <- rnorm(n)
# yF[4, ] <- 1 * x1F[4, ] - 0.5 * x2F[4, ] + eps
#
# # Save data
# save(list = c("x1T", "x2T", "yT", "x1F", "x2F", "yF"), 
#      file = "assumptions3D-TF.RData")
#
# # For the course
# assumptions3D <- data.frame(x1 = t(rbind(x1T, x1F)), x2 = t(rbind(x2T, x2F)),
#                             y = t(rbind(yF, yT)))
# # Save data
# save(list = "assumptions3D", file = "assumptions3D.RData")
