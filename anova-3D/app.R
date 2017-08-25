#
# Shiny web application for illustrating the ANOVA decomposition in 3D and its
# dependence on the error standard deviation
#

library(shiny)
library(plot3D)

# Add an alpha value to a colour
addAlpha <- function(col, alpha = 1) {

  if (missing(col)) stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb) / 255, 2,
        function(x) rgb(x[1], x[2], x[3], alpha = alpha))

}

# Data
n1 <- 3
XX <- expand.grid(seq(-3, 3, l = n1), seq(-3, 3, l = n1))
n <- n1^2
X1 <- XX[, 1]
X2 <- XX[, 2]
# n <- 16
# X1 <- runif(n, -3, 3)
# X2 <- runif(n, -3, 3)
data <- data.frame(X = X1, Y = X2)

# Parameters
nGrid <- 5

# Create and plot initial XYZ-grid
gX <- gY <- seq(-3.5, 3.5, length = nGrid)

# Compute regression surface
x <- pretty(gX, nGrid)
y <- pretty(gY, nGrid)
x <- c(gX[1], x[x > gX[1] & x < gX[nGrid]], gX[nGrid])
y <- c(gY[1], y[y > gY[1] & y < gY[nGrid]], gY[nGrid])
lx <- length(x)
ly <- length(y)
xy <- expand.grid(x = x, y = y)
new <- data.frame(X = xy$x, Y = xy$y)
M <- mesh(x, y)

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - action buttom for generating a new sample
  # - the slider inputs for the error standard deviation
  # - the slider inputs for the rotations and zoom
  verticalLayout(

    inputPanel(

      actionButton(inputId = "newSample", label = HTML("<h5>Get a new<br> sample!</h5>")),
      sliderInput(inputId = "sigma", label = "Error standard deviation:",
                  min = 0, max = 3, value = 1, step = 0.1),
      sliderInput(inputId = "theta", label = "Horizontal rotation:",
                  min = -180, max = 180, value = -25, step = 10),
      sliderInput(inputId = "phi", label = "Vertical rotation:",
                  min = -90, max = 90, value = 10, step = 5)

    ),

    plotOutput("anovaPlot3D")

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

  output$anovaPlot3D <- renderPlot({

    # Check if the buttom was clicked
    if (values$default == 0) {

      set.seed(423432)
      error <- rnorm(n)

    } else {

      error <- eps()

    }

    # Estimate lm and compute Y-related quantities
    Y <- -0.25 * X1 + 0.25 * X2 + input$sigma * error
    my <- mean(Y)
    data$Z <- Y
    mod <- lm(Z ~ X + Y, data = data)
    pred <- predict(mod, newdata = new, type = "response")

    # Info for title
    sst <- sum((Y - my)^2)
    ssr <- sum((mod$fitted.values - my)^2)
    sse <- sum((Y - mod$fitted.values)^2)
    R2 <- sprintf("%.2f", ssr / sst)
    sst <- paste("SST(", sprintf("%.2f", sst), ")", sep = "")
    ssr <- paste("SSR(", sprintf("%.2f", ssr), ")", sep = "")
    sse <- paste("SSE(", sprintf("%.2f", sse), ")", sep = "")

    # Plot data
    gridMat <- scatter3D(data$X, data$Y, data$Z, pch = 16, theta = input$theta,
                         phi = input$phi, bty = "g", axes = TRUE, colkey = FALSE,
                         col = 1, xlim = range(gX), ylim = range(gY),
                         zlim = c(-5, 5), nticks = nGrid, cex = 1,
                         ticktype = "detailed", xlab = "x1", ylab = "x2",
                         zlab = "y", type = "n")

    # Segments
    d <- 0.075
    segments3D(x0 = data$X - d, y0 = data$Y, z0 = data$Z, x1 = data$X - d,
               y1 = data$Y, z1 = rep(my, n), col = 4, lwd = 3, add = TRUE)
    segments3D(x0 = data$X + d, y0 = data$Y, z0 = rep(my, n), x1 = data$X + d,
               y1 = data$Y, z1 = mod$fitted.values, col = "forestgreen",
               lwd = 3, add = TRUE)
    segments3D(x0 = data$X, y0 = data$Y, z0 = data$Z, x1 = data$X, y1 = data$Y,
               z1 = mod$fitted.values, col = "orange", lwd = 3, add = TRUE)

    # Points
    points3D(data$X, data$Y, data$Z, pch = 16, add = TRUE, col = 1, cex = 1.5)

    # Regression and mean
    surf3D(x = M$x, y = M$y, z = matrix(rnorm(lx * ly, mean = my, sd = 1e-6),
                                        nrow = lx, ncol = ly),
           col = addAlpha("red", 0), border = addAlpha("red", 0.5), lty = 2,
           add = TRUE)
    surf3D(x = M$x, y = M$y, z = matrix(pred, nrow = lx, ncol = ly),
           col = addAlpha("red", 0.1), border = addAlpha("red", 0.2), add = TRUE)

    # Legend
    legend("bottomright", legend = expression("Fitted plane", "Sample mean " * bar(Y),
                                          (Y[i] - bar(Y))^2, (hat(Y)[i] - bar(Y))^2,
                                          (hat(Y)[i] - Y[i])^2),
           col = c(2, 2, 4, "forestgreen", "orange"), lty = c(1, 2, 1, 1, 1),
           lwd = c(2, 1, 3, 3, 3), cex = 1.5)

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


