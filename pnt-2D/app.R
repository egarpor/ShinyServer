
library(pnt)
library(shiny)

# Visualization PC1Curve
# (x1, x2, x3) parametrize u as a point in S^3
# (y1, y2) parametrize v as a point in S^2, which is then lifted to the 
# nested sphere within S^3 that is orthogonal to u

# Common objects
N <- 1e3
alpha <- seq(-pi, pi, l = N)
col <- rainbow(N)
s <- 1 / 20
anim <- animationOptions(interval = 300, loop = TRUE, playButton = NULL,
                 pauseButton = NULL)

# UI for application
ui <- fluidPage(align = "center",
                
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "w", label = "winding number:",
                   choices = c(1, 2), selected = 1, inline = TRUE), 
      sliderInput(inputId = "x1", label = "u(x1):", min = 0, max = 1, 
                  value = s, step = s, animate = anim, post = "pi"),
      sliderInput(inputId = "x2", label = "u(x2):", min = 0, max = 1, 
                  value = 2 * s, step = s, animate = anim, post = "pi"),
      sliderInput(inputId = "x3", label = "u(x3):", min = -1, max = 1, 
                  value = 3 * s, step = 2 * s, animate = anim, post = "pi"),
      sliderInput(inputId = "y1", label = "v(y1):", min = 0, max = 1, 
                  value = 4 * s, step = s, animate = anim, post = "pi"),
      sliderInput(inputId = "y2", label = "v(y2):", min = -1, max = 1, 
                  value = 5 * s, step = 2 * s, animate = anim, post = "pi")
      ),
    
    mainPanel(
      plotOutput("plot")
    )
    
  )
  
)

# Server logic
server <- function(input, output) {

  output$plot <- renderPlot({
    
    # w
    w <- as.integer(input$w)
    
    # u, v
    u <- anglesToSphere(theta = pi * c(input$x1, input$x2, input$x3))
    v <- c(anglesToSphere(theta = pi * c(input$y1, input$y2)) %*% 
             Hu(u = u)[-1, ])
    
    # PC1Curve
    indCos <- seq(1, 4, by = 2)
    x <- cos(alpha / w) %o% u + sin(alpha / w) %o% v
    y <- matrix(atan2(y = x[, indCos + 1], x = x[, indCos]), nrow = N, 
                ncol = 2)[, , drop = FALSE] * rep(c(w, w), each = N)
    y <- toPiInt(y)
    
    # Plot
    plot(y, xlim = c(-pi, pi), ylim = c(-pi, pi), main = paste(
      "u = (", paste(sprintf("%.2f", u), collapse = ", "), ")\n",
      "v = (", paste(sprintf("%.2f", v), collapse = ", "), ")", 
      sep = ""), axes = FALSE, xlab = expression(theta[1]),
      ylab = expression(theta[2]), col = col)
    torusAxis()
    
  }, width = 650, height = 650)
  
}

# Run the application
shinyApp(ui = ui, server = server)
