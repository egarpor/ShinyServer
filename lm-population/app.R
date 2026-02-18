#
# Shiny web application for illustrating the linear model assumptions at the
# population level and several possible distributions of X
#

library(shiny)
library(plotly)

# =========================
# UI
#=========================

ui <- fluidPage(
  align = "center",

  # Input panel (sliders will be centered automatically)
  inputPanel(
    sliderInput("b0", "Intercept:",
                min=-2, max=2, value=0.5, step=0.1),
    sliderInput("b1", "Slope",
                min=-3, max=3, value=1, step=0.1),
    sliderInput("sigma2", "Error variance:",
                min=0.5, max=2, value=1.1, step=0.1),
    sliderInput("x0", "Conditioning value x0:",
                min=-3, max=3, value=0, step=0.1),
    selectInput("distX", "Distribution of X:",
                choices=c("Normal","Uniform","Discrete")),
    actionButton(inputId = "newSample",
                 label = HTML("<h5>Get a new<br> sample!</h5>"))
  ),

  # Centered plot
  div(class = "centered-plot",
      plotlyOutput("jointPlot", height="750px", width = "100%")
  )
)

# =========================
# SERVER
#=========================

server <- function(input, output, session){

  # Reactive values to store the current sample AND camera
  sample_data <- reactiveValues(
    Xs = NULL,
    Ys = NULL,
    current_b0 = 0.5,
    current_b1 = 1,
    current_sigma2 = 1.1,
    current_distX = "Normal",
    # Store the original errors (epsilon) for transformation
    epsilon = NULL
  )

  # Store camera position - initialize with default
  camera_pos <- reactiveVal(list(
    eye = list(x = -1.25, y = -1.75, z = 1.6)
  ))

  # Function to generate initial base sample
  generate_base_sample <- function(distX) {
    n_sample <- 100

    # Sample X based on distribution
    if(distX == "Normal"){
      Xs <- rnorm(n_sample, 0, 1)
    } else if(distX == "Uniform"){
      Xs <- runif(n_sample, -2, 2)
    } else { # Discrete
      support <- c(-2,-1,0,1,2)
      probs   <- c(0.1,0.2,0.4,0.2,0.1)
      Xs <- sample(support, n_sample, replace=TRUE, prob=probs)
    }

    # Generate errors (will be transformed later)
    epsilon <- rnorm(n_sample, 0, 1)  # Standard normal errors

    list(Xs = Xs, epsilon = epsilon)
  }

  # Function to compute Y from parameters and errors
  compute_Y <- function(b0, b1, sigma2, Xs, epsilon) {
    b0 + b1*Xs + sqrt(sigma2) * epsilon
  }

  # Initialize or get new base sample (when distX changes or newSample button)
  observeEvent(
    list(input$newSample, input$distX),
    {
      # Generate new base sample
      base <- generate_base_sample(input$distX)
      sample_data$Xs <- base$Xs
      sample_data$epsilon <- base$epsilon

      # Compute Y with current parameters
      sample_data$Ys <- compute_Y(
        input$b0, input$b1, input$sigma2,
        base$Xs, base$epsilon
      )

      # Update current parameters
      sample_data$current_b0 <- input$b0
      sample_data$current_b1 <- input$b1
      sample_data$current_sigma2 <- input$sigma2
      sample_data$current_distX <- input$distX
    },
    ignoreNULL = FALSE
  )

  # Transform sample when parameters change (not when newSample or distX change)
  observeEvent(
    list(input$b0, input$b1, input$sigma2),
    {
      # Only transform if we have a base sample
      if(!is.null(sample_data$Xs) && !is.null(sample_data$epsilon)) {

        # Compute new Ys by transforming the existing sample
        sample_data$Ys <- compute_Y(
          input$b0, input$b1, input$sigma2,
          sample_data$Xs, sample_data$epsilon
        )

        # Update current parameters
        sample_data$current_b0 <- input$b0
        sample_data$current_b1 <- input$b1
        sample_data$current_sigma2 <- input$sigma2
      }
    },
    ignoreNULL = FALSE
  )

  output$jointPlot <- renderPlotly({

    b0 <- input$b0
    b1 <- input$b1
    sigma <- sqrt(input$sigma2)
    x0 <- input$x0
    distX <- input$distX

    # Use the stored sample data
    Xs <- sample_data$Xs
    Ys <- sample_data$Ys

    # If no sample exists yet, generate one
    if(is.null(Xs)) {
      base <- generate_base_sample(distX)
      Xs <- base$Xs
      epsilon <- base$epsilon
      Ys <- compute_Y(b0, b1, input$sigma2, Xs, epsilon)

      # Store in reactiveValues
      sample_data$Xs <- Xs
      sample_data$epsilon <- epsilon
      sample_data$Ys <- Ys
    }

    # -----------------------
    # Define fX dynamically and find its maximum
    # -----------------------

    if(distX == "Normal"){
      fX <- function(x) dnorm(x, 0, 1)
      max_fX <- dnorm(0, 0, 1)
    } else if(distX == "Uniform"){
      fX <- function(x) dunif(x, -2, 2)
      max_fX <- dunif(0, -2, 2)
    } else { # Discrete
      support <- c(-2,-1,0,1,2)
      probs   <- c(0.1,0.2,0.4,0.2,0.1)
      fX <- function(x){
        sapply(x, function(xx){
          if(xx %in% support)
            probs[which(support==xx)]
          else 0
        })
      }
      max_fX <- max(probs)
    }

    # -----------------------
    # FIXED AXIS LIMITS based on slider extremes
    # -----------------------
    x_limits <- c(-3, 3)

    # Calculate Y limits based on extreme parameter values
    max_y_line <- max(2 + 3*3, 2 + (-3)*(-3), -2 + 3*3, -2 + (-3)*(-3))
    min_y_line <- min(-2 + (-3)*3, -2 + 3*(-3), 2 + (-3)*3, 2 + 3*(-3))

    # Add margin for error (3 * max sigma) and some padding
    margin <- 3*2 + 1
    y_limits <- c(min_y_line - margin, max_y_line + margin)

    # Make symmetric for better visualization
    y_max <- max(abs(y_limits))
    y_limits <- c(-y_max, y_max)

    # -----------------------
    # FIXED Z-AXIS LIMITS
    # -----------------------

    z_limit <- 0.6

    # -----------------------
    # Grid using fixed limits
    # -----------------------

    x <- seq(x_limits[1], x_limits[2], length=7^3)
    y <- seq(y_limits[1], y_limits[2],
             length=if (distX %in% c("Normal", "Uniform")) 200 else 100)

    z <- outer(x, y, Vectorize(function(x,y){
      fX(x) * dnorm(y, mean=b0 + b1*x, sd=sigma)
    }))

    z <- t(z)

    # -----------------------
    # Conditional, slice, and marginal
    # -----------------------

    y_full <- seq(y_limits[1], y_limits[2], length=300)

    joint_slice <- fX(x0) *
      dnorm(y_full, mean=b0 + b1*x0, sd=sigma)

    cond_density <- if (fX(x0) > 0) {
      dnorm(y_full, mean=b0 + b1*x0, sd=sigma)
    } else {
      0
    }

    marginal_density <- fX(x)

    # -----------------------
    # Plot with fixed axis limits
    # -----------------------

    fig <- plot_ly(source = "lm_plot") %>%
      event_register('plotly_relayout')  # Register for camera events

    # Joint surface
    fig <- fig %>% add_surface(
      x=~x, y=~y, z=~z,
      opacity=0.6,
      colorscale=list(c(0,1), c("lightblue","blue")),
      showscale=FALSE,
      hoverinfo="none",
      name="Joint Distribution"
    )

    # Regression line (z=0)
    fig <- fig %>% add_trace(
      type="scatter3d",
      mode="lines",
      x=x,
      y=b0 + b1*x,
      z=rep(0,length(x)),
      line=list(color="red", width=6),
      name="Regression line",
      hoverinfo="none",
      hovertemplate=paste('<extra></extra>')
    )

    # Sample points
    fig <- fig %>% add_trace(
      type="scatter3d",
      mode="markers",
      x=Xs,
      y=Ys,
      z=0,
      marker=list(
        size=2,
        color="black",
        opacity=0.6
      ),
      name="Sample",
      hoverinfo="none",
      hovertemplate=paste('<extra></extra>')
    )

    # Joint slice
    fig <- fig %>% add_trace(
      type="scatter3d",
      mode="lines",
      x=rep(x0,length(y_full)),
      y=y_full,
      z=joint_slice,
      line=list(color="blue", width=6),
      name="f_XY(x0, ·) slice",
      hoverinfo="none",
      hovertemplate=paste('<extra></extra>')
    )

    # Conditional
    fig <- fig %>% add_trace(
      type="scatter3d",
      mode="lines",
      x=rep(x0,length(y_full)),
      y=y_full,
      z=cond_density,
      line=list(color="orange", width=6),
      name="Conditional f_{Y|X=x0}",
      hoverinfo="none",
      hovertemplate=paste('<extra></extra>')
    )

    # Marginal (placed at top of Y range)
    fig <- fig %>% add_trace(
      type="scatter3d",
      mode="lines",
      x=x,
      y=rep(y_limits[2], length(x)),
      z=marginal_density,
      line=list(color="lightblue", width=6),
      name="Marginal f_X",
      hoverinfo="none",
      hovertemplate=paste('<extra></extra>')
    )

    fig <- fig %>% layout(
      dragmode = "turntable",
      scene = list(
        xaxis = list(title="X", range = x_limits, autorange = FALSE),
        yaxis = list(title="Y", range = y_limits, autorange = FALSE),
        zaxis = list(
          title="Density",
          range = c(0, z_limit),
          autorange = FALSE
        ),
        camera = camera_pos()  # Use stored camera position
      )
    ) %>%
      config(
        scrollZoom = FALSE,  # This disables scroll zoom
        displayModeBar = TRUE
      )

    fig
  })

  # Update stored camera position when user changes it
  observeEvent(event_data("plotly_relayout", source = "lm_plot"), {
    relayout_data <- event_data("plotly_relayout", source = "lm_plot")

    if (!is.null(relayout_data)) {
      # Check for camera updates
      if (!is.null(relayout_data$scene.camera)) {
        # Full camera update
        camera_pos(relayout_data$scene.camera)
      } else {
        # Individual component updates
        current_cam <- camera_pos()
        updated <- FALSE

        if (!is.null(relayout_data$scene.camera.eye)) {
          current_cam$eye <- relayout_data$scene.camera.eye
          updated <- TRUE
        }
        if (!is.null(relayout_data$scene.camera.center)) {
          current_cam$center <- relayout_data$scene.camera.center
          updated <- TRUE
        }
        if (!is.null(relayout_data$scene.camera.up)) {
          current_cam$up <- relayout_data$scene.camera.up
          updated <- TRUE
        }

        if(updated) {
          camera_pos(current_cam)
        }
      }
    }
  })
}

# =========================
# RUN APP
#=========================

shinyApp(ui, server)