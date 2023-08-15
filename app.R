library(shiny)
library(plotly)

# The function you provided
generate_manifold <- function(n_points, n_dims, noise_level) {
  X <- matrix(runif(n_points * n_dims), nrow = n_points)
  noise <- matrix(rnorm(n_points * n_dims, 0, noise_level), nrow = n_points)
  X <- X + noise
  X[, 2] <- 0.5 * sin(3 * X[, 1]) + 0.5 * cos(2 * X[, 3])
  X[, 3] <- 0.5 * sin(2 * X[, 2]) + 0.5 * cos(3 * X[, 1])
  return(X)
}

# Define Shiny app
ui <- fluidPage(
  titlePanel("Interactive Manifold"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_points", "Number of Points:", min = 100, max = 20000, value = 10000),
      sliderInput("noise_level", "Noise Level:", min = 0, max = 1, value = 0.1, step = 0.01)
    ),
    mainPanel(
      plotlyOutput("plot3d")
    )
  )
)

server <- function(input, output, session) {
  
  output$plot3d <- renderPlotly({
    X <- generate_manifold(input$n_points, 3, input$noise_level)
    plot_ly(x = X[,1], y = X[,2], z = X[,3], type = "scatter3d", mode = "markers", 
            marker = list(size = 2, color = X[,1], colorscale = "Cividis")) %>%
      layout(scene = list(xaxis = list(title = "Dimension 1"),
                          yaxis = list(title = "Dimension 2"),
                          zaxis = list(title = "Dimension 3")))
  })
  
}

shinyApp(ui = ui, server = server)


