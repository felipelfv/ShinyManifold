library(plotly)

#the parameters of the object manifold (as if it was a probability density)
n_points <- 10000   #random number of points on the manifold
n_dims <- 3        #dimensions of the manifold
noise_level <- 0.1 #random level of noise in the manifold just for the natural variability 

#function that generates points on the manifold
generate_manifold <- function(n_points, n_dims, noise_level) {
  #generating the random set of points in the manifold space
  X <- matrix(runif(n_points * n_dims), nrow = n_points)
  
  #noise to the points to increase variability
  noise <- matrix(rnorm(n_points * n_dims, 0, noise_level), nrow = n_points)
  X <- X + noise
  
  #transformation to the manifold to create a curved shape
  X[, 2] <- 0.5 * sin(3 * X[, 1]) + 0.5 * cos(2 * X[, 3])
  X[, 3] <- 0.5 * sin(2 * X[, 2]) + 0.5 * cos(3 * X[, 1])
  
  return(X)
}

#the points on the manifold
X <- generate_manifold(n_points, n_dims, noise_level)

#3D plot of the manifold using Plotly
plot_ly(x = X[,1], y = X[,2], z = X[,3], type = "scatter3d", mode = "markers", 
        marker = list(size = 2, color = X[,1], colorscale = "Cividis")) %>%
  layout(scene = list(xaxis = list(title = "Dimension 1"),
                      yaxis = list(title = "Dimension 2"),
                      zaxis = list(title = "Dimension 3")))