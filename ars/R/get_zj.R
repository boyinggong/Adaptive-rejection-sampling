# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Get the intersection points (the 'z_i' defined by the paper)
get_zj <- function(x_values, y_values, slopes)
{
    n <- length(x_values)
    zj <- (y_values[2:n] - y_values[1:(n-1)] -
    (x_values[2:n] * slopes[2:n]) +
    (x_values[1:(n-1)] * slopes[1:(n-1)])) /
    (slopes[1:(n-1)] - slopes[2:n])
    return(zj)
}
