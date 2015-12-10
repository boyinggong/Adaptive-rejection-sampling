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

# The lower piecewise function
lower_piecewise <- function(x, x_values, y_values, domains) {
    ux <- ((x < x_values[1]) | (x > x_values[length(x_values)])) * -100
    n <- length(x_values)-1
    u <- ((x > x_values[1:n]) & (x <= x_values[2:(n+1)])) *
    ((x_values[2:(n+1)] - x) * y_values[1:n] +
    (x - x_values[1:n]) * y_values[2:(n+1)]) /
    (x_values[2:(n+1)] - x_values[1:n])
    return(ux+sum(u))
}