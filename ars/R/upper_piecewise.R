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

# The upper piecewise function
upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
    n <- length(domains)-1
    fx <- (x > domains[1:n] & x <= domains[2:(n+1)]) *
    (((x - x_intercepts[1:n]) * slopes[1:n]) + y_values[1:n])
    return(sum(fx))
}