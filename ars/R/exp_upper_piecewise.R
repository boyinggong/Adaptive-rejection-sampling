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

# The exponential of the upper piecewise function
exp_upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
    return(exp(upper_piecewise(x, x_intercepts, y_values, slopes, domains)))
}