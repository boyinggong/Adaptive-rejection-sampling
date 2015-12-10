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

sk <- function(x, xi, yi, k, my_domains) {
    n <- length(my_domains)-1
    integrals <- ((exp(yi + (k * (my_domains[2:(n+1)] - xi)))
    - exp(yi + (k * (my_domains[1:n] - xi)))) / k)
    return(integrals)
}