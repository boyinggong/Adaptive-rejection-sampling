
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# calculate the derivative of logarithm of functions
dh <- function(x,g) {
    x <- as.numeric(x)
    return(mosaic::D(log(g(x)) ~ x)(x))
}