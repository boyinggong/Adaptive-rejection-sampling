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

find_init <- function(g, my_total_range){
    h <- function(x) { log(g(x)) }
    if (my_total_range[1] == -Inf & my_total_range[2] == Inf){
        if (dh(0, g) == 0) x = 1 else x = 0
        if (dh(x, g) > 0){
            x2 = x
            have_found = FALSE
            while(have_found == FALSE){
                x2 = x2 + 1
                have_found = dh(x2, g) < 0
            }
            return(c(x, x2))
        }else{
            x1 = x
            have_found = FALSE
            while(have_found == FALSE){
                x1 = x1 - 1
                have_found = dh(x1, g) > 0
            }
            return(c(x1, x))
        }
    }else if (my_total_range[1] == -Inf & my_total_range[2] != Inf){
        if(dh(my_total_range[2], g) > 0) return(c(my_total_range[2]-1, my_total_range[2]-2))
        else{
            x1 = my_total_range[2]
            have_found = FALSE
            while(have_found == FALSE){
                x1 = x1 - 1
                have_found = dh(x1, g) > 0
            }
            return(c(x1-0.01, my_total_range[2]-0.01))
        }
    }else if (my_total_range[1] != -Inf & my_total_range[2] == Inf){
        if(dh(my_total_range[1], g) < 0) return(c(my_total_range[1]+1, my_total_range[1]+2))
        else{
            x2 = my_total_range[1]
            have_found = FALSE
            while(have_found == FALSE){
                x2 = x2 + 1
                have_found = dh(x2, g) < 0
            }
            return(c(my_total_range[1]+0.01, x2+0.01))
        }
    }else{
        return(c(2/3*my_total_range[1]+1/3*my_total_range[2],
        1/3*my_total_range[1]+2/3*my_total_range[2]))
    }
}


