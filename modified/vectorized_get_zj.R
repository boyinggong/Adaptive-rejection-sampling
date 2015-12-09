x_values <- seq(1, 3, by = .001)
y_values <- seq(5, 7, by = .001)
slopes <- seq(9, 11, by = .001)

################    original function   ################

get_zj <- function(x_values, y_values, slopes)
{
  zj <- rep(0, length(x_values)-1)
  for (i in seq(length(zj)))
  {
    zj[i] <- (y_values[i+1] - y_values[i] - 
                (x_values[i+1] * slopes[i+1]) + 
                (x_values[i] * slopes[i])) /
      (slopes[i] - slopes[i+1])
  }
  return(zj)
}

################    vectorized function   ################

get_zj_new <- function(x_values, y_values, slopes)
{
  n <- length(x_values)
  zj <- (y_values[2:n] - y_values[1:(n-1)] -
           (x_values[2:n] * slopes[2:n]) +
           (x_values[1:(n-1)] * slopes[1:(n-1)])) /
    (slopes[1:(n-1)] - slopes[2:n])
  return(zj)
}

################     compare time used    ################

benchmark(zj <- get_zj(x_values, y_values, slopes), zj_new <- get_zj_new(x_values, y_values, slopes))

# test replications elapsed relative user.self sys.self user.child
# 1         zj <- get_zj(x_values, y_values, slopes)          100   0.810       30     0.799    0.006          0
# 2 zj_new <- get_zj_new(x_values, y_values, slopes)          100   0.027        1     0.025    0.002          0
# sys.child
# 1         0
# 2         0

################      compare results     ################

all(zj == zj_new)

# [1] TRUE

