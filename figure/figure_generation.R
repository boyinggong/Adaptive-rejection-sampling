library('devtools')
## replace with your path to ars
setwd('~/Dropbox/stat243_final_project/243FinalProject/ars')
load_all()
devtools::test()
check()

sv <- ars(dnorm, c(-Inf,Inf), n = 100000)
hist(sv, col='blue', prob=TRUE, breaks=100, xlim=c(-3,3), xlab = "x", main="Histogram of Values Sampled with ARS")
x <- seq(-4,4,.01)
lines(x, dnorm(x),lty=5,lwd=2,col="red")
legend("topright", c("Sample Values", "Analytical Form"), col=c("blue", "red"), lwd=2, cex = 0.7)


sv <- ars(dunif, c(0,1), n = 100000)
hist(sv, col='blue', prob=TRUE, breaks=50, xlab = "x", main="Histogram of Values Sampled with ARS")
x <- seq(0,1,.01)
lines(x, dunif(x),lty=5,lwd=2,col="red")
legend("bottomright", c("Sample Values", "Analytical Form"), col=c("blue", "red"), lwd=2, cex = 0.7)


sv <- ars(dexp, c(0,Inf), n = 100000)
hist(sv, col='blue', prob=TRUE, breaks=100, xlim = c(0,3), xlab = "x", main="Histogram of Values Sampled with ARS")
x <- seq(0,6,.01)
lines(x, dexp(x),lty=5,lwd=2,col="red")
legend("topright", c("Sample Values", "Analytical Form"), col=c("blue", "red"), lwd=2, cex = 0.7)

g <- function(x) { return(dchisq(x, df = 3)) }

sv <- ars(g, c(0.0,Inf), n = 100000)
hist(sv, col='blue', prob=TRUE, breaks=100, xlim = c(0,12), xlab = "x", main="Histogram of Values Sampled with ARS")
x <- seq(0,12,.01)
lines(x, g(x),lty=5,lwd=2,col="red")
legend("topright", c("Sample Values", "Analytical Form"), col=c("blue", "red"), lwd=2, cex = 0.7)
