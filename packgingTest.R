sv <- simulate_things(dnorm, c(-Inf,Inf), n = 10000)
print(length(sv))
histogram(sv, col='blue', prob=TRUE, bw=.5)


###################     test     ###################

## density plot

# add plots comparing the simlated density and the actual density
# for our adaptive rejection sampling function and built-in R function

par(mfrow = c(1, 2))

plot(density(sv), main = "ARS")
lines(a <- seq(-4.5, 4.5, by = 0.05), dnorm(a), col='blue')

plot(density(rnorm(10000)), main = "built-in fuuction")
lines(a <- seq(-4.5, 4.5, by = 0.05), dnorm(a), col='blue')

par(mfrow = c(1, 1))
legend(4.5,.35,lty=c(1,1),text.font=0.1,cex=0.5,c("sampled","true density"),col = c("black","blue"))
dev.off()

## qq plot

qqplot(qnorm(ppoints(500)), sv,
main = expression("Q-Q plot: AQS for std. Normal"))

## Kolmogorov-Smirnov Tests

ks_res <- ks.test(sv, pnorm)
ks_res_bt <- ks.test(rnorm(10000), pnorm)

## Cram´er-Von Mises (CvM) Test

install.packages("goftest")
library(goftest)
cvm_res <- cvm.test(sv, "pnorm", mean=0, sd=1)
cvm_res_bt <- cvm.test(rnorm(10000), "pnorm", mean=0, sd=1)

## Anderson-Darling (AD) Test

library(goftest)
ad_res <- ad.test(sv, "pnorm", mean=0, sd=1)
ad_res_bt <- cvm.test(rnorm(10000), "pnorm", mean=0, sd=1)

## summary table

summary <- data.frame(c(ks_res$p.value, cvm_res$p.value, ad_res$p.value),
c(ks_res$p.value > 0.05, cvm_res$p.value > 0.05, ad_res$p.value > 0.05),
c(ks_res_bt$p.value, cvm_res_bt$p.value, ad_res_bt$p.value),
row.names = c("Kolmogorov-Smirnov","Cram´er-Von Mises","Anderson-Darling"))
colnames(summary) <- c("ARS p-value","Significant", "Built-in p-value")
print(summary)

