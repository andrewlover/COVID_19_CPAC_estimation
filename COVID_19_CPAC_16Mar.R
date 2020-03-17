###################################################################
# Project:    COVID-19 
# This file:  CPAC as sentinel event
# When/Who:   Andrew Lover/ Tom McAndrew Mar 16 2020
###################################################################

# Prelims
rm(list=ls())
graphics.off()
set.seed(14411)

###################################################################
# I. Setup and projections
###################################################################
predict_n <- function(n0, d, ndays) n0 * 2^(ndays/d)
# prediction
predict_n <- function(n0, d, ndays) n0 * 2^(ndays/d)

# point estimate

# cpac analysis
n_cpac <- 1 # incidence
pop_cpac <- 20000
pop_usa <- 329*10^6 # US Census Bureau
n0hat <- pop_usa*n_cpac/pop_cpac

dhat <- 6.4
ndays <- seq(0,15) # 0 added so that first element is n0
predicthat <- predict_n(n0 = n0hat, d  = dhat, 
                        ndays = ndays)
plot(predicthat ~ ndays, type = "l", 
     xlab = "Days since March 1st",
     ylab = "Prediction")

# uncertainty
nsamp <- 30000 # increased for better precision for beta draws

# beta distribution for relative incidence proportion
# for prior: use same point estimate, sample size of 1

pop_prior <- 1
n_prior <- pop_prior*n_cpac/pop_cpac
# curve(dbeta(x, n_cpac + n_prior, pop_cpac + pop_prior))
prop.s <- rbeta(nsamp, n_cpac + n_prior, pop_cpac + pop_prior - n_cpac - n_prior)
n0.s <- prop.s*pop_usa

#hist(n0hat.s)
#mean(n0hat.s)
#median(n0hat.s)

quantile(n0.s, c(0.025, 0.975))
quantile(n0.s, c(0.1, 0.9))   # 80% interval for n0
quantile(n0.s, c(0.25, 0.75)) # 50% interval for n0

# d 6.4 days (95% CI: 5.8-7.1) # Lancet 2020 doubling time estiamtes
# let's assume lognormal for doubling time

d.s <- exp(rnorm(nsamp, log(6.4), sd = (log(7.1) - log(5.8))/4))
## check quantiles
quantile(d0.s, c(0.025, 0.975))

## sens analysis analysis for distrution  ##
# library(truncnorm)
# d.s <- rtruncnorm(nsamp, mean=6.4,sd=(1.3/4)^2, a=0,b=Inf)

###################################################################
# II. Projections
###################################################################

# lazy looping
predict.st <- matrix(NA, nsamp, length(ndays))
for (s in 1:nsamp){
  predict.st[s,] <- predict_n(n0 = n0.s[s], d  = d.s[s], 
                              ndays = ndays)
}

ci.qt <- apply(predict.st, 2, quantile, c(0.025, 0.975))

plot(predicthat ~ ndays, type = "l", 
     xlab = "Days since March 1st",
     ylab = "Prediction",
     ylim = range(ci.qt))
lines(ci.qt[1,] ~ ndays, lty = 2)
lines(ci.qt[2,] ~ ndays, lty = 2)

ci80.qt <- apply(predict.st, 2, quantile, c(0.1, 0.9))    ## yes
ci80.qt
ci50.qt <- apply(predict.st, 2, quantile, c(0.25, 0.75))  ## not useful
ci50.qt

###################################################################
# III. Plots
###################################################################

plot(predicthat ~ ndays, type = "l", 
     xlab = "Days since March 1st (CPAC)",
     ylab = "Total predicted infections, nationaly", las = 0)
lines(ci80.qt[1,] ~ ndays, lty = 2)
lines(ci80.qt[2,] ~ ndays, lty = 2)

###################################################################
# IV.Stata bits for plotting
###################################################################

ci.qt <- apply(predict.st, 2, quantile, c(0.025, 0.975))
plot(predicthat ~ ndays, type = "l", 
     xlab = "Days since March 1st (CPAC)",
     ylab = "Total predicted infections, nationaly", las = 0)
lines(ci.qt[1,] ~ ndays, lty = 2)
lines(ci.qt[2,] ~ ndays, lty = 2)
abline(v=14, col="maroon", lwd=2, lty=2)

ci80.qt <- apply(predict.st, 2, quantile, c(0.1, 0.9))
ci80.qt

plot(predicthat ~ ndays, type = "l", 
     xlab = "Days since March 1st (CPAC)",
     ylab = "Total predicted infections, nationaly", las = 0)
lines(ci80.qt[1,] ~ ndays, lty = 2)
lines(ci80.qt[2,] ~ ndays, lty = 2)
abline(v=14, col="maroon", lwd=2, lty=2)

predict2 <- as.data.frame(predict.st, row.names = NULL, optional = FALSE, make.names = TRUE)
phat2 <- as.data.frame(predicthat, row.names = NULL, optional = FALSE, make.names = TRUE)
ci80.qt2 <- as.data.frame(ci80.qt, row.names = NULL, optional = FALSE, make.names = TRUE)

write.dta(phat2, "~/Downloads/phat2.dta")
write.dta(ci80.qt2, "~/Downloads/ci80_qt2.dta")

predict2 <- as.data.frame(predict.st, row.names = NULL, optional = FALSE, make.names = TRUE)
write.dta(predict2, "~/Downloads/predict2.dta")

###################################################################
# V. End
###################################################################


