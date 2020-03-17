###################################################################
# Project:    COVID-19 
# This file:  CPAC as sentinel event
# When/Who:   Andrew Lover/ Tom McAndrew Mar 16 2020
###################################################################

loadLibraries <- function(){
    require(dplyr)
}

storeAsCSV <- function(d,filname){
    d.df  <- as.data.frame(d, row.names = NULL, optional = FALSE, make.names = TRUE)
    write.csv(d,sprintf('./analysisData/%s',filname))
}

predict_n <- function(n0, d, ndays) n0 * 2^(ndays/d)

main <- function(){
    loadLibraries()

    # Prelims
    rm(list=ls())
    set.seed(14411)

    # Parameters from CPAC and US Census Bureau
    n_cpac   <- 1                        # Number of positive cases at CPAC
    pop_cpac <- 2*10^4                   # Estimated number of attendees at CPAC
    pop_usa  <- 329*10^6                 # US population according to the US Census Bureau
    n0hat    <- pop_usa*n_cpac/pop_cpac  # Assumed number  of US positive

    dhat  <- 6.4
    ndays <- seq(0,15,1) # 0 added so that first element is n0

    # Compute the mean trajectory
    predicthat     <- predict_n(n0 = n0hat, d  = dhat, ndays = ndays)
    storeAsCSV(predicthat, "meanPrediction.csv")

    #------------------------------------------------------------
    # Number of Monte Carlo Draws
    N <- 3*10^4

    # Sample from I0
    # prior is assumed to follow the CPAC incidence with a sample size of 1
    pop_prior <- 1
    n_prior   <- pop_prior*n_cpac/pop_cpac

    prop.s <- rbeta(N, n_cpac + n_prior, pop_cpac + pop_prior - n_cpac - n_prior)
    n0.s   <- prop.s*pop_usa

    # Sample from D
    # D is assumed to follow a lognormal distribution
    # Doubling time is estimated from literature
    d.s <- exp(rnorm(N, log(6.4), sd = (log(7.1) - log(5.8))/4))

    # Store sampled trajectories
    predict.st <- matrix(NA, N, length(ndays))
    for (s in 1:N){
        predict.st[s,] <- predict_n(n0 = n0.s[s], d  = d.s[s], ndays = ndays)
    }; storeAsCSV(predict.st, "sampledTrajectories.csv")

    # Store 80% confidence intervals
    ci80.qt <- apply(predict.st, 2, quantile, c(0.1, 0.9))    ## yes
    storeAsCSV(ci80.qt, "_80%CIS.csv")
};main()
