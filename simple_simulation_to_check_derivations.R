
library(MASS)
library(matrixcalc)
library(reshape2)


#Set up:
#1. Generate covariates. Maybe 7 days baseline, 7 days Rx1
#2. Generate treatment assignment: Half tx, half control 
#3. Generate random intercepts. Correlated. 
#4. Specify parameter values for fixed effects
#5. Generate outcomes as a function of fixed and random effects. c

#Things to vary:
# Correlation between random effects c(0, 0.33, .66)
# Proportion zero days. As this goes up more benefit (power) of joint modeling: c(0.25, 0.50, 0.75)
# Sample size c(100, 200, 300)
# No effects of time. 

N=1000
t=6 # number of repeated risk factors


id <- rep(1:N, each=(t))
time <- rep(1:t, N)

d <- cbind(id, time)

#######################################
# Draw random effects #################
#######################################

sds <- c(8, 1)
D <- diag(sds)
R <- matrix(c(1, 0.5, 
              0.5, 1), 2,2)

S <- round(D %*% R %*% D, 3)
is.symmetric.matrix(S)
is.positive.definite(S)

# random intercepts for each of the 4 sub-models
# Want to be able to vary the correlations 
re <- data.frame(mvrnorm(n=N, mu=c(0, 0), Sigma=S))
re$id <- 1:N
re$mix <- rbinom(N, 1, 0.5) # generate mixture indicator




#######################################
# Generate survival times##############
#######################################

eventdf <- re

gamma00 <- 2.6 # first mixture intercept
gamma01 <- 2.3 # second mixture intercept

sigma00 <- .7 # first mixture residual sd
sigma01 <- .5 # first mixture residual sd

alpha0 <- .01 # coefficeint on random intercept
alpha1 <- .3 # coefficient on random slope

eventdf$mean0 <- gamma00 + alpha0*eventdf$X1 
#+ alpha1*eventdf$X2

eventdf$mean1 <- gamma01 + alpha0*eventdf$X1 
#+ alpha1*eventdf$X2

for (i in 1:nrow(eventdf)){
  eventdf$chd[i] <- ifelse(eventdf$mix[i]==0, rlnorm(n=1, meanlog=eventdf$mean0[i], sdlog=sigma00),
                           rlnorm(n=1, meanlog=eventdf$mean1[i], sdlog=sigma01))
}

mean(eventdf$chd[eventdf$mix==0])
hist(eventdf$chd[eventdf$mix==0])
mean(eventdf$chd[eventdf$mix==1])
hist(eventdf$chd[eventdf$mix==1])

### Censor if greater than a certain value

eventdf$censor <- ifelse(eventdf$chd > 27, 1, 0)
eventdf$chd <- ifelse(eventdf$chd>27, 27, eventdf$chd)


######################################
# Generate risk factor trajectories
######################################

riskdf <- merge(d, re, by="id")

beta0 <- 200
beta1 <- 4

riskdf$mean <- beta0 + beta1*riskdf$time + riskdf$X1 + riskdf$X2*riskdf$time


for (i in 1:nrow(riskdf)){
  riskdf$chol[i] <- rnorm(n=1, mean=riskdf$mean[i], sd=10)
}


fit <- lmer(chol ~ time + (1 | id), data=riskdf)
summary(fit)
