rm(list=ls())

library(haven)
library(gmodels)
library(mgcv)
library(lme4)
library(nlme)


###Notes
# exam variable takes on one value, 1.
# exam year takes on one value, 1991
# 2600 participants missing total cholesterol
# total sample is 18286
# 8.58% died of CVD. 
# lenyfl is the Length of follow-up in years. Recorded for everyone. Then the cvd_dth variable is a censoring
# indicator variable where 0 is censored and 1 is non-censored. And age of death is
# age + length of follow-up. 

setwd("C:/Users/jsi894/Box Sync/LRPP/projects/power_prior") 


aric <- read_sas("C:/Users/jsi894/Box Sync/LRPP/projects/power_prior/power_prior_data/ARIC/ARIC.sas7bdat", NULL)

names(aric)<-tolower(names(aric))

aric <- data.frame(aric)

####FOR NOW DROP 6 PARTICIPANTS WITH total CHolesterol greater than 500
aric <- aric[aric$totchl<500,]


summary(aric[, c("age", "bminow", "chd_dth", "cvd_dth", "exam", "forsmoker", 
                 "lenychd", "lenyfl", "race", "sex", "smoker", "totchl", "tot_dth", "birthyr")])

table(aric$race)
table(aric$sex)
table(aric$chd_dth, aric$cvd_dth)

summary(aric[,"age"])

hist(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1])
table(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1])
mean(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1], na.rm=TRUE)
sd(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1], na.rm=TRUE)

hist(log(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1]))

mean(log(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1]))
sd(log(aric$lenyfl[aric$exam==1 & aric$cvd_dth==1]))



#MEN ONLY WITH COVARIATES
men.tot <- gam(totchl~s(age, bs="cs", k=20), data=aric[aric$sex==1 & aric$exam==4,],
               method="REML")
plot(men.tot)

#WOMEN ONLY WITH COVARIATES
women.tot <- gam(totchl~s(age, bs="cs", k=20), data=aric[aric$sex==2,],
                 method="REML")


men.tot <- gamm(totchl~s(age, bs="cs", k=20), data=aric[aric$sex==1,],
                random=list(id_final=~1))

men.tot <- gamm(totchl~s(age, bs="cs", k=20), data=aric[aric$sex==1,],
                correlation=corAR1(form=~1|id_final))

aric <- aric[!is.na(aric$age),]

aric$cage <- aric$age - mean(aric$age)
aric$cage2 <- aric$cage * aric$cage
aric$cage3 <- aric$cage2 * aric$cage

aric$age2 <- aric$age * aric$age


fit <- lmer(totchl ~ cage + cage2 + cage3 + (1 | id_final), data=aric[!is.na(aric$age),])

fit <- lmer(totchl ~ age + (1 | id_final), data=aric[!is.na(aric$age),])

fit <- lmer(totchl ~ poly(age, 5) + (1 | id_final), data=aric[!is.na(aric$age),])

aric$jimmy <- predict(fit, re.form=NA)

plot(aric$age, aric$jimmy)

#A review of spline function procedures in R

#Aris Perperoglou, Willi Sauerbrei, Michal Abrahamowicz & Matthias Schmid 

#BMC Medical Research Methodology volume 19, Article number: 46 (2019) 






summary(men.tot$lme)
summary(men.tot$gam)
plot(men.tot$gam)