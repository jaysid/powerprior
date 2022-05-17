rm(list=ls())

library(readxl)
library(gmodels)
library(mgcv)

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

nhanes <- read_excel("C:/Users/jsi894/Box Sync/LRPP/projects/power_prior/power_prior_data/NHANESIII/NHIII_data.xls.xlsx")

names(nhanes)<-tolower(names(nhanes))

nhanes <- data.frame(nhanes)

#drop other race
nhanes <- nhanes[nhanes$race != 5, ]

nhanes$black <- ifelse(nhanes$race==2, 1, 0)

summary(nhanes$age, nhanes$cvd_dth, nhanes$exam, nhanes$exam_year, nhanes$forsmoker, nhanes$id_final, nhanes$id_original,
        nhanes$lenyanycvd, nhanes$race, nhanes$sex, nhanes$smoker, nhanes$totchl, nhanes$tot_dth)

summary(nhanes[c("age", "cvd_dth", "exam", "exam_year", "forsmoker", "id_final",
                 "lenyanycvd", "race", "sex", "smoker", "totchl", "tot_dth")])

head(nhanes[c("age", "cvd_dth", "forsmoker", "id_final",
              "lenyanycvd", "race", "sex", "smoker", "totchl", "tot_dth")])


CrossTable(nhanes$sex, nhanes$cvd_dth) # around equal proportions of CVD death in men and women

CrossTable(nhanes$forsmoker, nhanes$smoker) #22% former smokers. 30% of current non-smokers. 

CrossTable(nhanes$tot_dth, nhanes$cvd_dth) # 22% died during follow-up. Of those 38% were CVD death. 

CrossTable(nhanes$race) # 67.8% white, 28.8% black, 3.4% other. 

nhanes[1:50,c("age", "cvd_dth", "forsmoker", "id_final",
              "anycvd", "lenyanycvd", "lenyfl", "race", "sex", "smoker", "totchl", "tot_dth")]

summary(nhanes$lenyanycvd-nhanes$lenyfl)



#############
# Model Total Cholesterol as a function of age by sex.
###############

#MEN ONLY WITH COVARIATES
men.tot <- gam(totchl~s(age, bs="cs", k=20), data=nhanes[nhanes$sex==1,],
               method="REML")

#WOMEN ONLY WITH COVARIATES
women.tot <- gam(totchl~s(age, bs="cs", k=20), data=nhanes[nhanes$sex==2,],
                 method="REML")

par(mfrow=c(2,1))

plot(men.tot, shade=TRUE, residuals=FALSE,
     seWithMean=TRUE, select=1, shift=coef(men.tot)[1], xlab="", ylab="")
title(ylab="Total Cholesterol")
title(xlab="Age", line=2.2)
title(main="Men", outer=FALSE, line=.5)
abline(h=coef(men.tot)[1], col="red", lty=2)
summary(men.tot)
#gam.check(wst.men.abs)
#concurvity(wst.men.abs, full=TRUE)


#WOMEN ONLY WITH COVARIATES
women.tot <- gam(totchl~s(age, bs="cs", k=20), data=nhanes[nhanes$sex==2,],
                 method="REML")

plot(women.tot, shade=TRUE, residuals=FALSE,
     seWithMean=TRUE, select=1, shift=coef(women.tot)[1], xlab="", ylab="", rug=TRUE)
title(ylab="Total Cholesterol")
title(xlab="Age", line=2.2)
title(main="Women", outer=FALSE, line=.5)
abline(h=coef(women.tot)[1], col="red", lty=2)
summary(women.tot)


###MODEL time to CVD as a function of age and total cholesterol#####

b0 <- gam(lenyfl ~ s(age) + s(totchl) + smoker + black,
          weights=cvd_dth, family=cox.ph, data=nhanes[nhanes$sex==1,])
plot(b0)

###MODEL time to CVD as a function of total cholesterol#####

b0 <- gam(lenyfl ~ s(totchl) + s(age) + smoker + black,
          weights=cvd_dth, family=cox.ph, data=nhanes[nhanes$sex==1,])

b1 <- gam(lenyfl ~ s(totchl) +s(age) + smoker + black,
          weights=cvd_dth, family=cox.ph, data=nhanes[nhanes$sex==2,])

par(mfrow=c(2,1))

plot(b0, shade=TRUE, select=1, xlab="", seWithMean=TRUE)
title(main="Men", outer=FALSE, line=.5)
title(xlab="Total Cholesterol", line=2.2)
abline(h=0, col="red", lwd=2)


plot(b1, shade=TRUE, select=1, xlab="", seWithMean=TRUE)
title(main="Women", outer=FALSE, line=.5)
title(xlab="Total Cholesterol", line=2.2)
abline(h=0, col="red", lwd=2)

