#install relevant packages
install.packages("MASS")
install.packages("HH")
install.packages("pscl")
install.packages("boot")

#load relevant packages
library(tidyverse)
library(dplyr)
library(gmodels)
library(plotly)
library(psych)
library(MASS)
library(HH)
library(car)
library(ggplot2)
library(pscl)
library(boot)


#set working directory
setwd("~/TAPS online caregiver support/Data/final")

# read in data file
widem4 <- read.csv("widem4.csv")

# for variables with outliers, create truncated versions

# years as a caregiver
# note that there are a few cases above 13 that look like outliers--recode all to 14
widem4$cgdur1trunc <- widem4$cgdur1
widem4$cgdur1trunc[widem4$cgdur1 >= 14] <- 14

table(widem4$cgdur1trunc, widem4$cgdur1)
table(widem4$cgdur1trunc)
describe(widem4$cgdur1)
hist(widem4$cgdur1)
describe(widem4$cgdur1trunc)
hist(widem4$cgdur1trunc)

# per capita household income

# COMPUTE UNIVARIATE DESCRIPTIVE STATS ON SOCIODEMOGRAPHIC CHARACTERISTICS (Table 1)

#gender
CrossTable(widem4$cmale, prop.c = TRUE)

#race/ethnicity
#1 = non-Hispanic white
#2 = non-Hispanic black, other, and multirace
#3 = Hispanic
CrossTable(widem4$craceth3, prop.c = TRUE)

#age
describe(widem4$cagecont)

#married, living with partner, or has significant other--may not be able to include this as predictor of class membership
#because so few respondents are unmarried or without a partner (only 22)
CrossTable(widem4$cinrel1, prop.c = TRUE)

#children under 18 in household (Y/N)
CrossTable(widem4$cinhh1, prop.c = TRUE)

#highest level of education 
CrossTable(widem4$ceducoll, prop.c = TRUE)

# per capita annual household income
describe(widem4$pcincome1)

# resides in metropolitan area
CrossTable(widem4$metro1, prop.c = TRUE)

# CAREGIVING DETAILS

#caregiver served before or after 9/11
#era of military service (1 = post-9/11 only, 2 = pre-9/11 only, 3 = both post- and pre-9/11)
CrossTable(widem4$v911stat, prop.c = TRUE)

#relationship of care recipient to caregiver (q3 = 1 is spouse, 2 = partner or sig. other)
CrossTable(widem4$q3, prop.c = TRUE)

#hours spent caregiving per week
describe(widem4$cgtime1num)

#number of years since began caring for care recipient
describe(widem4$cgdur1)

#care recipient's history of diagnosed physical conditions (yes/no)
CrossTable(widem4$vdxphybw1, prop.c = TRUE)
#psychological conditions (yes/no)
CrossTable(widem4$vdxpsybw1, prop.c = TRUE)
#neurological conditions (yes/no)
CrossTable(widem4$vdxneurobw1, prop.c = TRUE)
#six or more medical conditions total
CrossTable(widem4$vdxsum6w1, prop.c = TRUE)

#total number of tasks with which caregiver helps care recipient (ADLs and IADLs)
describe(widem4$cgtsksm1)

#total frequency with which caregiver assists care recipient with various tasks
describe(widem4$cgtskfr1)

# BIVARIATE ASSOCIATIONS BETWEEN CLASS MEMBERSHIP AND SOCIODEMOGRAPHIC CHARACTERISTICS

# compute bivariate associations and run chi-square tests for most likely class membership and sociodemographic characteristics
#gender
CrossTable(widem4$cmale, widem4$profile, fisher=TRUE)

# significant differences between classes on gender: perform follow-up pairwise tests between classes
# create 6 subsets of data, one for each pair

# high vs. above average
hiaa <- filter(widem4, profile==1 | profile==4)

# high vs. limited
hilim <- filter(widem4, profile==1 | profile==3)

# high vs. low
hilow <- filter(widem4, profile==1 | profile==2)

# above average vs. limited
aalim <- filter(widem4, profile==4 | profile==3)

# above average vs. low
aalow <- filter(widem4, profile==4 | profile==2)

# limited vs. low
limlow <- filter(widem4, profile==3 | profile==2)

# conduct follow-up pairwise tests between classes by gender

# high vs. above average
CrossTable(hiaa$cmale, hiaa$profile, fisher=TRUE)

# high vs. limited
CrossTable(hilim$cmale, hilim$profile, fisher=TRUE)

# high vs. low
CrossTable(hilow$cmale, hilow$profile, fisher=TRUE)

# above average vs. limited
CrossTable(aalim$cmale, aalim$profile, fisher=TRUE)

# above average vs. low
CrossTable(aalow$cmale, aalow$profile, fisher=TRUE)

# limited vs. low
CrossTable(limlow$cmale, limlow$profile, fisher=TRUE)


#race/ethnicity
CrossTable(widem4$craceth3, widem4$profile, fisher=TRUE)

#age
describe(widem4$cagecont)
hist(widem4$cagecont)

# one-way ANOVA

#use by command to split dataframe by profile (as factor variable) and calculate mean age and SD for each profile

by(widem4, widem4$profactor, function(x){
  
  agemean <- describe(x$cagecont)
  
})

# conduct Bartlett's test of equality of variances to test this assumption of ANOVA
bartlett.test(cagecont ~ profactor, data=widem4) #null hypothesis of equality of variances not rejected (p = .84)

# run one-way ANOVA to determine whether there are significant differences between the 4 profiles on age
ageprof <- aov(cagecont ~ profactor, data=widem4)
summary(ageprof)
describe(ageprof$residuals) #residuals are normally distributed, so normality assumption of ANOVA appears to be satisfied

# conduct follow-up pairwise comparisons of profiles to determine which differ significantly from each other
# use Tukey's Honestly Significant Difference test with 95% family-wise confidence level
TukeyHSD(ageprof)

# married or living with partner
CrossTable(widem4$cinrel, widem4$profile, fisher = TRUE)

# children in household
CrossTable(widem4$cinhh1, widem4$profile, chisq = TRUE)

# college degree
CrossTable(widem4$ceducoll, widem4$profile, chisq = TRUE)

# per capita annual household income
# compute means for each profile
by(widem4, widem4$profactor, function(x){
  
  pcincmean <- describe(x$pcincome1)
  
})

# run one-way ANOVA
incprof <- aov(pcincome1 ~ profactor, data=widem4)
summary(incprof)
describe(incprof$residuals) #residuals are characterized by a high degree of kurtosis
hist(incprof$residuals)

# test equality of variance using Bartlett's test
bartlett.test(pcincome1 ~ profactor, data=widem4) # null hypothesis of equal variances rejected

# run non-parametric Kruskal-Wallis test in lieu of ANOVA
kruskal.test(pcincome1 ~ profactor, data=widem4) # sig. at p < .001, so run pairwise follow-up tests

#use pairwise.wilcox.test() to calculate pairwise comparisons between groups with corrections for multiple tests
pairwise.wilcox.test(widem4$pcincome1, widem4$profactor, p.adjust.method = "BH")

# resides in metropolitan area
CrossTable(widem4$metro1, widem4$profile, chisq=TRUE)

# VETERAN/CARE RECIPIENT CHARACTERISTICS BY CLASS MEMBERSHIP  

# veteran served before vs. after 9/11
#1 = post-9/11 only
#2 = pre-9/11 only
#3 = both pre- and post-9/11

# create binary indicator for post-9/11 only vs. pre-9/11 only or both pre- and post-9/11
widem4$vpost911 <- NA
widem4$vpost911[widem4$v911stat==1] <- 1
widem4$vpost911[widem4$v911stat==2 | widem4$v911stat==3] <- 0

CrossTable(widem4$vpost911, widem4$profile, chisq=TRUE)

# conduct follow-up pairwise tests

# high vs. above average
CrossTable(hiaa$v911stat, hiaa$profile, chisq=TRUE) # sig. diff at p < .05

# high vs. limited
CrossTable(hilim$v911stat, hilim$profile, chisq=TRUE) # not sig. diff

# high vs. low
CrossTable(hilow$v911stat, hilow$profile, chisq=TRUE) # not sig. diff

# above average vs. limited
CrossTable(aalim$v911stat, aalim$profile, chisq=TRUE) # sig. diff at p = .005

# above average vs. low
CrossTable(aalow$v911stat, aalow$profile, chisq=TRUE) # sig. diff at p = .002

# limited vs. low
CrossTable(limlow$v911stat, limlow$profile, chisq=TRUE) # not sig. diff

# v911stat:
  # 1 = post-9/11 only
  # 2 = pre-9/11 only
  # 3 = post- and pre-9/11

# COMPARE EACH PAIR OF VETERAN GROUPS WITHIN PROFILES WHERE DIFFS WERE SIG

# compare each pair of veteran groups within high vs. above average profiles

# extract subset of high and above average Ss whose care recipients served in
   # post-9/11 only and pre-9/11 only:
   hiaapopronly <- filter(hiaa, v911stat==1 | v911stat==2)

   # post-9/11 only and both post- and pre-9/11:
   hiaapoboth <- filter(hiaa, v911stat==1 | v911stat==3)

   # pre-9/11 only and both post- and pre-9/11:
   hiaapreboth <- filter(hiaa, v911stat==2 | v911stat==3)
   
# extract subset of above average and limited Ss whose care recipients served in
   # post-9/11 only and pre-9/11 only:
   aalimpopronly <- filter(aalim, v911stat==1 | v911stat==2)

   # post-9/11 only and both post- and pre-9/11:
   aalimpoboth <- filter(aalim, v911stat==1 | v911stat==3)

   # pre-9/11 only and both post- and pre-9/11:
   aalimpreboth <- filter(aalim, v911stat==2 | v911stat==3)
   
# extract subset of above average and low Ss whose care recipients served in
   # post-9/11 only and pre-9/11 only:
   aalowpopronly <- filter(aalow, v911stat==1 | v911stat==2)
   
   # post-9/11 only and both post- and pre-9/11:
   aalowpoboth <- filter(aalow, v911stat==1 | v911stat==3)
   
   # pre-9/11 only and both post- and pre-9/11:
   aalowpreboth <- filter(aalow, v911stat==2 | v911stat==3)
   
# HIGH VS. ABOVE AVERAGE

# compare post- and pre-9/11 only, high and above average
CrossTable(hiaapopronly$v911stat, hiaapopronly$profile, chisq=TRUE) # no sig diffs

# compare post-9/11 only and both post- and pre-9/11, high and above average
CrossTable(hiaapoboth$v911stat, hiaapoboth$profile, chisq=TRUE) # no sig diffs

# compare pre-9/11 only and both post- and pre-9/11, high and above average
CrossTable(hiaapreboth$v911stat, hiaapreboth$profile, chisq=TRUE) # sig diffs at p = .02

# ABOVE AVERAGE VS. LIMITED

# compare post- and pre-9/11 only
CrossTable(aalimpopronly$v911stat, aalimpopronly$profile, chisq=TRUE) #sig diffs at p = .002

# compare post-9/11 only with both post- and pre-9/11
CrossTable(aalimpoboth$v911stat, aalimpoboth$profile, chisq=TRUE) # not sig diff

# compare pre-9/11 only with both post- and pre-9/11
CrossTable(aalimpreboth$v911stat, aalimpreboth$profile, chisq=TRUE) # sig diff at p = .008

# ABOVE AVERAGE VS. LOW

# compare post- and pre-9/11 only
CrossTable(aalowpopronly$v911stat, aalowpopronly$profile, chisq=TRUE) # sig diff at p < .001

# compare post-9/11 only with both post- and pre-9/11
CrossTable(aalowpoboth$v911stat, aalowpoboth$profile, chisq=TRUE) # not sig diff

# compare pre-9/11 only with both post- and pre-9/11
CrossTable(aalowpreboth$v911stat, aalowpreboth$profile, chisq=TRUE) # sig diff at p = .004


# MEDICAL CONDITIONS

# veteran diagnosed with at least one physical condition
CrossTable(widem4$vdxphybw1, widem4$profile, fisher=TRUE) # conduct follow-up tests

# high vs. above average
CrossTable(hiaa$vdxphybw1, hiaa$profile, fisher=TRUE) # no sig diffs

# high vs. limited
CrossTable(hilim$vdxphybw1, hilim$profile, fisher=TRUE) # no sig diffs

# high vs. low
CrossTable(hilow$vdxphybw1, hilow$profile, fisher=TRUE) # no sig diffs

# above average vs. limited
CrossTable(aalim$vdxphybw1, aalim$profile, fisher=TRUE) # sig. at p = .007

# above average vs. low
CrossTable(aalow$vdxphybw1, aalow$profile, fisher=TRUE) # sig at p = .005

# limited vs. low
CrossTable(limlow$vdxphybw1, limlow$profile, fisher=TRUE) # no sig diffs


# veteran diagnosed with at least one psychological condition
CrossTable(widem4$vdxpsybw1, widem4$profile, fisher=TRUE) # p < .001

# conduct follow-up tests

# high vs. above average
CrossTable(hiaa$vdxpsybw1, hiaa$profile, fisher=TRUE) # p = .01

# high vs. limited
CrossTable(hilim$vdxpsybw1, hilim$profile, fisher=TRUE) # no sig diffs

# high vs. low
CrossTable(hilow$vdxpsybw1, hilow$profile, fisher=TRUE) # no sig diffs

# above average vs. limited
CrossTable(aalim$vdxpsybw1, aalim$profile, chisq=TRUE) # p = .03

# above average vs. low
CrossTable(aalow$vdxpsybw1, aalow$profile, chisq=TRUE) # p < .001

# limited vs. low
CrossTable(limlow$vdxpsybw1, limlow$profile, chisq=TRUE) # p = .02


# veteran diagnosed with at least one neurological condition
CrossTable(widem4$vdxneurobw1, widem4$profile, chisq=TRUE)

# conduct follow-up tests

# high vs. above average
CrossTable(hiaa$vdxneurobw1, hiaa$profile, chisq=TRUE) # no sig diffs

# high vs. limited
CrossTable(hilim$vdxneurobw1, hilim$profile, chisq=TRUE) # no sig diffs

# high vs. low
CrossTable(hilow$vdxneurobw1, hilow$profile, chisq=TRUE) # no sig diffs

# above average vs. limited
CrossTable(aalim$vdxneurobw1, aalim$profile, chisq=TRUE) # p = .02

# above average vs. low
CrossTable(aalow$vdxneurobw1, aalow$profile, chisq=TRUE) # p = .003

# limited vs. low
CrossTable(limlow$vdxneurobw1, limlow$profile, chisq=TRUE) # no sig diffs

#compute mean hours spent caregiving per week by profile

by(widem4, widem4$profactor, function(x){
  
  cgtimemean <- describe(x$cgtime1num)
  
})

# test equality of variance using Bartlett's test
bartlett.test(cgtime1num ~ profactor, data=widem4) #null hypothesis of equal variances not rejected (p = .31), so okay to do ANOVA

#run one-way ANOVA to determine whether there are significant differences between the 4 profiles on hours spent caregiving
cgtimeprof <- aov(cgtime1num ~ profactor, data=widem4)
summary(cgtimeprof)
#assess normality of residuals
describe(cgtimeprof$residuals) #residuals approximate normal distribution

TukeyHSD(cgtimeprof)

#years spent caregiving for veteran

hist(widem4$cgdur1)
describe(widem4$cgdur1)

by(widem4, widem4$profactor, function(x){
  
  cgdurmean <- describe(x$cgdur1)
  
})

cgdurprof <- aov(cgdur1 ~ profactor, data=widem4)
summary(cgdurprof)
describe(cgdurprof$residuals) #distribution of residuals is very kurtotic (and so normality assumption of ANOVA is violated)
hist(cgdurprof$residuals)
by(widem4, widem4$profactor, function(x){
  
  cgdurmeantrunc <- describe(x$cgdur1trunc)
  
})

cgdurproftrunc <- aov(cgdur1trunc ~ profactor, data=widem4)
summary(cgdurproftrunc)
describe(cgdurproftrunc$residuals)
hist(cgdurproftrunc$residuals)


# run Kruskal Wallis H test instead of ANOVA to test for sig diffs between profiles (non-parametric analog to one-way ANOVA)
kruskal.test(cgdur1 ~ profactor, data=widem4)

#use pairwise.wilcox.test() to calculate pairwise comparisons between groups with corrections for multiple tests
pairwise.wilcox.test(widem4$cgdur1, widem4$profactor, p.adjust.method = "BH")

# Total number of tasks with which caregiver assists care recipient (ADLs and IADLS)
hist(widem4$cgtsksm1)
describe(widem4$cgtsksm1)

by(widem4, widem4$profactor, function(x){
  
  cgtsksmmean <- describe(x$cgtsksm1)
  
})

# test equality of variances using Bartlett's test
bartlett.test(cgtsksm1 ~ profactor, data=widem4) # p = .05, not sig.

#run one-way ANOVA to determine whether there are significant differences between the 4 profiles on hours spent caregiving
cgtsksmprof <- aov(cgtsksm1 ~ profactor, data=widem4)
summary(cgtsksmprof) # not sig.--no need to run pairwise tests
describe(cgtsksmprof$residuals) # residuals approximate normal distribution, indicating acceptability of ANOVA

# Total frequency with which caregiver assists care recipient with different tasks
hist(widem4$cgtskfr1)
describe(widem4$cgtskfr1) 

by(widem4, widem4$profactor, function(x){
  
  cgtskfrmean <- describe(x$cgtskfr1)
  
})

# test equality of variances using bartlett test (parametric test)

bartlett.test(cgtskfr1 ~ profactor, data=widem4) # bartlett test is significant, indicating unequal variances and need
#for non-parametric test in lieu of ANOVA

# Kruskal Wallis Test of sig diffs between profiles on task frequency
kruskal.test(cgtskfr1 ~ profactor, data=widem4) # sig. diff at p = .002, so do pairwise test

#use pairwise.wilcox.test() to calculate pairwise comparisons between groups with corrections for multiple tests
pairwise.wilcox.test(widem4$cgtskfr1, widem4$profactor, p.adjust.method = "BH")

# regression models predicting social behavior and psychological health from class membership with 
# adjustment for sociodemographic characteristics
# use "high" class as reference category (most positive social support class)

# create object to abbreviate representation of predictors in wave 1 multivariate adjusted regression models

preds <- "abovavg + limited + low + 
                cmale + cwhite + cagecont + cinrel1 + cinhh1 + ceducoll + pcincome1 + metro1 + mvcn1 +
                vpre911 + vboth911 + vdxpsybw1 + vdxneurobw1 + 
                cgtime1num + cgdur1 + cgtskfr1"

# FREQUENCY OF INTERACTING WITH OTHER MILITARY CAREGIVERS

# cross-sectional model at wave 1

# examine distribution of outcome
describe(widem4$mcgifrq1)
hist(widem4$mcgifrq1)

# estimate linear regression model with class membership dummy vars as predictors of military cg interaction freq
# while adjusting for sociodemographic characteristics

freqreg <- lm(as.formula(paste0("mcgifrq1 ~", preds)), data=widem4)
summary(freqreg)

#assess multicollinearity of predictors
vif(freqreg) # max vif is 2.35, so multicollinearity is not a problem

#examine distribution of residuals to assess normality
describe(freqreg$residuals)
hist(freqreg$residuals)

nrow(as.data.frame(freqreg$residuals)) # determine number of nonmissing obs included in model

# USE OF OTHER STRUCTURED SOCIAL SUPPORT GROUPS

# examine distribution of outcome variable

# number of structured social support groups other than MVCN to which participant belongs
describe(widem4$socsupg1)
table(widem4$socsupg1)
hist(widem4$socsupg1)

# examine unadjusted means on socsupg1 by profile

by(widem4, widem4$profactor, function(x){
  
  socsupgmean <- describe(x$socsupg1)
  
})

# 57.6% of participants have a score of 0 on number of structured social support groups--should consider a zero-inflated
# Poisson or negative binomial model
# in a Poisson distribution, the mean == the variance; in a negative binomial distribution, the variance is greater than the mean (i.e., overdispersion)
# M = 1.28, variance = 3.13

zinbssg1 <- zeroinfl(as.formula(paste0("socsupg1 ~", preds)), data=widem4, dist="negbin", EM=TRUE)
summary(zinbssg1)
nrow(as.data.frame(zinbssg1$residuals)) # determine number of nonmissing obs in model

# convert output from count/negative binomial portion to incident rate ratios by exponentiating unstandardized coefficients

countcoeffs <- round(exp(zinbssg1$coefficients$count),2)

print(countcoeffs)

# convert output from logistic or zero portion of model to odds ratios by exponentiating unstandardized coefficients

zerocoeffs <- round(exp(zinbssg1$coefficients$zero),2)

print(zerocoeffs)

# frequency with which caregiver participated in other social support groups
describe(widem4$sgrpfrq1) # looks like a pile-up at 1 (should probably be recoded to zero)
hist(widem4$sgrpfrq1)

# recode sgrpfrq1 from scale of 1 to 7 to 0 to 6: pile-up at zero (56.5% of Ss have value of 0)
widem4$sgrpfrq1r <- widem4$sgrpfrq1 - 1

table(widem4$sgrpfrq1, widem4$sgrpfrq1r)
describe(widem4$sgrpfrq1r) # mean = 1.79, variance = 5.2441, so use zinb model (overdispersion where variance > mean)

zinbsgrpfrq1 <- zeroinfl(as.formula(paste0("sgrpfrq1r ~", preds)), data=widem4, dist="negbin", EM=TRUE)
summary(zinbsgrpfrq1)

nrow(as.data.frame(zinbsgrpfrq1$residuals)) # determine number of nonmissing obs in model

# convert output from count/negative binomial portion to incident rate ratios by exponentiating unstandardized coefficients

zin2countcoeffs <- round(exp(zinbsgrpfrq1$coefficients$count),2)

print(zin2countcoeffs)

# convert output from logistic or zero portion of model to odds ratios by exponentiating unstandardized coefficients

zin2zerocoeffs <- round(exp(zinbsgrpfrq1$coefficients$zero),2)

print(zin2zerocoeffs)


# DEPRESSIVE SYMPTOM SEVERITY
describe(widem4$depsev1)
hist(widem4$depsev1)
table(widem4$depsev1)

depreg1 <- lm(as.formula(paste0("depsev1 ~", preds)), data=widem4) 
summary(depreg1)
hist(depreg1$residuals)
describe(depreg1$residuals) # residuals are normally distributed, indicating tenability of linear reg model
vif(depreg1) # highest VIF is 2.35, indicating no problems with multicollinearity

nrow(as.data.frame(depreg1$residuals)) # determine number of nonmissing obs in model

# ANXIETY
describe(widem4$anxiety_r1)
hist(widem4$anxiety_r1)

anxreg1 <- lm(as.formula(paste0("anxiety_r1 ~", preds)), data=widem4)
summary(anxreg1)

#examine normality of residuals
hist(anxreg1$residuals)
describe(anxreg1$residuals) #residuals are normally distributed

nrow(as.data.frame(anxreg1$residuals)) # determine number of nonmissing obs in model

# HOPEFULNESS
hist(widem4$hope1)
describe(widem4$hope1)

hopereg1 <- lm(as.formula(paste0("hope1 ~", preds)), data=widem4)
summary(hopereg1)

# examine normality of residuals
hist(hopereg1$residuals)
describe(hopereg1$residuals) # residuals are normally distributed

nrow(as.data.frame(hopereg1$residuals)) # determine number of nonmissing obs in model


# MODELS PREDICTING CHANGE IN PSYCHOLOGICAL HEALTH FROM WAVE 1 TO WAVE 2 AND WAVE 1 TO WAVE 3

# create object to represent class membership as predictors of outcomes in unadjusted regression models

classmem <- "abovavg + limited + low"

# DEPRESSIVE SYMPTOM SEVERITY

# change from wave 1 to wave 2

cor(widem4$depsev1, widem4$m3depsev1, use = "complete.obs", method="pearson") #r = .70

# latent class membership predicting change in depressive symptoms from wave 1 to wave 2 (unadjusted model)

depreg3unadj <- lm(as.formula(paste0("m3depsev1 ~ depsev1 +", classmem)), data=widem4)
summary(depreg3unadj)

# latent class membership predicting change in depressive symptoms from wave 1 to wave 2 (adjusted model)

depreg3mo <- lm(as.formula(paste0("m3depsev1 ~ depsev1 +", preds)), data=widem4)
summary(depreg3mo)

describe(depreg3mo$residuals)
hist(depreg3mo$residuals)

vif(depreg3mo) # max VIF is 2.5

# change from wave 1 to wave 3

cor(widem4$depsev1, widem4$m6depsev1, use = "complete.obs", method="pearson") #r = .57

# latent class membership predicting change in depressive symptoms from wave 1 to wave 3 (unadjusted model)

depreg6unadj <- lm(as.formula(paste0("m6depsev1 ~ depsev1 +", classmem)), data=widem4)
summary(depreg6unadj)

# latent class membership predicting change in depressive symptoms from wave 1 to wave 3 (adjusted model)

depreg6mo <- lm(as.formula(paste0("m6depsev1 ~ depsev1 +", preds)), data=widem4)
summary(depreg6mo)

describe(depreg6mo$residuals)
hist(depreg6mo$residuals)

vif(depreg6mo) # max VIF is 2.5

# ANXIETY

# change from wave 1 to wave 2

# unadjusted model

anxreg3unadj <- lm(as.formula(paste0("m3anxiety_r1 ~ anxiety_r1 +", classmem)), data=widem4)
summary(anxreg3unadj)

# adjusted model

anxreg3mo <- lm(as.formula(paste0("m3anxiety_r1 ~ anxiety_r1 +", preds)), data=widem4)
summary(anxreg3mo)

describe(anxreg3mo$residuals)
hist(anxreg3mo$residuals)

vif(anxreg3mo) # max VIF is 2.3, so multicollinearity is not a problem

# change from wave 1 to wave 3

# unadjusted model

anxreg6unadj <- lm(as.formula(paste0("m6anxiety_r1 ~ anxiety_r1 +", classmem)), data=widem4)
summary(anxreg6unadj)

# adjusted model

anxreg6mo <- lm(as.formula(paste0("m6anxiety_r1 ~ anxiety_r1 +", preds)), data=widem4)
summary(anxreg6mo)

describe(anxreg6mo$residuals)
hist(anxreg6mo$residuals)

vif(anxreg6mo) # max VIF is 2.29

# summary of findings with anxiety: class membership did not significantly predict change from wave 1 to 
# wave 2 or 3 in unadjusted or adjusted models

# HOPEFULNESS

# change from wave 1 to wave 2

# unadjusted model

hope3mounadj <- lm(as.formula(paste0("m3hope1 ~ hope1 +", classmem)), data=widem4)
summary(hope3mounadj)

# adjusted model

hope3mo <- lm(as.formula(paste0("m3hope1 ~ hope1 +", preds)), data=widem4)
summary(hope3mo)

describe(hope3mo$residuals)
hist(hope3mo$residuals)

vif(hope3mo) # max VIF is 2.85

# change from wave 1 to wave 3

# unadjusted model

hope6mounadj <- lm(as.formula(paste0("m6hope1 ~ hope1 +", classmem)), data=widem4)
summary(hope6mounadj)

# adjusted model

hope6mo <- lm(as.formula(paste0("m6hope1 ~ hope1 +", preds)), data=widem4)
summary(hope6mo)

describe(hope6mo$residuals)
hist(hope6mo$residuals)

vif(hope6mo) # max VIF is 2.89

# graph observed means of each class on dimensions that went into LPA in their standardized form

# create and output a group means data set for graphing in Excel

classmeans <- widem4 %>%
                     group_by(profactor) %>%
                     summarise(
                       cgunpzmn = mean(cgunpz),
                       tansuppzmn = mean(tansuppz),
                       lonezmn = mean(lonez),
                       mcgidenzmn = mean(mcgidenz),
                       relqualzmn = mean(relqualz),
                       emosuppzmn = mean(emosuppz),
                       affsuppzmn = mean(affsuppz),
                       possuppzmn = mean(possuppz),
                       socntotzmn = mean(socntotz),
                       nhhzmn = mean(nhhz),
                       clfrtotzmn = mean(clfrtotz)
                     )

print(classmeans)

write.csv(as.data.frame(classmeans), file="factormeans.csv")






















