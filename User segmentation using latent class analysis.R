#install new packages
#install.packages("poLCA")
#install.packages("mclust")
#install.packages("tidyverse")
#install.packages("devtools")
#install.packages("githubinstall")
#devtools::install_github("jrosen48/tidyLPA")

#load relevant packages
library(githubinstall)
library(devtools)
library(tidyverse)
library(foreign)
library(dplyr)
library(gmodels)
library(sas7bdat)
library(rmarkdown)
library(plotly)
library(psych)
library(mclust)
library(tidyLPA)

#set working directory
setwd("~/TAPS online caregiver support/Data/final")

#read in data
wide <- read.sas7bdat("widefile_7dec17.sas7bdat")

# DATA WRANGLING

# set all missing values to NA (including NaN)

wide[ is.na(wide) ] <- NA

# DATA CLEANING: clean up and derive some vars to prepare for LPA and other analyses

# MVCN vs. control (MVCN is ver == 2, control is ver == 1)

# wave 1

#recode MVCN condition at baseline to be differently named (mvcn1: mvcn = 1, control = 0)
wide$mvcn1 <- ifelse(wide$ver==2, 1, 0)

# wave 2 
# create binary indicator where 1 = in MVCN at wave 2, 0 = control at waves 1 and 2)

wide$mvcn2 <- ifelse(wide$ver3==2, 1,
                     ifelse(wide$ver3==1, 0, NA))

# wave 3
# create binary indicator where 1 = in MVCN at wave 3, 0 = control


wide$mvcn3 <- ifelse(wide$ver6==2, 1, 
                     ifelse(wide$ver6==1, 0, NA))

#wave_comp:
#0 = completed only baseline
#3 = completed baseline and 3-month, not 6-month
#6 = completed only baseline and 6-month, not 3-month
#36 = completed baseline, 3- and 6-month
table(wide$wave_comp)

#create variable to represent number of children under 18 in household
#0 = 0
#1 = 1
#2 = 2
#3 = 3
#4 = 4 or more
wide$ncinhh1[wide$Q60==1] <- 0
wide$ncinhh1[wide$Q60==2] <- 1
wide$ncinhh1[wide$Q60==3] <- 2
wide$ncinhh1[wide$Q60==4] <- 3
wide$ncinhh1[wide$Q60==5] <- 4

#children under 18 in household (Y/N)
wide$cinhh1[wide$ncinhh1>=1] <- 1
wide$cinhh1[wide$ncinhh1==0] <- 0

# total number who live in one's household at least 50% of the time (range: 1-10): recode "0" to be "1" (no one
#should have answered "0")
wide$nhh <- wide$Q59_1
wide$nhh[wide$Q59_1==0] <- 1

#recode hh income from original scale with ranges as response options to numeric using midpoint of ranges
wide$hhincome1[wide$Q61==0] <- NA
wide$hhincome1[wide$Q61==1] <- 12500
wide$hhincome1[wide$Q61==2] <- 30000
wide$hhincome1[wide$Q61==3] <- 42500
wide$hhincome1[wide$Q61==4] <- 62500
wide$hhincome1[wide$Q61==5] <- 87500
wide$hhincome1[wide$Q61==6] <- 125000
wide$hhincome1[wide$Q61==7] <- 175000

table(wide$hhincome1, wide$Q61)

# create per capita adjusted household income (divide hh income by number of people in household and then
#divide by 1,000 to scale this variable so that it's not so discrepant from the scales of other covariates)
wide$pcincome1 <- (wide$hhincome1/wide$nhh)/1000

describe(wide$pcincome1)
hist(wide$pcincome1)

#create variable with 4 levels of education
#1 = hs grad or lower
#2 = some college or associate's degree
#3 = bachelor's degree
#4 = graduate or professional degree
wide$cedu4[wide$cedu==1 | wide$cedu==2] <- 1
wide$cedu4[wide$cedu==3 | wide$cedu==4] <- 2
wide$cedu4[wide$cedu==5] <- 3
wide$cedu4[wide$cedu==6 | wide$cedu==7] <- 4
table(wide$cedu, wide$cedu4)

#CREATE DUMMY-CODED BINARY INDICATOR FOR HIGHEST LEVEL OF EDUCATION 
#1 = COMPLETED AT LEAST BACHELOR'S DEGREE
#0 = COMPLETED ASSOCIATE'S DEGREE OR LESS
wide$ceducoll <- ifelse(wide$cedu >= 5 & wide$cedu <= 7, 1,
                        ifelse(wide$cedu >= 1 & wide$cedu <= 4, 0, NA))

table(wide$cedu, wide$ceducoll)

#create dummy variables for v911stat to include in regression models, where ref category is post-9/11 only
wide$vpre911[wide$v911stat==2] <- 1
wide$vpre911[wide$v911stat==1 | wide$v911stat==3] <- 0

wide$vboth911[wide$v911stat==3] <- 1
wide$vboth911[wide$v911stat==1 | wide$v911stat==2] <- 0

#care recipients' previously-diagnosed medical conditions
#create variable to represent total number of physical conditions

wide$vdxphytw1 <- rowSums(wide[, c("q8_1", "q8_4", "q8_5", "q8_6", "q8_7", "q8_8", "q8_9", 
                                   "q8_12", "q8_13", "q8_14", "q8_15", "q8_16", "q8_17", 
                                   "q8_18")], na.rm=TRUE)

#create binary variable to represent whether had at least one physical condition

wide$vdxphybw1 <- NA
wide$vdxphybw1[wide$vdxphytw1 >= 1] <- 1
wide$vdxphybw1[wide$vdxphytw1 == 0] <- 0

#PSYCHOLOGICAL CONDITIONS: 
#PTSD (q8_2)
#Depression (q8_3)
#Substance use disorder (q8_10)

#create variable to represent total number of psychological conditions

wide$vdxpsytw1 <- rowSums(wide[, c("q8_2", "q8_3", "q8_10")], na.rm=TRUE)

#create binary variable to represent whether had at least one psych condition

wide$vdxpsybw1 <- NA
wide$vdxpsybw1[wide$vdxpsytw1 >= 1] <- 1
wide$vdxpsybw1[wide$vdxpsytw1 == 0] <- 0

#NEUROLOGICAL CONDITIONS:
#TBI (q8_11)
#Parkinson's disease (q8_19)
#dementia (q8_20)

#create variables to represent total number of neurological conditions

wide$vdxneurotw1 <- rowSums(wide[, c("q8_11", "q8_19", "q8_20")], na.rm=TRUE)

#create binary variable to represent whether had neurological condition

wide$vdxneurobw1 <- NA
wide$vdxneurobw1[wide$vdxneurotw1 >= 1] <- 1
wide$vdxneurobw1[wide$vdxneurotw1 == 0] <- 0

#create variable to represent total number of medical conditions
wide$vdxsumw1 <- rowSums(wide[, c("vdxphytw1", "vdxpsytw1", "vdxneurotw1")], na.rm=TRUE)

#create binary variable to indicate six or more medical conditions (1=yes, 0=no)
wide$vdxsum6w1[wide$vdxsumw1 >= 6] <- 1
wide$vdxsum6w1[wide$vdxsumw1 < 6] <- 0

# total number of people in social network (range: 1.5-148.5)

#impute missing value on wide$q21r_3 (assume no people in that category for case with missing value)
wide$q21r_3[ is.na(wide$q21r_3) ] <- 0

#create new sum for socntot1 with imputed q21r_3 so that everyone will have a nonmissing value

wide$socntot1 <- apply(wide[,c('q21r_1','q21r_2','q21r_3','q21r_4','q21r_5','q21r_6','q21r_7')], 
                       1, function(x) sum(x))

#time spent caregiving per week
#convert ordinal variable cgtime1 to a numeric variable
wide$cgtime1num <- NA
wide$cgtime1num[wide$cgtime1==1] <- .5
wide$cgtime1num[wide$cgtime1==2] <- 2.5
wide$cgtime1num[wide$cgtime1==3] <- 6.5
wide$cgtime1num[wide$cgtime1==4] <- 14.5
wide$cgtime1num[wide$cgtime1==5] <- 25.5
wide$cgtime1num[wide$cgtime1==6] <- 35.5
wide$cgtime1num[wide$cgtime1==7] <- 50.5
wide$cgtime1num[wide$cgtime1==8] <- 70.5
wide$cgtime1num[wide$cgtime1==9] <- 90.5

hist(wide$cgtime1num)
table(wide$cgtime1num) 

#reverse score anxiety at waves 1, 2, and 3

#wave1

wide$anxiety_r1 <- 100 - wide$anxiety1
describe(wide$anxiety_r1)
describe(wide$anxiety1)

# wave 2 (3-month)

wide$m3anxiety_r1 <- 100 - wide$m3anxiety1

cor(wide$m3anxiety_r1, wide$m3anxiety1, use = "complete.obs") # corr is -1, indicating that reverse-scored var was correctly created

# wave 3 (6-month)

wide$m6anxiety_r1 <- 100 - wide$m6anxiety1

cor(wide$m6anxiety_r1, wide$m6anxiety1, use = "complete.obs") # corr is -1


cor(wide$anxiety_r1, wide$anxiety1, use = "complete.obs")
cor(wide$anxiety_r1, wide$depsev1, use = "complete.obs")

# create standardized versions of variables in LPA to facilitate interpretation of profiles

wide$cgunpz <- as.vector(scale(wide$cgunp1))
wide$tansuppz <- as.vector(scale(wide$tansupp1))
wide$lonez <- as.vector(scale(wide$lone1))
wide$mcgidenz <- as.vector(scale(wide$mcgiden1))
wide$relqualz <- as.vector(scale(wide$relqual1))
wide$emosuppz <- as.vector(scale(wide$emosupp1))
wide$affsuppz <- as.vector(scale(wide$affsupp1))
wide$possuppz <- as.vector(scale(wide$possupp1))
wide$socntotz <- as.vector(scale(wide$socntot1))
wide$nhhz <- as.vector(scale(wide$nhh))
wide$clfrtotz <- as.vector(scale(wide$clfrtot1))



# COMPUTE UNIVARIATE DESCRIPTIVE STATISTICS FOR VARIABLES TO BE INCLUDED IN LPA
# INSTRUMENTAL/TANGIBLE SUPPORT AVAILABLE

# number of other unpaid caregivers who help with care recipient (range: 0-3)
describe(wide$cgunp1)

# tangible support (range: 0-100)
describe(wide$tansupp1)

# EMOTIONAL/COMPANIONATE SOCIAL SUPPORT AND LONELINESS

# loneliness (range: 3-12)
describe(wide$lone1)

# belongingness/military caregiver identity (range: 1-5)
describe(wide$mcgiden1)

# quality of relationship between caregiver and care recipient (range: 1.14-5)
describe(wide$relqual1)

# emotional/informational social support (range: 0-100)
describe(wide$emosupp1)

# affectionate support (range: 0-100)
describe(wide$affsupp1)

# positive social interaction support (range: 0-100)
describe(wide$possupp1)

# SOCIAL NETWORK SIZE/SOCIAL ISOLATION
# total number of people in social network
describe(wide$socntot1)

# total number of close friends (range: 1-6)
describe(wide$clfrtot1)

# compute correlation between total number of people in social network and total # close friends
# to see if they're too highly correlated for both to be included in LPA

cor(wide$socntot1, wide$clfrtot1, use = "complete.obs") # corr = .60--not too highly correlated to 
#include both in LPA

# total number who live in one's household at least 50% of the time (range: 1-10)
describe(wide$nhh)

# EXAMINE HISTOGRAMS OF VARIABLES TO BE INCLUDED IN LPA
hist(wide$cgunp1) # distribution looks funny (right-tailed skew--not terribly surprising for a count)
hist(wide$tansupp1)
hist(wide$lone1)
hist(wide$mcgiden1)
hist(wide$relqual1)
hist(wide$emosupp1)
hist(wide$affsupp1)
hist(wide$possupp1)
hist(wide$socntot1)
hist(wide$nhh)
hist(wide$clfrtot1)

# MCLUST CODE

#create subset of data that includes only variables that go into LPA in mclust

lcaset <- subset(wide, select=c(cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                                affsuppz, possuppz, socntotz, nhhz, clfrtotz))

# mclustBIC: Return an object of class 'mclustBIC' containing the Bayesian Information Criterion for the specified
#mixture models numbers of clusters. Auxiliary information returned as attributes.
#The corresponding print method shows the matrix of values and the top 3 models (in terms of parameterization and
# number of components) according to the #BIC criterion.

#output object lcasetbic with default settings (considers all model parameterizations and numbers of components/classes)

lcasetbic <- mclustBIC(lcaset)

print(lcasetbic)

#mclustModel yields the best model based on BIC for a given set of model parameterizations and numbers of components

cl <- mclustModel(lcaset, lcasetbic, G = 2:6)

cl

#output BIC object for BICs of models with between 2 and 6 classes with EEI parameterization (varying means, equal variances, covariances set to 0--conventional specs)

lcaeei <- mclustBIC(lcaset, G = 2:6, modelNames = 'EEI')
print(lcaeei)

#examine parameters for EEI model with 4 classes

lcaeei4 <- mclustModel(lcaset, lcaeei, G=4)

lcaeei4

#examine parameters for EEI model with 5 classes

lcaeei5 <- mclustModel(lcaset, lcaeei, G=5)

lcaeei5

#create dataframe from object that contains probability of belonging to each of 4 classes in EEI model

eei4prob <- as.data.frame(lcaeei4$z)

str(eei4prob)

#create variable to indicate class membership with highest probability 
eei4prob$max <- pmax(eei4prob$V1, eei4prob$V2, eei4prob$V3, eei4prob$V4)

View(eei4prob)
describe(eei4prob$max)

#create indicator to represent class membership based on class with highest probability (with values 1, 2, 3, or 4)
eei4prob$classmem[eei4prob$V1==eei4prob$max] <- 1
eei4prob$classmem[eei4prob$V2==eei4prob$max] <- 2
eei4prob$classmem[eei4prob$V3==eei4prob$max] <- 3
eei4prob$classmem[eei4prob$V4==eei4prob$max] <- 4

#compute class ns 
table(eei4prob$classmem)

#compute class proportions
prop.table(table(eei4prob$classmem))


#bind eei4prob dataframe with class membership probabilities to the original dataframe wide
#to prepare to examine associations between most likely class membership and other variables in wide dataframe

wideprobs <- cbind(wide, eei4prob)

#examine characteristics and contents of wideprobs df
str(wideprobs)
class(wideprobs)
View(wideprobs)

wideprobs2 <- subset(wideprobs, select=c(cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                                         affsuppz, possuppz, socntotz, nhhz, clfrtotz, classmem))

View(wideprobs2)

wideprobs2 %>% group_by(classmem) %>% summarise(mean(cgunpz))

wideprobs2 %>% group_by(classmem) %>% summarise(mean(tansuppz))

wideprobs2 %>% group_by(classmem) %>% summarise(mean(relqualz))

wideprobs2 %>% group_by(classmem) %>% summarise(mean(possuppz))

lcaeei4$parameters$mean

describe(wideprobs$V1)
describe(wideprobs$V2)
describe(wideprobs$V3)
describe(wideprobs$V4)

wideprobs %>% group_by(classmem) %>% summarise(mean(V1))
wideprobs %>% group_by(classmem) %>% summarise(mean(V2))
wideprobs %>% group_by(classmem) %>% summarise(mean(V3))
wideprobs %>% group_by(classmem) %>% summarise(mean(V4))

# see if there are any cases that had equivalent probabilities across classes
wideprobs$equal <- 0
wideprobs$equal[wideprobs$V1==wideprobs$V2 | wideprobs$V1==wideprobs$V3 | wideprobs$V1==wideprobs$V4 |
        wideprobs$V2==wideprobs$V3 | wideprobs$V2==wideprobs$V4 | wideprobs$V3==wideprobs$V4] <- 1

table(wideprobs$equal)


# TIDYLPA CODE

#examine fit indices for solutions with different numbers of profiles (range: 2-5); 
#specify all models as "EEI" (model 1 in tidyLPA syntax), which
#indicates varying means with equal variances and covariances fixed to 0 (commonly-used model because
#it doesn't require the model to estimate as many parameters andd thus affords a more parsimonious,
#stable solution.)

#EEI (MODEL 1): VARYING MEANS, EQUAL VARIANCES, COVARIANCES FIXED TO 0 (MPLUS DEFAULT MODEL)

#2 profiles

create_profiles_lpa(wide, cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                            affsuppz, possuppz, socntotz, nhhz, clfrtotz, n_profiles = 2, model = 1) %>%
  plot_profiles_lpa


#3 profiles

create_profiles_lpa(wide, cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                    affsuppz, possuppz, socntotz, nhhz, clfrtotz, n_profiles = 3, model = 1) %>%
  plot_profiles_lpa

#4 profiles

create_profiles_lpa(wide, cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                    affsuppz, possuppz, socntotz, nhhz, clfrtotz, n_profiles = 4, model = 1) %>%
  plot_profiles_lpa


#5 profiles

create_profiles_lpa(wide, cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                    affsuppz, possuppz, socntotz, nhhz, clfrtotz, n_profiles = 5, model = 1) %>%
  plot_profiles_lpa

#6 profiles

create_profiles_lpa(wide, cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                    affsuppz, possuppz, socntotz, nhhz, clfrtotz, n_profiles = 6, model = 1) %>%
  plot_profiles_lpa

# CONDUCT BLRT USING MCLUST

# general syntax: mclustBootstrapLRT(data, modelName = NULL, nboot = 999, level = 0.05, maxG = NULL,
#verbose = interactive(), ...)

#by default, by leaving setting of maxG argument to NULL, it should stop when a test is not significant at the 
#specified level; also by default, 999 is the number of bootstrap replications

y <- mclustBootstrapLRT(lcaset, modelName = 'EEI', level = 0.05, maxG = 5)

print(y)

# FINAL 4-CLASS SOLUTION: DATA PREP FOR REGRESSION MODELS

#output tibble that is similar to df and includes original df variables

widem4 <- create_profiles_lpa(wide, cgunpz, tansuppz, lonez, mcgidenz, relqualz, emosuppz,
                              affsuppz, possuppz, socntotz, nhhz, clfrtotz, n_profiles = 4, model = 1, 
                              return_orig_df = TRUE)

widem4 <- as.data.frame(widem4)

View(widem4) #note that profiles are contained in a variable called "profile" and probabilities of final class
#based on class with maximum posterior probability in "posterior_prob"

table(widem4$profile)

#compute average (mean) posterior probability for each profile--note that they range from .87-.97, which is good
widem4 %>% group_by(profile) %>% summarise(mean(posterior_prob))

#create binary indicators for most likely class membership based on highest posterior probability
widem4$high <- 0
widem4$high[widem4$profile==1] <- 1

widem4$low <- 0
widem4$low[widem4$profile==2] <- 1

widem4$limited <- 0
widem4$limited[widem4$profile==3] <- 1

widem4$abovavg <- 0
widem4$abovavg[widem4$profile==4] <- 1

#confirm that binary indicators have the correct frequency distributions
table(widem4$high)
table(widem4$low)
table(widem4$limited)
table(widem4$abovavg)

#create profile variable from integer to factor to compute means for each profile and do anova
widem4$profactor <- as.factor(widem4$profile)

# write out permanent dataset for ease of accessing in future

widem4 <- write.csv(widem4, 'widem4.csv')


 




