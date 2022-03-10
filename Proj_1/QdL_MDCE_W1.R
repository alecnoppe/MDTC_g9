library(mice)
library(dplyr)
library(ggplot2)
library(naniar)

dfcom <- readRDS("complete_data.rds")
dfinc <- readRDS("incomplete_data_g9.rds")
dfimp_listwise <- dfimp

#Scientifically interesting model:
dfcom$hrtdif <- dfcom$active - dfcom$rest
dfimp_listwise$heartdif <- dfimp_listwise$active - dfimp_listwise$rest
scintcom <- lm(hrtdif ~ age + smoke + bmi, data = dfcom)
scintimp_listwise <- lm(heartdif ~ age + smoke + bmi, data = dfimp_listwise, na.action = )
summary(scintcom)
summary(scintimp_listwise)
#Scientifically interesting questions:
#Is there a difference between the difference between active and resting heart rates for smokers and non-smokers, controlling for bmi?

summary(dfcom)
summary(dfinc)
#Distributions:
#Active looks maybe MCAR; all summary statistics very similar
#Height looks maybe MCAR; all summary statistics very similar
#Weight looks maybe MCAR: all summary statistics very similar
#BMI looks maybe MCAR: all summary statistics very similar
#Smoke? IDK

vis_miss(dfinc)
#Age, sex, intensity and rest BMI are not missing
md.pattern(dfinc)

#Missingness tests
aov_age <- aov(age ~ is.na(smoke) + is.na(bmi) + is.na(height) + is.na(active) + is.na(weight), data = dfinc)
summary(aov_age)

mcar_test(dfinc)
#Little's test shows MCAR data - but is the sample size too small?
#Look into MAR/OAR stuff?


#Data that can be CALCULATED:
#Height and BMI present, but not weight: 22+5=27 cases

#Formula: BMI = weight (kg) / height (m)^2
#weight = 
testdf <- dfinc
testdf$weight2 <- testdf$weight
testdf$weight2 <- testdf$bmi * (testdf$height / 100)^2
testdf$height2 <- testdf$height
testdf$height2 <- 100 * sqrt(testdf$weight / testdf$bmi)


lapply(testdf$weight, if (is.na(testdf$weight) == TRUE) return("Y"))
testdf$missingweight <- is.na(testdf$weight)
testdf$missingheight <- is.na(testdf$height)
testdf$missingbmi <- is.na(testdf$bmi)


testdf$weight2 <- NULL
if (testdf$missingweight == TRUE) {
  if(testdf$missingheight == FALSE) {
    if(testdf$missingbmi == FALSE) {
      testdf$weight2 <- testdf$bmi * (testdf$height / 100)^2
    }
  }
}
