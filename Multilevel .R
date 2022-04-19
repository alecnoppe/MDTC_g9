require(mice)

"age" , "smoke", "sex", "intensity", "active", "rest", "height", "weight", "bmi"
setwd("~/Dropbox/Work/Year 3/Missing data theory/Group assignments/Assignment 1")
dfcom <- readRDS("complete_data.rds")
dfincom <- readRDS("incomplete_data_g9.rds")
d <- dfincom[, c("bmi", "active")]
f <- brandsma[, c("sch", "lpo")]


#  d$sex<-ifelse(d$sex=="male",1,0)

#continuos
library(mice)
impmethod <- character(ncol(dfincom))
names(impmethod) <- colnames(dfincom)
impmethod["bmi"] <- "2l.lmer"
impmethod
pm <- make.predictorMatrix(dfincom)
pm
pm[, "age"] <- 0
pm["bmi", c("age", "smoke", "sex", "intensity", "active", "rest", "height", "weight")] 
pm
res.mice.md <- mice(dfincom, m=5, predictorMatrix = pm,
                    method=impmethod, maxit=10, printFlag = FALSE, seed=1874)
res.mice.md

#dichotomous
dfincom$smoke <- factor(dfincom$smoke)
impmethod <- character(ncol(dfincom))
names(impmethod) <- colnames(dfincom)
impmethod["smoke"] <- "2l.bin"
impmethod
pm <- make.predictorMatrix(dfincom)
pm
pm[, "age"] <- 0
pm["smoke", c("age", "smoke", "sex", "intensity", "active", "rest", "height", "weight")]
pm
res.mice.md <- mice(dfincom, m=5, predictorMatrix = pm,
                    method=impmethod, maxit=10, printFlag = FALSE, seed=1983)
res.mice.md


#running it again
require(mice)
require(lattice)
require(pan)
head(dfincom)
summary(dfincom)
md.pattern(dfincom) #13 unique patterns
md.pattern(dfincom[ , -8]) #pattern without weight
#weight has the highest missing values, hence determine what other values are affected by weight
histogram(~ active | is.na(weight), data=dfincom) # relation
histogram(~ active | is.na(height), data=dfincom) # no relation
histogram(~ active | is.na(bmi), data=dfincom) #relation
histogram(~ height | is.na(smoke), data=dfincom) # some

ini <- mice(dfincom, maxit = 0)
meth <- ini$meth
meth
meth[c(5,7,8,9)] <- "norm" #using norm instead of pmm
meth
pred <- ini$pred
pred
pred[, "age"] <- 0
pred[, "sex"] <- 0
pred[, "intensity"] <- 0
pred[, "rest"] <- 0
pred

imp1 <- mice(dfincom, meth = meth, pred = pred, print = FALSE)
#compare
summary(complete(imp1))
summary(dfincom)
#compare icc
library(multilevel)
data.frame(vars = names(dfincom)) 
           observed = c(ICC1(aov(active ~ weight, dfincom)), 
                        ICC1(aov(active ~ bmi, dfincom)), 
                        ICC1(aov(height ~ smoke, dfincom))) 
           norm     = c(ICC1(aov(active ~ weight, complete(imp1))), 
                        ICC1(aov(active ~ bmi, complete(imp1))), 
                        ICC1(aov(height ~ smoke, complete(imp1))))
#Run the same thing but include sex as predictor for all
#should try again with intensity
           
pred <- ini$pred
pred[, "age"] <- 0
pred[, "intensity"] <- 0
pred[, "rest"] <- 0
imp2 <- mice(dfincom, meth = meth, pred = pred, print = FALSE)
#compare icc
data.frame(vars = names(dfincom)) 
observed = c(ICC1(aov(active ~ sex, dfincom)), 
             ICC1(aov(age ~ sex, dfincom)), 
             ICC1(aov(height ~ sex, dfincom))) 
norm     = c(ICC1(aov(active ~ sex, complete(imp1))), 
             ICC1(aov(age ~ sex, complete(imp1))), 
             ICC1(aov(height ~ sex, complete(imp1))))
normclass = c(ICC1(aov(active ~ sex, complete(imp2))), 
             ICC1(aov(age ~ sex, complete(imp2))), 
             ICC1(aov(height ~ sex, complete(imp2))))
observed
norm
normclass #normclass and observed are closest
# This uses thefixed effects approach. This conforms to formulating seperate regression models for each class and imputing within classes from these models.
plot(imp2, c("active", "weight", "bmi"))
imp3 <- mice.mids(imp2, maxit = 10)
plot(imp3, c("active", "weight", "bmi"))
imp3b <- mice.mids(imp3, maxit = 20, print = FALSE)
plot(imp3b, c("active", "weight", "bmi"))
#convergence is alright
densityplot(imp2)

#impute using PMM with all variables as predictors
imp4 <- mice(dfincom)
densityplot(imp4) #shape is there
#compare ICC again
data.frame(vars = names(dfincom)) 
observed = c(ICC1(aov(active ~ sex, dfincom)), 
             ICC1(aov(age ~ sex, dfincom)), 
             ICC1(aov(height ~ sex, dfincom))) 
norm     = c(ICC1(aov(active ~ sex, complete(imp1))), 
             ICC1(aov(age ~ sex, complete(imp1))), 
             ICC1(aov(height ~ sex, complete(imp1))))
normclass = c(ICC1(aov(active ~ sex, complete(imp2))), 
              ICC1(aov(age ~ sex, complete(imp2))), 
              ICC1(aov(height ~ sex, complete(imp2))))
PMM = c(ICC1(aov(active ~ sex, complete(imp4))), 
              ICC1(aov(age ~ sex, complete(imp4))), 
              ICC1(aov(height ~ sex, complete(imp4))))
original = c(ICC1(aov(active ~ as.factor(sex), complete(imp4))), 
                  ICC1(aov(active ~ as.factor(sex), complete(imp4))), 
                  ICC1(aov(height ~ as.factor(sex), complete(imp4))))
observed
norm
normclass
PMM
original

#using 2l.norm
ini <- mice(dfincom, maxit = 0)
pred <- ini$pred
pred["age", ]  <- c(0, 2, 2, 3, 0, 2, 2,2,2)
meth <- ini$meth
meth <- c("2l.norm", "", "", "", "", "", "","","")
imp5 <- mice(dfincom, pred = pred, meth=meth, print = FALSE)
bwplot(imp5, ~age)
stripplot(imp5)

plot(density(dfincom$age))  #true data 
lines(density(complete(imp5)$age), col = "red", lwd = 2)  #2l.norm
lines(density(complete(imp4)$age), col = "green", lwd = 2)  #PMM
#very close to each other


#using 2l.pan
ini <- mice(dfincom, maxit = 0)
pred <- ini$pred
pred["age", ]  <- c(0, 2, 2, 3, 0, 2, 2,2,2)
meth <- ini$meth
meth <- c("2l.pan", "", "", "", "", "", "","","")
imp6 <- mice(dfincom, pred = pred, meth=meth, print = FALSE)
bwplot(imp6, ~age)
stripplot(imp6)

plot(density(dfincom$age), main = "black = truth | green = PMM | red = 2l.pan")  # 
lines(density(complete(imp6)$age), col = "red", lwd = 2)  #2l.pan
lines(density(complete(imp4)$age), col = "green", lwd = 2)  #PMM

plot(imp6)

#putting it all together
#2l.norm: Imputes univariate missing data using a two-level normal model with heterogeneous within group variances
#2l.pan: Imputes univariate missing data using a two-level normal model with homogeneous within group variances
#2lonly.mean: Imputes the mean of the class within the class
#2lonly.norm: Imputes univariate missing data at level 2 using Bayesian linear regression analysis
#2lonly.pmm: Imputes univariate missing data at level 2 using predictive mean matching

#each with the best method
summary(dfincom)
ini <- mice(dfincom, maxit = 0)
pred <- ini$pred
pred["smoke", ] <- c(0, 2, 0, 0, 2, 0, 2,2,2)  
pred["active", ] <- c(0, 2, 0, 0, 2, 0, 2,2,2)  
pred["height", ] <- c(0, 2, 0, 0, 2, 0, 2,2,2)  
pred["weight", ] <- c(0, 2, 0, 0, 2, 0, 2,2,2)#cant find algorithim fro weight
pred["bmi", ] <- c(0, 2, 0, 0, 2, 0, 2,2,2)  
meth <- ini$meth
meth <- c("", "logreg", "", "", "2l.norm", "", "2l.norm","","2l.pan")
imp7 <- mice(dfincom, pred = pred, meth = meth, print = FALSE)

#all with pmm
pmmdata <- dfincom
pmmdata$intensity <- as.factor(dfincom$intensity)
imp8 <- mice(pmmdata, m = 5, print = FALSE)
densityplot(imp8)
plot(imp8)
