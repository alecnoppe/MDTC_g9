library(mice)
library(ggplot2)
dfcom <- readRDS("complete_data.rds")
dfinc <- readRDS("incomplete_data_g9.rds")

summary(nhanes2)
imp1 <- mice(dfinc, m = 10, seed = 235711, print = FALSE)
library(dplyr)

## Compute the MI estimated means from 'imp1':
complete(imp1, "all") %>% 
  lapply(function(x) select(x, where(is.numeric)) %>% colMeans()) %>%
  do.call(rbind, .) %>%
  colMeans()
#means of incomplete dataset
dfinc %>% select(where(is.numeric)) %>% colMeans(na.rm = TRUE)

imp <- mice(dfinc, seed = 235711, print = FALSE)
imp$predictorMatrix
plot(imp)
densityplot(imp)
meth <- c(age = "", smoke = "pmm", sex = "", intensity ="", active = "cart" , rest = "", height = "cart", weight = "cart", bmi = "bmi")
#First imputation
pred <- quickpred(dfinc, mincor = 0.45)
imp1 <- mice(dfinc, 
              m = 8, 
              method = "norm", 
              predictorMatrix = pred, 
              seed = 235711,
              print = FALSE)
plot(imp1)
densityplot(imp1)

#Second imputation
pred1 <- mice(dfinc, maxit = 0)$predictorMatrix
imp2 <- mice(dfinc, 
             m = 15, 
             method = meth, 
             predictorMatrix = pred1,
             seed = 235711,
             print = FALSE)
