library(mice)
library(dplyr)
library(ggplot2)
library(naniar)
library(tidyr)

dfcom <- readRDS("complete_data.rds")
dfinc <- readRDS("incomplete_data_g9.rds")

miceoutMean <- mice(dfinc,
                method = ifelse(colnames(dfinc) == "smoke", "logreg", "mean"),
                m = 1,
                maxit = 1)
dfMeanimp <- complete(miceoutMean)

summary(dfMeanimp)
summary(dfinc)
summary(dfcom)

miceoutReg <- mice(dfinc,
                   method = ifelse(colnames(dfinc) == "smoke", "logreg", "norm.predict"),
                   m = 1,
                   maxit = 1)
dfRegimp <- complete(miceoutReg)
summary(dfRegimp)
summary(dfMeanimp)

xyplot(miceoutMean, active + bmi + height + weight ~ age + rest)
xyplot(miceoutReg, active + bmi + height + weight ~ age + rest)
#There are no uneven distributions of the NAs: they seem equally distributed over the other vars.

modelCom <- lm(active ~ rest + bmi + sex + smoke + intensity, data = dfcom)
modelList <- lm(active ~ rest + bmi + sex + smoke + intensity, data = dfinc)
modelMean <- lm(active ~ rest + bmi + sex + smoke + intensity, data = dfMeanimp)
modelReg <- lm(active ~ rest + bmi + sex + smoke + intensity, data = dfRegimp)
summary(modelCom)
summary(modelList)
summary(modelMean)
summary(modelReg)

#some exploratory histograms: TODO do them in one
#how?
#Maybe:
#1: create df for each variable
#2: fill columns for with each imputation method for that variable
#3: create long dataframe?
#Plot
ggplot(modelCom, aes(x=active)) + geom_histogram(binwidth = 3, color = "gray", fill = "white")
ggplot(modelList, aes(x=active)) + geom_histogram(binwidth = 3, color = "gray", fill = "white")
ggplot(modelMean, aes(x=active)) + geom_histogram(binwidth = 3, color = "gray", fill = "white")
ggplot(modelReg, aes(x=active)) + geom_histogram(binwidth = 3, color = "gray", fill = "white")

dfactive <- data.frame(complete = dfcom$active,
                       listwise = dfinc$active,
                       meanimp = dfMeanimp$active,
                       regimp = dfRegimp$active)

dfbmi <- data.frame(complete = dfcom$bmi,
                    listwise = dfinc$bmi,
                    meanimp = dfMeanimp$bmi,
                    regimp = dfRegimp$bmi)

dfheight <- data.frame(complete = dfcom$height,
                      listwise = dfinc$height,
                      meanimp = dfMeanimp$height,
                      regimp = dfRegimp$height)

dfweight <- data.frame(complete = dfcom$weight,
                       listwise = dfinc$weight,
                       meanimp = dfMeanimp$weight,
                       regimp = dfRegimp$weight)

ggplot(gather(dfactive), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

ggplot(gather(dfbmi), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

ggplot(gather(dfheight), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

ggplot(gather(dfweight), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


#Monotone data; visual analysis
#If BMI is missing, then WEIGHT is always missing.
#If HEIGHT is missing, then WEIGHT is always missing; but NOT BMI
#If ACTIVE is missing, then WEIGHT is always missing
#If SMOKE is missing, then ACTIVE and WEIGHT are always missing.

df_missing_lowint <- dfinc[dfinc$intensity == "low",]
df_missing_medint <- dfinc[dfinc$intensity == "moderate",]
df_missing_hiint <- dfinc[dfinc$intensity == "high",]
vis_miss(df_missing_lowint)
md.pattern(df_missing_lowint)



md.pattern(df_missing_lowint, plot = FALSE)

pc <- md.pairs(dfinc)
pc
round(100 * pc$mr/(pc$mr+pc$mm))
round(100 * pc$rm/(pc$rm+pc$rr))

pl <- md.pairs(df_missing_lowint)
pl
round(100 * pl$mr/(pl$mr+pl$mm)) 
round(100 * pl$rm/(pl$rm+pl$rr))

pm <- md.pairs(df_missing_medint)
pm
round(100 * pm$mr/(pm$mr+pm$mm)) 
round(100 * pm$rm/(pm$rm+pm$rr))

ph <- md.pairs(df_missing_hiint)
ph
round(100 * ph$mr/(ph$mr+ph$mm))
round(100 * ph$rm/(ph$rm+ph$rr))


