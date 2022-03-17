library(mice)
library(naniar)
library(ggplot2)

setwd("~/Dropbox/Work/Year 3/Missing data theory/Group assignments/Assignment 1")
incds <- readRDS ("incomplete_data_g9.rds")
cds <- readRDS ("complete_data.rds")
summary(incds)

proportionna <- colMeans(is.na(incds))
proportionna ["bmi"] 
gg_miss_var(incds)
md.pattern(incds)
mweight <- is.na(incds$weight)
test <- t.test(age ~ mweight, data=incds)
test$statistic
test$p.value

ggplot(data=incds, mapping = aes(x=weight, y=age)) + geom_miss_point()
t.test(age ~ is.na(bmi), data = incds)

library(finalfit)
library(GGally)

ff_glimpse(
  incds,
  dependent = "active",
  explanatory = c('rest', 'smoke', 'bmi', 'intensity', 'age'),
)
incds %>%
  missing_pairs(dependent, explanatory)
incds %>% 
  missing_pairs(dependent, explanatory, position = "fill", )
