missingbmi <- glm(is.na(bmi) ~ age + smoke + sex + intensity + active + rest + height, family = "binomial", data = dfinc)
missingheight <- glm(is.na(height) ~ age + smoke + sex + intensity + active + rest + bmi, family = "binomial", data = dfinc)
missingactive <- glm(is.na(active) ~ age + smoke + sex + intensity + rest + bmi, family = "binomial", data = dfinc)
missingsmoke <- glm(is.na(smoke) ~ age + sex + intensity + rest + bmi + height, family = "binomial", data = dfinc)

summary(missingbmi)
summary(missingheight)
summary(missingactive)
summary(missingsmoke)
