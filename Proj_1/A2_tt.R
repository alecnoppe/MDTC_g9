data <- readRDS("incomplete_data_g9.rds")
mBmi <- is.na(data$bmi)
mHeight <- is.na(data$height)
mWeight <- is.na(data$weight)
mSmoke <- is.na(data$smoke)
mSex <- is.na(data$sex)
mInt <- is.na(data$intensity)
mAct <- is.na(data$active)
mRest <- is.na(data$rest)


age.mBmi <- t.test(age ~ mBmi, data = data)$statistic
age.mSmoke <- t.test(age ~ mSmoke, data = data)$statistic
age.mSex <- t.test(age ~ mSex, data = data)$statistic
age.mHeight <- t.test(age ~ mHeight, data = data)$statistic
age.mWeight <- t.test(age ~ mWeight, data = data)$statistic
age.mInt <- t.test(age ~ mInt, data = data)$statistic
age.mAct <- t.test(age ~ mAct, data = data)$statistic
age.mRest <- t.test(age ~ mRest, data = data)$statistic

age.missing <- c(age.mSmoke, 0, 0, age.mAct, 0, age.mHeight, age.mWeight, age.mBmi)
age.missing