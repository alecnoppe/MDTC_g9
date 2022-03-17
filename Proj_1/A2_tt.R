data <- readRDS("incomplete_data_g9.rds")
mAge <- is.na(data$age)
mBmi <- is.na(data$bmi)
mHeight <- is.na(data$height)
mWeight <- is.na(data$weight)
mSmoke <- is.na(data$smoke)
mSex <- is.na(data$sex)
mInt <- is.na(data$intensity)
mAct <- is.na(data$active)
mRest <- is.na(data$rest)

age.mAge <- t.test(age ~ mAge, data = data)$statistic
age.mBmi <- t.test(age ~ mBmi, data = data)$statistic
age.mSmoke <- t.test(age ~ mSmoke, data = data)$statistic
age.mSex <- t.test(age ~ mSex, data = data)$statistic
age.mHeight <- t.test(age ~ mHeight, data = data)$statistic
age.mWeight <- t.test(age ~ mWeight, data = data)$statistic
age.mInt <- t.test(age ~ mInt, data = data)$statistic
age.mAct <- t.test(age ~ mAct, data = data)$statistic
age.mRest <- t.test(age ~ mRest, data = data)$statistic

age.missing <- c(age.mAge, age.mSmoke, 0, 0, age.mAct, 0, age.mHeight, age.mWeight, age.mBmi)

bmi.mAge <- t.test(bmi ~ mAge, data = data)$statistic
bmi.mSmoke <- t.test(bmi ~ mSmoke, data = data)$statistic
bmi.mSex <- t.test(bmi ~ mSex, data = data)$statistic
bmi.mHeight <- t.test(bmi ~ mHeight, data = data)$statistic
bmi.mWeight <- t.test(bmi ~ mWeight, data = data)$statistic
bmi.mInt <- t.test(bmi ~ mInt, data = data)$statistic
bmi.mAct <- t.test(bmi ~ mAct, data = data)$statistic
bmi.mRest <- t.test(bmi ~ mRest, data = data)$statistic
bmi.mBmi <- t.test(bmi ~ mBmi, data = data)$statistic

bmi.missing <- c(bmi.mAge, bmi.mSmoke, 0, 0, bmi.mAct, 0, bmi.mHeight, bmi.mWeight, 1)

