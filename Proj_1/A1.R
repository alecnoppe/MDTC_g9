data <- readRDS("incomplete_data_g9.rds")
smoke_y <- sum(is.na(subset(data, smoke == "yes")))/nrow(subset(data, smoke == "yes"))
smoke_n <- sum(is.na(subset(data, smoke == "no")))/nrow(subset(data, smoke == "no"))
sex_f <- sum(is.na(subset(data, sex == "female")))/nrow(subset(data, sex == "female"))
sex_m <- sum(is.na(subset(data, sex == "male")))/nrow(subset(data, sex == "male"))
int_l <- sum(is.na(subset(data, intensity  == "low")))/nrow(subset(data, intensity  == "low"))
int_m <- sum(is.na(subset(data, intensity  == "moderate")))/nrow(subset(data, intensity  == "moderate"))
int_h <- sum(is.na(subset(data, intensity  == "high")))/nrow(subset(data, intensity  == "high"))


c_smoke_y <- c(smoke_y, smoke_y)
c_smoke_n <- c(smoke_n, smoke_n)
c_smoke <- data.frame("yes" = c_smoke_y, "no" = c_smoke_n)
c_sex_m <- c(sex_m, sex_m)
c_int_l <- c(int_l, int_l)
c_int_m <- c(int_m, int_m)
c_int_h <- c(int_h, int_h)

hist_1_data <- data.frame("Smoke: yes" = c_smoke_y, "Smoke: no" = c_smoke_n, "sex: female" = c_sex_f, "sex: male" = c_sex_m, "int: low" = c_int_l, "int: moderate" = c_int_m, "int: high" = int_h)
barplot(c_smoke, names.arg = c("Smoke? yes", "Smoke? no"), col = c("#E69F00", "#56B4E9"))

