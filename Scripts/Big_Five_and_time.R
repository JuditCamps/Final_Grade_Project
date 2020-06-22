data <- read.csv("Vital_plan_and_personality.csv", sep = ";", header = TRUE, na.strings = "NULL")
dim(data)

headers <- names(data)

library(tidyverse)

cols_vital <- select(data, X4002:X4111)
cols_pers <- select(data, X10901:X10950)
names_vital <- names(cols_vital)
names_pers <- names(cols_pers)


num_cols_vital <- match(names_vital, names(data))
num_cols_pers <- match(names_pers, names(data))


# Obtaining table with id_user, age, gender and results for personality questionnaire:
id_user <- data$id_user
age <- data$age
gender <- data$gender

pers_data <- select(data, X10901:X10950)

personality_test <- cbind(id_user, age, gender, pers_data)

dim(personality_test)
personality_test[1:5, 1:5]

# Compute values for 
# Questions index for each personality type:
E_pers <- seq(1+3, 50+3, by = 5)
A_pers <- seq(2+3, 50+3, by = 5)
C_pers <- seq(3+3, 50+3, by = 5)
N_pers <- seq(4+3, 50+3, by = 5)
O_pers <- seq(5+3, 50+3, by = 5)

# Positives/negatives values for each personality:
E_vals <- c(+1, -1, +1, -1, +1, -1, +1, -1, +1, -1)
A_vals <- c(-1, +1, -1, +1, -1, +1, -1, +1, +1, +1)
C_vals <- c(+1, -1, +1, -1, +1, -1, +1, -1, +1, +1)
N_vals <- c(-1, +1, -1, +1, -1, -1, -1, -1, -1, -1)
O_vals <- c(+1, -1, +1, -1, +1, -1, +1, +1, +1, +1)


# Calculate the values for each different personality:
Extroversion <- 20+rowSums(E_vals*personality_test[,E_pers])
Agreeableness <- 14 + rowSums(A_vals*personality_test[,A_pers])
Conscientiousness <- 14 + rowSums(C_vals*personality_test[,C_pers])
Neuroticism <- 38 + rowSums(N_vals*personality_test[N_pers])
Openness <- 8+rowSums(O_vals*personality_test[,O_pers])

Big_Five_Results <- cbind(id_user, age, gender, Openness, Conscientiousness, Extroversion, Agreeableness, Neuroticism)

head(Big_Five_Results)
dim(Big_Five_Results)

summary(Big_Five_Results)

# Save as a new table:
# write.csv(Big_Five_Results, "Big_Five_Results.csv")

##### 
# Joining personality and times:

times.personality <- read.csv("personality_times.csv", sep = ",")
head(times.personality)
times.vital <- read.csv("vital_plan_times.csv", sep = ",")
head(times.vital)

times.personality <- rename(times.personality, sec_diff_pers = sec_difference)
head(times.personality)
times.vital <- rename(times.vital, sec_diff_vital = sec_difference)
head(times.vital)


BF_pers <- merge(x = Big_Five_Results, y = times.personality[,2:3])
head(BF_pers)
dim(BF_pers)
BF_vital <- merge(x = Big_Five_Results, y = times.vital[,2:3])
head(BF_vital)
dim(BF_vital)

Big_five_times <- merge(x=BF_pers, y=BF_vital)
dim(Big_five_times)
summary(Big_five_times)

names(Big_five_times)
write.csv(Big_five_times, "Big_Five_Times.csv")
