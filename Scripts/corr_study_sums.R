# Study of correlation between sums:

d <- read.csv("bbhi_complete.csv")
View(d)

ids <- d$id_user
rownames(d) <- ids

# We save the values of the sums in as new table:
sums <- d[,26:56]

# Now we will look for correlation between sums:
library(corrplot)
corrplot(cor(sums), method = "color")

# Saving the plot:
# png('corrplot_sums.png')
# corrplot(cor(sums), method = "color")
# dev.off()

# We create a data frame with the questions and the sum_mental_health:
mental_health <- d[,c(27, 2:25)]
View(mental_health)

# We find the correlation between mental health and the initial questionnairecreate (first questions):
cor_table_mental <- data.frame(cor(mental_health))
# we save the table:
# write_csv(cor_table_mental, "cor_table_mental.csv")

# We plot the correlation:
corrplot(cor(mental_health), method = "color")

# We save the plot:
# png('corrplot_mental_health.png')
# corrplot(cor(mental_health), method = "color")
# dev.off()