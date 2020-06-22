# Clustering mental health sum:
###############################
library(tidyverse)
library(ggplot2)
# Reading file:
d <- read.csv("bbhi_complete.csv")
ids <- d$id_user
rownames(d) <- ids

d$gender <- as.factor(d$gender)
d$Q1.5 <- as.factor(d$Q1.5)
d$Q1.6 <- as.factor(d$Q1.6)
d$Q1.9 <- as.factor(d$Q1.9)
d$Q1.10 <- as.factor(d$Q1.10)
d$Q1.11 <- as.factor(d$Q1.11)
d$Q1.12 <- as.factor(d$Q1.12)
d$Q1.12.1 <- as.factor(d$Q1.12.1)
d$Q1.13 <- as.factor(d$Q1.13)
d$Q1.14 <- as.factor(d$Q1.14)
d$Q1.15 <- as.factor(d$Q1.15)
d$Q1.16 <- as.factor(d$Q1.16)
d$Q1.17 <- as.factor(d$Q1.17)
d$Q1.18 <- as.factor(d$Q1.18)
d$Q1.19.1 <- as.factor(d$Q1.19.1)
d$Q1.19.2 <- as.factor(d$Q1.19.2)
d$Q1.20.1 <- as.factor(d$Q1.20.1)
d$Q1.20.2 <- as.factor(d$Q1.20.2)
d$Q1.21 <- as.factor(d$Q1.21)
d$Q1.22 <- as.factor(d$Q1.22)

mental_health <- d[,c(1,27, 2:25)]

# Now we will create the dummy variables and do a one-hot encoding:
library(caret)
dummy_data <- dummyVars(" ~ .", data = mental_health)
data_dumVars <- data.frame(predict(dummy_data, newdata = mental_health))

head(data_dumVars)

# We save the numerical variables which we will scale:
numerical_variables <- c(2, 3, 6, 17, 18)
numer <- data.frame(data_dumVars[,c(1, numerical_variables)])

categ <- data.frame(data_dumVars[, -numerical_variables])

scaled_numerical <- data.frame(scale(numer[,-1]))
head(scaled_numerical)

id_user <- numer[,1]

sc_numer <- cbind(id_user, scaled_numerical)

scaled_data <- merge(sc_numer, categ)
head(scaled_data)

data <- scaled_data[,-1]
rownames(data) <- scaled_data[,1]
head(data)

# CLUSTERING:
set.seed(123)
library(factoextra)
# Elbow method:
elbow_plot <- fviz_nbclust(data, kmeans, method = "wss", k.max = 24) + 
  theme_minimal() + ggtitle("the Elbow Method")
ggsave("elbow_plot.png",
       plot = elbow_plot,
       device = "png")
  # optimal number of clusters = 3

# We perform k-means:
set.seed(123)
km3 <- kmeans(data, 3, nstart = 25)

# We save the clusters to the data:
dd <- cbind(data, clusters = km3$cluster)
mental_clusters <- cbind(mental_health, clusters = km3$cluster)


# t-SNE:
library(tsne)
set.seed(123)
x <- tsne(data, whiten = FALSE)
plot(x, col = dd$clusters)

mental_clusters$clusters <- as.factor(mental_clusters$clusters)

cluster1 <- mental_clusters[mental_clusters$clusters == 1,]
cluster2 <- mental_clusters[mental_clusters$clusters == 2,]
cluster3 <- mental_clusters[mental_clusters$clusters == 3,]

install.packages("memisc")
library(memisc)
library(data.table)
t_mental_clusters <- as.data.table(mental_clusters, keep.rownames=FALSE)

percentages(t_mental_clusters, by = clusters, which = Q1.15)
summary(mental_health$sum_mental_health_1)
mean(cluster1$sum_mental_health_1)
sd(cluster1$sum_mental_health_1)
mean(cluster2$sum_mental_health_1)
sd(cluster2$sum_mental_health_1)
mean(cluster3$sum_mental_health_1)
sd(cluster3$sum_mental_health_1)

table(cluster1$Q1.12.1)
table(cluster2$Q1.12.1)
table(cluster3$Q1.12.1)
