#install.packages("Hmisc")
#install.packages("psych")
#install.packages("car")

library(Hmisc)
library(psych)
library(car)
library(ggplot2)
library(devtools)
library(reshape2)

BFP_times <- read.csv("Big_Five_Times.csv")
#View(BFP_times)
summary(BFP_times)
BFP_times$X <- NULL

id_user <- BFP_times$id_user

BFP_times$id_user <- NULL
rownames(BFP_times) <- id_user


# Eliminating outliers from time_personality:
Q <- quantile(BFP_times$sec_diff_pers, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(BFP_times$sec_diff_pers)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

# Eliminating outliers from time_vital:
Q2 <- quantile(BFP_times$sec_diff_vital, probs=c(.25, .75), na.rm = FALSE)
iqr2 <- IQR(BFP_times$sec_diff_vital)
up2 <-  Q2[2]+1.5*iqr2 # Upper Range  
low2<- Q2[1]-1.5*iqr2 # Lower Range

eliminated<- subset(BFP_times, BFP_times$sec_diff_pers > (Q[1] - 1.5*iqr) 
                    & BFP_times$sec_diff_pers < (Q[2]+1.5*iqr))

eliminated<- subset(eliminated, eliminated$sec_diff_vital > (Q2[1] - 1.5*iqr2) 
                    & eliminated$sec_diff_vital < (Q2[2]+1.5*iqr2))

summary(eliminated) # Summary of the new table with no outliers



#### 
# Correlation:
d <- eliminated
head(d)


# Code from:
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

corr <- round(cor(d),2)
melt_corr <- melt(corr)

ggplot(data = melt_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(corr)
upper_tri

melt_upper <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melt_upper, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


cormap <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# We save the correlation plot:
# 
# ggsave(
#   "cormap.png",
#   plot = cormap,
#   device = "png",
# )

  


