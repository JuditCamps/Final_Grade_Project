# Time plots for Vital plan and personality questionnaires:
library(ggplot2)

# Vital plan:
t_vp <- read.csv("vital_plan_times.csv", sep = ";")
View(t_vp)
summary(t_vp)

# Removing the outliars from vital plan times:
Q <- quantile(t_vp$sec_difference, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(t_vp$sec_difference)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

noout_t_vp <- subset(t_vp, t_vp$sec_difference > (Q[1] - 1.5*iqr) 
                    & t_vp$sec_difference < (Q[2]+1.5*iqr))

summary(noout_t_vp)

# Saving table as a new csv file:
# write.csv(noout_t_vp, "times_vital_nooutliers.csv")


# Personality:
t_p <- read.csv("personality_times.csv", sep = ";")
View(t_p)
summary(t_p)

# Removing outliars from personality times:
Q <- quantile(t_p$sec_difference, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(t_p$sec_difference)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

noout_t_p <- subset(t_p, t_p$sec_difference > (Q[1] - 1.5*iqr) 
                     & t_p$sec_difference < (Q[2]+1.5*iqr))

summary(noout_t_p)
# Saving table as a new csv file:
 #write.csv(noout_t_p, "times_personality_nooutliers.csv")


# Creating the time plots:
# Vital plan time plot:
vital_plan_time <- ggplot(data=noout_t_vp, aes(sec_difference)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept=mean(sec_difference)),
             linetype="dashed") +
  labs(x = "Seconds", y = "Count") + 
  ggtitle("Time for Vital Plan") + 
  theme_minimal()
summary(noout_t_vp)
ggsave(
  "vital_plan_time.png",
  plot = vital_plan_time,
  device = "png"
)

# Personality time plot:
personality_time <- ggplot(data=noout_t_p, aes(sec_difference)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept=mean(sec_difference)),
             linetype="dashed") + 
  labs(x = "Seconds", y = "Count") + 
  ggtitle("Time for Personality") + 
  theme_minimal()
summary(noout_t_p)
ggsave(
  "personality_time.png",
  plot = personality_time,
  device = "png"
)
