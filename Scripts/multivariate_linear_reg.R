# Multivariate linear regression on mental health and initial questionnaire:

d <- read.csv("bbhi_complete.csv")
ids <- d$id_user
rownames(d) <- ids

# We convert the categorical variables into factors:
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
############################

# Generating a new dataset with the sum_mental_health_1 column and the initial questions:
mental_health <- d[,c(27, 2:25)]

# Generating Training and Test sets:
# Creating the train and test indexes:
set.seed(123)
train_i <- sample(1:nrow(mental_health), 0.8*nrow(mental_health))
test_i <- setdiff(1:nrow(mental_health), train_i)
# Getting the train and test sets:
train_set <- mental_health[train_i,]
test_set <- mental_health[test_i,]

# Regression:
afit1 <- lm(sum_mental_health_1 ~ age+gender+ Q1.3.1+Q1.5+Q1.6+Q1.7+Q1.8+Q1.9+Q1.10+
              Q1.11+Q1.12+Q1.12.1+Q1.13+Q1.14+Q1.15+Q1.16+Q1.17+Q1.18+Q1.19.1+Q1.19.2+
              Q1.20.1+Q1.20.2+Q1.21+Q1.22, 
            data=train_set)
Anova(afit1) # least significant: age, Q1.11, Q1.22, Q1.8, Q1.20.2
summary(afit1)

afit2 <- lm(sum_mental_health_1 ~ gender+ Q1.3.1+Q1.5+Q1.6+Q1.7+Q1.9+Q1.10+
              Q1.12+Q1.12.1+Q1.13+Q1.14+Q1.15+Q1.16+Q1.17+Q1.18+Q1.19.1+Q1.19.2+
              Q1.20.1+Q1.21, 
            data=train_set)
Anova(afit2)
summary(afit2)

library(kable)
anova_fit2_res <- Anova(afit2)
write_csv(anova_fit2_res, "anova_fit2.csv")

summ_fit2 <- summary(afit2)
library( broom )
write.csv(tidy(afit2) , "coefficients.csv" )
write.csv(glance(afit2) , "an.csv" )

### Anova for afit2:

# Anova Table (Type II tests)
# 
# Response: sum_mental_health_1
# Sum Sq   Df F value    Pr(>F)    
# gender        5.9    1  0.8671   0.35188    
# Q1.3.1       11.1    1  1.6332   0.20142    
# Q1.5         48.7    3  2.3952   0.06656 .  
# Q1.6         84.2    5  2.4845   0.02980 *  
#   Q1.7         29.1    1  4.2932   0.03840 *  
#   Q1.9        141.6    2 10.4433 3.089e-05 ***
#   Q1.10        74.9    2  5.5253   0.00405 ** 
#   Q1.12        44.0    2  3.2441   0.03922 *  
#   Q1.12.1      31.6    3  1.5528   0.19896    
# Q1.13        41.6    4  1.5348   0.18949    
# Q1.14        28.3    3  1.3932   0.24307    
# Q1.15       318.0    3 15.6392 4.780e-10 ***
#   Q1.16        20.8    2  1.5327   0.21623    
# Q1.17        37.7    2  2.7839   0.06206 .  
# Q1.18        26.6    3  1.3073   0.27036    
# Q1.19.1      27.7    4  1.0229   0.39405    
# Q1.19.2      65.0    4  2.3982   0.04827 *  
#   Q1.20.1      37.3    5  1.1002   0.35824    
# Q1.21        28.3    4  1.0428   0.38366    
# Residuals 12632.9 1864                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### summary afit2
# Call:
#   lm(formula = sum_mental_health_1 ~ gender + Q1.3.1 + Q1.5 + Q1.6 + 
#        Q1.7 + Q1.9 + Q1.10 + Q1.12 + Q1.12.1 + Q1.13 + Q1.14 + Q1.15 + 
#        Q1.16 + Q1.17 + Q1.18 + Q1.19.1 + Q1.19.2 + Q1.20.1 + Q1.21, 
#      data = train_set)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.6238  -1.6723   0.2675   1.8831   7.3900 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.855630   2.459063   4.008 6.37e-05 ***
#   gender2      0.165670   0.177915   0.931  0.35188    
# Q1.3.1      -0.004819   0.003771  -1.278  0.20142    
# Q1.52        0.034789   0.226119   0.154  0.87774    
# Q1.53        0.520883   0.435554   1.196  0.23188    
# Q1.54       -0.376387   0.209709  -1.795  0.07285 .  
# Q1.62       -0.243658   0.255051  -0.955  0.33953    
# Q1.63       -0.541128   0.263156  -2.056  0.03989 *  
#   Q1.64       -0.786030   0.280536  -2.802  0.00513 ** 
#   Q1.65       -0.351175   0.466967  -0.752  0.45213    
# Q1.66       -0.807699   0.364067  -2.219  0.02664 *  
#   Q1.7         0.018975   0.009158   2.072  0.03840 *  
#   Q1.92       -0.650543   0.254329  -2.558  0.01061 *  
#   Q1.93        0.215799   0.195861   1.102  0.27069    
# Q1.102      -1.284362   0.810067  -1.586  0.11302    
# Q1.103       0.440235   0.168056   2.620  0.00888 ** 
#   Q1.122       0.726556   0.299715   2.424  0.01544 *  
#   Q1.123       0.885735   0.339476   2.609  0.00915 ** 
#   Q1.124       0.720620   0.259916   2.773  0.00562 ** 
#   Q1.12.11    -0.741456   0.437230  -1.696  0.09009 .  
# Q1.12.12    -0.275752   0.184225  -1.497  0.13461    
# Q1.12.13    -0.047239   0.167432  -0.282  0.77787    
# Q1.12.14           NA         NA      NA       NA    
# Q1.132       0.472635   0.324122   1.458  0.14495    
# Q1.133       0.435465   0.317604   1.371  0.17051    
# Q1.134       0.693008   0.333378   2.079  0.03778 *  
#   Q1.135       0.730457   0.391169   1.867  0.06201 .  
# Q1.142       0.248389   0.316013   0.786  0.43196    
# Q1.143       0.498662   0.307985   1.619  0.10559    
# Q1.144       0.324001   0.309192   1.048  0.29482    
# Q1.152       0.678752   0.384809   1.764  0.07792 .  
# Q1.153       1.150736   0.395983   2.906  0.00370 ** 
#   Q1.154       2.114357   0.427285   4.948 8.16e-07 ***
#   Q1.162       0.105631   0.371595   0.284  0.77624    
# Q1.163      -0.609848   0.356774  -1.709  0.08755 .  
# Q1.172       0.688648   0.293698   2.345  0.01914 *  
#   Q1.173       0.534822   0.467263   1.145  0.25253    
# Q1.182      -0.342363   1.143879  -0.299  0.76474    
# Q1.183      -0.352087   1.154755  -0.305  0.76047    
# Q1.184      -0.636882   1.148232  -0.555  0.57919    
# Q1.19.11     0.628920   0.658390   0.955  0.33958    
# Q1.19.12     0.709056   0.639286   1.109  0.26751    
# Q1.19.13     0.704394   0.642979   1.096  0.27343    
# Q1.19.14     0.326583   0.666047   0.490  0.62396    
# Q1.19.21    -1.210218   0.762837  -1.586  0.11280    
# Q1.19.22    -0.959604   0.752444  -1.275  0.20236    
# Q1.19.23    -0.855574   0.758790  -1.128  0.25965    
# Q1.19.24    -0.409223   0.774475  -0.528  0.59729    
# Q1.20.11    -1.195325   1.223121  -0.977  0.32856    
# Q1.20.12     0.220796   0.590413   0.374  0.70847    
# Q1.20.13     0.157794   0.588773   0.268  0.78873    
# Q1.20.14     0.173762   0.643023   0.270  0.78702    
# Q1.20.15     0.509106   0.609280   0.836  0.40349    
# Q1.212      -1.180462   1.381513  -0.854  0.39295    
# Q1.213      -1.319239   1.359082  -0.971  0.33183    
# Q1.214      -1.066083   1.361826  -0.783  0.43382    
# Q1.215      -1.235606   1.367670  -0.903  0.36641    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.603 on 1864 degrees of freedom
# Multiple R-squared:  0.1211,	Adjusted R-squared:  0.0952 
# F-statistic: 4.671 on 55 and 1864 DF,  p-value: < 2.2e-16

prediction <- predict(afit2, test_set)

error <- prediction - test_set[["sum_mental_health_1"]]
sqrt(mean(error^2))
# 2.709817
