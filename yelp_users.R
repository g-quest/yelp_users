library(dplyr)
library(ggplot2)
library(readr)
library(e1071)
library(car)
library(outliers)
library(lmtest)

# read in users data into a data frame
readDF <- read_csv('/Users/g_quest/Desktop/yelp_academic_dataset_user.csv')
class(readDF)

# explore raw data
dim(readDF)
names(readDF)
glimpse(readDF) # cleaner alternative to str()
summary(readDF) # sanity check

# cut out irrelevant columns to reduce size and improve performance
users <- readDF[-c(1,8,10,15,16,18,21)]

# check NAs
any(is.na(users))

# descriptive statistics
summary(users)
sapply(users, sd)
sapply(users, skewness)
sapply(users, kurtosis)

# reduce dataset to avid users
users_sub <- subset(users, users$review_count > 19 & users$review_count < 201)

summary(users_sub)
sapply(users_sub, mean)
sapply(users_sub, sd)
sapply(users_sub, skewness)
sapply(users_sub, kurtosis)

users_sub['review_count'] <- log10(users_sub$review_count) 
users_sub['compliment_plain'] <- log10(users_sub$compliment_plain + 1)
users_sub['compliment_writer'] <- log10(users_sub$compliment_writer + 1)
users_sub['compliment_note'] <- log10(users_sub$compliment_note + 1)
users_sub['compliment_more'] <- log10(users_sub$compliment_more + 1)
users_sub['compliment_photos'] <- log10(users_sub$compliment_photos + 1)
users_sub['compliment_funny'] <- log10(users_sub$compliment_funny + 1)
users_sub['compliment_cute'] <- log10(users_sub$compliment_cute + 1)
users_sub['compliment_hot'] <- log10(users_sub$compliment_hot + 1)
users_sub['compliment_profile'] <- log10(users_sub$compliment_profile + 1)
users_sub['compliment_cool'] <- log10(users_sub$compliment_cool + 1)
users_sub['compliment_list'] <- log10(users_sub$compliment_list + 1)
users_sub['useful'] <- log10(users_sub$useful + 1)
users_sub['funny'] <- log10(users_sub$funny + 1)
users_sub['cool'] <- log10(users_sub$cool + 1)
users_sub['fans'] <- log10(users_sub$fans + 1)

summary(users_sub)
sapply(users_sub, sd)
sapply(users_sub, skewness)
#sapply(users_sub, kurtosis)

# initial model
lm1 <- lm(review_count ~ compliment_photos + compliment_list + compliment_funny + 
            compliment_plain + compliment_note + compliment_writer + 
            compliment_cute + compliment_more + compliment_hot + compliment_profile + 
            compliment_cool, data = users_sub)

summary(lm1)

lm2 <- lm(review_count ~ compliment_photos + compliment_list + compliment_funny + 
            compliment_plain + compliment_note + compliment_writer + 
            compliment_cute + compliment_more + compliment_hot + compliment_profile, 
            data = users_sub)

summary(lm2)

# examine extreme outliers
outlierTest(lm3)

lm3 <- lm(review_count ~ compliment_photos + compliment_list + compliment_funny + 
            compliment_plain + compliment_note + compliment_writer + 
            compliment_cute + compliment_more + compliment_hot + compliment_profile, 
          data = sub3)


sub3 <- users_sub2[-c(70951, 169989), ]

# Influential Observations
# added variable plots 
av.Plots(lm1)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(users_sub)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(lm1,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(lm1, main="QQ Plot")
# distribution of studentized residuals

sresid <- studres(lm1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(lm1)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(lm1)


# Evaluate Collinearity
vif(lm1) # variance inflation factors 
vif(lm2)

bptest(lm3)

# Evaluate Nonlinearity
# component + residual plot 
crPlots(lm1)
# Ceres plots 
ceresPlots(lm1)

library(gvlma)
lm4 <- gvlma(lm3) 
summary(lm4)

# Test for Autocorrelated Errors
durbinWatsonTest(lm1)



