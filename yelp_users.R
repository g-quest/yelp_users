library(ggplot2)
library(readr)
library(visreg)
# read in users data into a data frame
usersDF <- read_csv('/Users/g_quest/Google Drive/Projects/yelp_users_analysis/yelp_academic_dataset_user.csv')

# show column names
names(usersDF)

# sanity check. get raw statistics with data untouched
summary(usersDF)

# initial modeling: multiple regression on compliment variables with review_count as dependent varibale
modelUsers <- lm(review_count ~ compliment_funny + compliment_writer + compliment_more + compliment_plain + 
                   compliment_cute + compliment_profile + compliment_photos + compliment_note + compliment_hot + 
                   compliment_cool + compliment_list + fans, 
                 data = usersDF)

summary(modelUsers)


