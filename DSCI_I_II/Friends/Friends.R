library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read in data
foreveralone <- read.csv("foreveralone.csv", as.is = TRUE)

# Data from samples
gender <- foreveralone$gender # Numerical
friends <- foreveralone$friends # Categorical
social_fear <- foreveralone$social_fear # Categorical

# Print dataframe table
df <- data.frame(foreveralone, gender, friends, social_fear)
df

# New form
gender.1 <- df$gender.1
friends.1<- df$friends.1
social_fear.1 <- df$social_fear.1

# Number of Males and Females
Males <- gender.1 == "Male"
sum(Males)
Females <- gender.1 == "Female"
sum(Females)

# Outliers
outliersfriends <- friends > 50
friends[friends >50]

# Friends by Gender Boxplot
outlier = friends.1 > 50
df %>%
filter(!outlier) %>% 
ggplot(aes(gender.1, friends.1)) + geom_boxplot() + ggtitle("Friends by Gender Boxplot") +
  xlab("Gender") + ylab("Number of Friends")

# Number Surveyed by Gender
df %>%
ggplot(aes(gender.1)) + geom_bar() +ggtitle("Number Surveyed by Gender") + xlab("Gender") +
  ylab("Number Surveyed")

#Proportion by Gender of Social Fear
ggplot(df, aes(gender.1, fill=social_fear.1)) +
  geom_bar(position="fill") +
    ggtitle("Social Fear by Gender Bar Chart (Proportion)") + xlab("Gender") + ylab("% With Social Fear") +
    labs(fill = "Social Fear")

#Social Fear and # of Friends
friends.1 <- ifelse(friends.1 == 0, "0 Friends", ifelse(friends.1 > 10, 
                    "More Than 10 Friends", ifelse(friends.1 <= 5, "1-5 Friends", "6-10 Friends")))

df <- data.frame(foreveralone, gender.1, friends.1, social_fear.1)

ggplot(df, aes(social_fear.1)) +
  geom_bar() + facet_wrap(~friends.1) + ggtitle("Social Fear and Number of Friends") +
  xlab("Social Fear") + ylab("Number of People")