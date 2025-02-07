# HW 2 ggplot viz
# Sources https://www.kaggle.com/datasets/julienjta/twitter-mentions-volumes
# https://www.kaggle.com/datasets/jpmiller/employee-attrition-for-healthcare

# Directions: Please follow the commented instructions to create various visualizations.  Then turn in your R script

# Libraries
library(readr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)

# Obtain the data from the repo
healthcare <- read_csv('/Users/jogardn/git/Harvard_DataMining_Business_Student/HW/HW II/watson_healthcare_modified.csv')
twitterVol <- read_csv('/Users/jogardn/git/Harvard_DataMining_Business_Student/HW/HW II/twitter volume.csv')

# Get to know your data by calling summary and head on the data sets
summary(healthcare)
head(healthcare)
summary(twitterVol)
head(twitterVol)

#1.
# Make a ggplot box plot with healthcare
# Use aes x = HourlyRate and fill = Attrition with a layer 
# add the layer geom_boxplot() 
jpeg('/Users/jogardn/git/Harvard_DataMining_Business_Student/personalFiles/healthcareBarplot.jpg')
ggplot(data=healthcare, aes(x = HourlyRate, fill = Attrition)) + geom_boxplot()
dev.off()
# Is there a difference between hourly wage for people that leave versus stay?  You may have to lookup how to interpret a boxplot

# ANSWER: The median Hourly Rate of people who stay is higher than those who leave.

#2.
# Build a ggplot2 density plot with healthcare
# Use aes x = TotalWorkingYears and fill = 'red'
# Add a layer geom_density
# Add a layer with theme_gdocs()
density_data <- density(healthcare$TotalWorkingYears)
mode_index <- which.max(density_data$y)
mode_x <- density_data$x[mode_index]
mode_y <- density_data$y[mode_index]

ggplot(data = healthcare, aes(x = TotalWorkingYears, fill = 'red')) + 
  geom_density() + 
  theme_gdocs() +
  geom_point(aes(x = mode_x, y = mode_y), color = "blue", size = 3) +
  annotate("text", x = mode_x, y = mode_y, label = paste("Mode:", round(mode_x, 2)), vjust = -1)

# What do you observe?  What is the average tenure?

# ANSWER: The average tenure is 9.2 years.

# Now call median() on the TotalWorkingYears column of the data 
median(healthcare$TotalWorkingYears)
# Are they similar to the what the visual is telling you?

# ANSWER: The median returns as 10 years. There are extremes in the data that skew the calculations. 

#3.
# Rebuild the density plot but add fill = Attrition in the aes() call
# add a geom_density() layer with alpha = 0.5 inside
# Add a layer with theme_gdocs()
# Add a title with ggtitle("Work Yrs distribution by Attrition")
ggplot(data = healthcare, aes(x = HourlyRate, fill = Attrition, alpha = 0.5)) + 
  geom_density(alpha = 0.5) + 
  theme_gdocs() + 
  ggtitle("Work Yrs distribution by Attrition")

# What do you observe inthe visual, is there a difference in tenure and total working years?

# ANSWER: Employees who are payed more stay with the company longer.

# Can you calculate the median by the group?  There are multiple ways to do this, one can subset, or use group_by from dplyr.

median_by_group <- healthcare %>%
  group_by(Attrition) %>%
  summarise(median_TotalWorkingYears = median(TotalWorkingYears, na.rm = TRUE))
print(median_by_group)

#   Attrition median_TotalWorkingYears
#   <chr>                        <dbl>
# 1 No                              10
# 2 Yes                              5

#4.
# Create a data frame called df using data.frame(table(dataSet$columnName)) referring to the MaritalStatus
df <- data.frame(table(healthcare$MaritalStatus))

# Create a geom_col bar chart with the df object.
# If done properly the aes should be x = Var1 and y = Freq
# Add theme_hc()
ggplot(df, aes(x = Var1, y = Freq)) + 
  geom_col() + 
  theme_hc()

# What is the most common marital status?

# ANSWER: The most common marital status is Married.

#5.
# Rebuild df as a data.frame, with a two-way tally with  MaritalStatus & Attrition like this: table(healthcare$MaritalStatus, healthcare$Attrition)
df <- data.frame(table(healthcare$MaritalStatus, healthcare$Attrition))

# Examine the df object by calling it in your console and notice the difference between the original 1-way tally
print(df)

# Build a stacked ggplot bar chart using the new df data
# aes should be x = Var1, y = Freq, fill = Var2
# add a layer geom_bar with position = 'stack' and stat = 'identity'
# add a layer theme_hc()

ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(position = 'stack', stat = 'identity') +
  theme_hc()

# What do you observe with attriting employees and the relationship to martical status? 

# ANSWER: Single employees are more likely to leave the company.

#6.
# Create an object twitter100 by applying head() with the additional parameter 100 to get the first 100 rows, or you can use indexing
twitter100 <- head(twitterVol,100)
# Use the cor() function on two columns, $Apple, and $Salesforce noting the correlation between twitter volume within this data
cor(twitter100$Apple, twitter100$Salesforce)

# Using twitter100, create a ggplot scatter plot
# aes should be x = Apple, y = Salesforce, and alpha = 0.5
# the geom_point layer
# make it theme_few
# add a smoothing line with geom_smooth with parameter method = 'lm' 
# to help the audience understand the relationship
ggplot(twitter100, aes(x = Apple, y = Salesforce, alpha = 0.5)) +
  geom_point() +
  theme_few() + 
  geom_smooth(method = "lm")

# Describe the relationship either positive or negative of the tweet volumes for these two companies?

# ANSWER: The Relationship is positive but the relationship is weak. There are a few outliers and a few data clusters. 

#7.
# Create a ggplot timeline of the apple twitter volume
# aes should be x = timestamp and  y = Apple
# Add a layer of geom_smooth()
# Add a smoothing line with method = 'auto' within geom_smooth()
# Add a theme theme_fivethirtyeight layer
# add a title with ggtitle and 'apple twitter volume'
ggplot(twitter100, aes(x = timestamp, y = Apple)) +
  geom_line() +
  geom_smooth(method = 'auto') +
  theme_fivethirtyeight() +
  ggtitle('apple twitter volume')

#8.
# First look at the twitter100 data with head()
head(twitter100)

# Next execute the code scaffold to pivot the data longer 
longTwitterVolume <- twitter100 %>%
  pivot_longer(!timestamp, names_to = "company", values_to = "twitterVolume")

# Review the change with head(); notice the data is the same just rearranged
head(longTwitterVolume)

# subset the data using the code scaffold below to just Facebook and Google
twoCompanies <- subset(longTwitterVolume, longTwitterVolume$company == 'Facebook' | longTwitterVolume$company == 'Google')

# Plot the two lines with ggplot and the twoCompanies data
# aes should be x = timestamp, y = twitterVolume, group = company and color = company
# add a geom_line()
# make the theme gdocs 
ggplot(twoCompanies, 
       aes(x = timestamp, y = twitterVolume, group = company, color = company)) +
  geom_line() +
  theme_gdocs()

# End
