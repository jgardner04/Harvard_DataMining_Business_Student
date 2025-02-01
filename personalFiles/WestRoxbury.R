housing.df <- read.csv('/Users/jogardn/git/Harvard_DataMining_Business_Student/BookDataSets/WestRoxbury.csv')

dim(housing.df)
head(housing.df)
View(housing.df)

housing.df[1:10, 1]
housing.df[1:10, ]
housing.df[5, 1:10]
housing.df[5, c(1:2, 4, 8:10)]
housing.df[, 1]
housing.df$TOTAL.VALUE
housing.df$TOTAL.VALUE[1:10]
length(housing.df$TOTAL.VALUE)
mean(housing.df$TOTAL.VALUE)
summary(housing.df)

# Random sample of 5 observations
s <- sample(row.names(housing.df), 5)
housing.df[s, ]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob=ifelse(housing.df$ROOMS> 10, 0.9, 0.01))
housing.df[s, ]

# rebalance
housing.df$REMODEL <- factor(housing.df$REMODEL)
table(housing.df$REMODEL)
upsampled.df <- caret::upSample(housing.df, housing.df$REMODEL, list=TRUE)$x
table(upsampled.df$REMODEL)

library(tidyverse)

# Get overview
str(housing.df)

# make REMODEL a factor variable
housing.df$REMODEL <- factor(housing.df$REMODEL)
str(housing.df$REMODEL)
levels(housing.df$REMODEL)

# use tidyverse to load and preprodcess data in one statement
housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL))

library(fastDummies)

housing.df <- dummy_cols(mlba::WestRoxbury,
                         remove_selected_columns=TRUE,
                         remove_first_dummy=TRUE)
housing.df %>% head(2)