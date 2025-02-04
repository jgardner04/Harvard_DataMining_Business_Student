library(tidyverse)
library(fastDummies)
library(mlba)

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

# Get overview
str(housing.df)

# make REMODEL a factor variable
housing.df$REMODEL <- factor(housing.df$REMODEL)
str(housing.df$REMODEL)
levels(housing.df$REMODEL)

# use tidyverse to load and preprodcess data in one statement
housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL))


housing.df <- dummy_cols(mlba::WestRoxbury,
                         remove_selected_columns=TRUE,
                         remove_first_dummy=TRUE)
housing.df %>% head(2)

housing.df <- mlba::WestRoxbury %>%
  mutate(REMODEL=factor(REMODEL))

set.seed(1)

# Partition data into training and test sets
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.6)
# collect all the columnts with training row ID into training set
train.df <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into holdout
holdout.rows <- setdiff(rownames(housing.df), train.rows)
holdout.df <- housing.df[holdout.rows, ]

## partitioning into trianing (50%), validation (30%) and test (20%) sets
# randomly sample 50% of the rows
train.rows <- sample(rownames(housing.df), nrow(housing.df)*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records not already in the training set
# use setdiff() to find redords not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), nrow(housing.df)*0.3)

# assign the remianing 20% row IDs serve as holdout
holdout.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- housing.df[train.rows, ]
valid.df <- housing.df[valid.rows, ]
holdout.df <- housing.df[holdout.rows, ]

# partitioning into training (60%) and holdout (40%) using caret
set.seed(1)
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, p=0.6, list=FALSE)
train.df <- housing.df[idx, ]
holdout.df <- housing.df[-idx, ]

housing.df <- mlba::WestRoxbury %>%
  drop_na() %>%
  select(-TAX) %>%
  mutate(REMODEL=factor(REMODEL)) %>%
  dummy_cols(select_columns = c('REMODEL'),
             remove_selected_columns=TRUE,
             remove_first_dummy=TRUE)

reg <- lm(TOTAL.VALUE ~ ., data=train.df)
train.res <- data.frame(actural=train.df$TOTAL.VALUE, predicted=reg$fitted.values, residuals=reg$residuals)

head(train.res)

pred <- predict(reg, newdata=holdout.df)
holdout.res <- data.frame(actual=holdout.df$TOTAL.VALUE, predicted=pred, residual=holdout.df$TOTAL.VALUE-pred)
head(holdout.res)
