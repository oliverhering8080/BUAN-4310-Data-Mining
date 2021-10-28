# Regression Tree DnD
# Data mining Week 6

library(rpart)
library(rpart.plot)
library(forecast)
library(caret)

hogwarts <- read.csv("super_heroes_hogwarts_v3a.csv", header = TRUE)
head(hogwarts, 10)

str(hogwarts)

# removing unnecessary variables
hogwarts <- hogwarts[ , c(19:24, 26)]
names(hogwarts)
nrow(hogwarts)


# TRAINING VALIDATION SPLIT-----------
set.seed(666)

train_index <- sample(1:nrow(hogwarts), 0.6 * nrow(hogwarts)) 
valid_index <- setdiff(1:nrow(hogwarts), train_index)
  
train_df <- hogwarts[train_index, ]
valid_df <- hogwarts[valid_index, ]

nrow(train_df)
nrow(valid_df)

head(train_df)
head(valid_df)

# REGRESSION TREE----------
names(train_df)

regression_tree <- rpart(HP ~ .,
                         data = train_df, method = 'anova', maxdepth = 20)

prp(regression_tree)
rpart.plot(regression_tree)


# PREDICTION-------------
predict_train <- predict(regression_tree, train_df)
accuracy(predict_train, train_df$HP)

predict_valid <- predict(regression_tree, valid_df)
accuracy(predict_valid, valid_df$HP)



# SHALLOWER TREE-------------
regress_tr_shallow <- rpart(HP ~ STR + DEX + CON + INT + WIS + CHA,
                            data = train_df, method = "anova", 
                            minbucket = 2, maxdepth = 3)
prp(regress_tr_shallow)


predict_train_shallow <- predict(regress_tr_shallow, train_df)
accuracy(predict_train_shallow, train_df$HP)

predict_valid_shallow <- predict(regress_tr_shallow, valid_df)
accuracy(predict_valid_shallow, valid_df$HP)


# PREDICTING A NEW RECORD
padawan_1 <- data.frame(STR = 18, 
                        DEX = 18, 
                        CON = 18, 
                        INT = 18,
                        WIS = 18,
                        CHA = 18)


regress_tr_pred <- predict(regression_tree, newdata = padawan_1)
regress_tr_pred

















