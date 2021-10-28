# A Classification Tree at Hogwarts
# Data Mining week 5

# SET UP------------------
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)


hogwarts <- read.csv("super_heroes_hogwarts_v3.csv", header = TRUE)
head(hogwarts, 10)
str(hogwarts)

hogwarts <- hogwarts[ , -c(1:8, 17, 19:26)]
names(hogwarts)

hogwarts$House <- as.factor(hogwarts$House)


# TRAINING VALIDATION SPLIT--------------
set.seed(666)

train_index <- sample(1:nrow(hogwarts), 0.6 * nrow(hogwarts))
valid_index <- setdiff(1:nrow(hogwarts), train_index)

train_df <- hogwarts[train_index, ]
valid_df <- hogwarts[valid_index, ]



# CLASSIFICATION TREE-------------
class_tr <- rpart(House ~ Manipulative + Resourceful + Dismissive + 
                    Intelligent + Trusting + Loyal + Stubborn + Brave,
                  data = train_df,
                  method = 'class',
                  maxdepth = 30)

# plotting the tree
prp(class_tr, cex = 0.8, tweak = 1)




# ALTERNATE SETTINGS---------------
class_tr_v2 <- rpart(House ~ Manipulative + Resourceful + Dismissive + 
                    Intelligent + Trusting + Loyal + Stubborn + Brave,
                  data = train_df,
                  method = 'class',
                  minbucket = 2,
                  maxdepth = 30)

prp(class_tr_v2, cex = 0.8, tweak = 1)



# CONFUSION MATRIX-------------
# training set
class_tr_train_predict <- predict(class_tr, train_df, type = "class")

confusionMatrix(class_tr_train_predict, train_df$House)


# validation set
class_tr_valid_predict <- predict(class_tr, valid_df, type = 'class')

confusionMatrix(class_tr_valid_predict, valid_df$House)


# PREDICTING NEW RECORD------------
oliver <- data.frame(Manipulative = 10,
                        Resourceful = 0,
                        Dismissive = 10,
                        Intelligent = 0,
                        Trusting = 0,
                        Loyal = 0,
                        Stubborn = 10,
                        Brave = 0)
oliver

oliver_house <- predict(class_tr, newdata = oliver)
oliver_house


# 2 CLASS SET UP-----------
library(car)

train_df_2 <- train_df
train_df_2$House <- recode(train_df_2$House,
                           " 'Gryffindor' = 'Not Slytherin'; 
                         'Ravenclaw' = 'Not Slytherin'; 
                         'Hufflepuff' = 'Not Slytherin'")
table(train_df_2$House)


valid_df_2 <- valid_df
valid_df_2$House <- recode(valid_df_2$House,
                           " 'Gryffindor' = 'Not Slytherin'; 
                         'Ravenclaw' = 'Not Slytherin'; 
                         'Hufflepuff' = 'Not Slytherin'")
table(valid_df_2$House)


# model
class_tr_slytherin <- rpart(House ~ Manipulative + Resourceful +
                              Dismissive + Intelligent + Trusting + Loyal +
                              Stubborn + Brave,
                            data = train_df_2, method = "class",  minbucket = 5,
                            maxdepth = 10)


prp(class_tr_slytherin, cex = 0.8, tweak = 1)

# confusion matrix
class_tr_train_slytherin_predict <- predict(class_tr_slytherin,
                                            train_df_2, type = "class")

confusionMatrix(class_tr_train_slytherin_predict, train_df_2$House,
                positive = "Slytherin")

class_tr_valid_slytherin_predict <- predict(class_tr_slytherin, 
                                            valid_df_2, type = "class")

confusionMatrix(class_tr_valid_slytherin_predict, valid_df_2$House,
                positive = "Slytherin")









