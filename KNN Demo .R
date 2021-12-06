# kNN Demo Script---------------------------------------------


library(caret)

# the data
hogwarts <- read.csv('super_heroes_hogwarts_v3a.csv', header = TRUE)
head(hogwarts, 10)

# checking data type
str(hogwarts)

# column names
names(hogwarts)

# number of rows
nrow(hogwarts)


# removing unwanted columns
hogwarts <- hogwarts[ , -c(1:8, 17, 19:26)]
names(hogwarts)


# looking at the variables
t(t(names(hogwarts)))

str(hogwarts)

# exploring the House variable
table(hogwarts$House)

# Set House as a factor---------------------------
hogwarts$House <- as.factor(hogwarts$House)

# Training Validation split--------------------------
set.seed(666)

# 70/30 split
train_index <- sample(1:nrow(hogwarts), 0.7 * nrow(hogwarts))
valid_index <- setdiff(1:nrow(hogwarts), train_index)

# creating the training and validation sets
train_df <- hogwarts[train_index, ]
valid_df <- hogwarts[valid_index, ]

nrow(train_df)
nrow(valid_df)

str(train_df)
str(valid_df)


# Entering in a new record-----------------------

padawan_1 <- data.frame(Manipulative = 8,
                        Resourceful = 9,
                        Dismissive = 8,
                        Intelligent = 9,
                        Trusting = 6,
                        Loyal = 8,
                        Stubborn = 6,
                        Brave = 7)



padawan_1


# Normalization---------------------

train_norm <- train_df
valid_norm <- valid_df

names(train_df)

# creating the normalizing algorithm
norm_values <- preProcess(train_df[, -c(9)],
                          method = c('center',
                                     'scale'))

# normalizing the training set
train_norm[, -c(9)] <- predict(norm_values,
                               train_df[, -c(9)])

head(train_norm)
table(train_norm$House)

# normalizing the validation set
valid_norm[, -c(9)] <- predict(norm_values,
                               valid_df[, -c(9)])

head(valid_norm)


# normalizing the new record
padawan_1_norm <- predict(norm_values, padawan_1)
padawan_1_norm


# THE KNN MODEL-------------------------------------------

knn_model <- caret::knn3(House ~ ., data = train_norm, k = 5)
knn_model



# THE PREDICTION---------------------
# training set 
knn_predict_train <- predict(knn_model, newdata = train_norm[, -c(9)],
                             type = 'class')

head(knn_predict_train)

# checking the model accuracy
confusionMatrix(knn_predict_train, as.factor(train_norm[, 9]))



# validation set
knn_prediction_valid <- predict(knn_model, newdata = valid_norm[, -c(9)],
                                type = 'class')

head(knn_prediction_valid)

# checking accuracy
confusionMatrix(knn_prediction_valid, as.factor(valid_norm[, 9]))



#predicting the new padawan
padawan_predict <- predict(knn_model, newdata = padawan_1_norm,
                           type = 'class')
padawan_predict

#predicting myself
oliver <- data.frame(Manipulative = 3,
                        Resourceful = 8,
                        Dismissive = 7,
                        Intelligent = 10,
                        Trusting = 5,
                        Loyal = 7,
                        Stubborn = 4,
                        Brave = 10)

oliver_norm <- predict(norm_values, oliver)

oliver_predict <- predict(knn_model, newdata = oliver_norm,
                          type = 'class')
oliver_predict

oliver_predict_prob <- predict(knn_model,
                               newdata = oliver_norm,
                               type = 'prob')
oliver_predict_prob

# the probability
padawan_predict_prob <- predict(knn_model,
                                newdata = padawan_1_norm,
                                type = 'prob')
padawan_predict_prob



# SIMPLIFYING THE MODEL INTO 2 CLASSES-------------------------
install.packages('car')
library(car)

# recoding the values
train_norm_2 <- train_norm

train_norm_2$House <- recode(train_norm_2$House,
                             " 'Gryffindor' = 'Not Slytherin'; 
                         'Ravenclaw' = 'Not Slytherin'; 
                         'Hufflepuff' = 'Not Slytherin'")
table(train_norm_2$House)


valid_norm_2 <- valid_norm

valid_norm_2$House <- recode(valid_norm_2$House,
                             " 'Gryffindor' = 'Not Slytherin'; 
                         'Ravenclaw' = 'Not Slytherin'; 
                         'Hufflepuff' = 'Not Slytherin'")
table(valid_norm_2$House)


# Training the new model
knn_model_k7_2 <- caret::knn3(House ~ ., data = train_norm_2, k = 7)
knn_model_k7_2

# predicting the Training Set
knn_pred_k7_train_2 <- predict(knn_model_k7_2, newdata = train_norm_2[, -c(9)], type = "class")
head(knn_pred_k7_train_2)

# confusion matrix 
confusionMatrix(knn_pred_k7_train_2, as.factor(train_norm_2[, 9]),
                positive = 'Slytherin')

# predicting the Validation Set
knn_pred_k7_valid_2 <- predict(knn_model_k7_2, newdata = valid_norm_2[, -c(9)], type = "class")
head(knn_pred_k7_valid_2)

confusionMatrix(knn_pred_k7_valid_2, as.factor(valid_norm_2[, 9]),
                positive = 'Slytherin')

# predicting myself
oliver_predict_2 <- predict(knn_model_k7_2,
                            newdata = oliver_norm,
                            type = 'prob')
oliver_predict_2


# EVALUATING THE MODEL---------------------
library(ROSE)

ROSE::roc.curve(valid_norm_2$House, knn_pred_k7_valid_2)



