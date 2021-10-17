# Oliver Hering
# kNN Lab 
# Data Mining and Big Data Fall 2021


# library and reading data
library(caret)

data <- read.csv('UniversalBank.csv', header = TRUE)
head(data)

# looking at data 
str(data)
names(data)

# CLEANING DATA-------------------------
data <- data[, -c(1, 5)]
names(data)

# re ordering variables
data <- data[, c(1:7, 9:12, 8)]
names(data)


table(data$Personal.Loan)
str(data)

# setting categorical variables as factors

data$Education <-as.factor(data$Education)
data$Securities.Account <-as.factor(data$Securities.Account)
data$CD.Account <-as.factor(data$CD.Account) 
data$Online <-as.factor(data$Online) 
data$CreditCard <-as.factor(data$CreditCard) 

data$Personal.Loan <- factor(data$Personal.Loan,
                                levels = c('0', '1'),
                                labels = c('No', 'Yes'))
str(data)


# TRAINING AND VALIDATION SPLIT---------------
set.seed(666)

train_index <- sample(1:nrow(data), 0.6 * nrow(data))
valid_index <- setdiff(1:nrow(data), train_index)

train <- data[train_index, ]
valid <- data[valid_index, ]

str(train)
str(valid)


# NORMALIZING

train_norm <- train
valid_norm <- valid


# function
norm_function <- preProcess(train[, -c(6, 8:12)],
                            method = c('center',
                                       'scale'))
# train_norm
train_norm[, -c(6, 8:12)] <- predict(norm_function,
                                 train[, -c(6, 8:12)])

head(train_norm)
table(train_norm$Personal.Loan)

# valid_norm
valid_norm[, -c(6, 8:12)] <- predict(norm_function,
                                valid[, -c(6, 8:12)])

head(valid_norm)
table(valid_norm$Personal.Loan)

# THE KNN MODEL----------------

# k = 3
knn_model_3 <- caret::knn3(Personal.Loan ~ ., data = train_norm, k = 3)
knn_model_3


# prediction and evaluation - training
knn_3_predict_train <- predict(knn_model_3, newdata = train_norm[, -c(12)],
                             type = 'class')
table(knn_3_predict_train)

confusionMatrix(knn_3_predict_train, as.factor(train_norm[, 12]),
                positive = 'Yes')



# k = 5
knn_model_5 <- caret::knn3(Personal.Loan ~ ., data = train_norm, k = 5)
knn_model_5


# prediction and evaluation - training
knn_5_predict_train <- predict(knn_model_5, newdata = train_norm[, -c(12)],
                             type = 'class')
table(knn_5_predict_train)

confusionMatrix(knn_5_predict_train, as.factor(train_norm[, 12]))



# k = 7
knn_model_7 <- caret::knn3(Personal.Loan ~ ., data = train_norm, k = 7)
knn_model_7


# prediction and evaluation - training
knn_7_predict_train <- predict(knn_model_7, newdata = train_norm[, -c(12)],
                             type = 'class')
table(knn_7_predict_train)

confusionMatrix(knn_7_predict_train, as.factor(train_norm[, 12]))


# VALIDATION SET--------------
knn_3_predict_valid <- predict(knn_model_3, newdata = valid_norm[, -c(12)],
                               type = 'class')
table(knn_3_predict_valid)

confusionMatrix(knn_3_predict_valid, as.factor(valid_norm[, 12]),
                positive = 'Yes')

# ROS CURVE--------------
library(ROSE)

ROSE::roc.curve(valid_norm$Personal.Loan,
                knn_3_predict_valid)


# PREDICTION AND EVALUATION NEW CUSTOMER-------------

new_customer <- data.frame(Age = 40,
                            Experience = 10,
                             Income = 84,
                             Family = 2,
                             CCAvg = 2,
                             Education = 2,
                             Mortgage = 0,
                             Securities.Account = 0,
                             CD.Account = 0,
                             Online = 1,
                             CreditCard = 1)
new_customer

new_customer$Education <- as.factor(new_customer$Education)
new_customer$Securities.Account <-as.factor(new_customer$Securities.Account)
new_customer$CD.Account <-as.factor(new_customer$CD.Account) 
new_customer$Online <-as.factor(new_customer$Online)
new_customer$CreditCard <-as.factor(new_customer$CreditCard)

str(new_customer)

# normalizing new customer
new_customer_norm <- predict(norm_function, new_customer)

# prediction and evaluation
new_customer_predict <- predict(knn_model_3,
                                newdata = new_customer_norm,
                                type = 'class')
new_customer_predict


#INTERPRETATION------------                                                   
# knn = 3 gives the most accurate prediction
# accuracy of both train and valid sets are high indicating no presence of overfitting
# new customer will not accept the offer according to our model





