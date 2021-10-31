# Decision Tree Lab
# BUAN 4310 Data Mining Fall 2021
# ----------------------------------------------------------------------------

library(dplyr)
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)

cars <- read.csv("ToyotaCorolla.csv", header = TRUE)
head(cars)
str(cars)

# REMOVING UNNEEDED VARIABLES and reording----------------------
t(t(names(cars)))

cars <- cars[, c(4, 7, 8, 9, 12, 14, 17, 19, 21, 25, 26, 28, 30, 34, 39, 3)]
t(t(names(cars)))

cars$Fuel_Type <- as.factor(cars$Fuel_Type)


# TRAINING VALIDATION SPLIT-------------------------
set.seed(666)

train_index <- sample(1:nrow(cars), 0.6 * nrow(cars))
valid_index <- setdiff(1:nrow(cars), train_index)

train_df <- cars[train_index, ]
valid_df <- cars[valid_index, ]

nrow(train_df)
nrow(valid_df)

str(train_df)
head(valid_df)


# REGRESSION TREE--------------------
names(train_df)

regression_tree <- rpart(Price ~ .,
                         data = train_df, method = 'anova', maxdepth = 20)

prp(regression_tree)
rpart.plot(regression_tree)


# EVALUATING MODEL-----------------------
predict_train <- predict(regression_tree, train_df)
accuracy(predict_train, train_df$Price)

predict_valid <- predict(regression_tree, valid_df)
accuracy(predict_valid, valid_df$Price)


# PREDICTING THE PRICE OF A NEW RECORD-----------------------
new_record <- data.frame(Age_08_04 = 77, 
                         KM = 117000, 
                         Fuel_Type = "Petrol", 
                         HP = 110, 
                         Automatic = 0, 
                         Doors = 5, 
                         Quarterly_Tax = 100, 
                         Mfr_Guarantee = 0, 
                         Guarantee_Period = 3, 
                         Airco = 1, 
                         Automatic_airco = 0, 
                         CD_Player = 0, 
                         Powered_Windows = 0, 
                         Sport_Model = 0, 
                         Tow_Bar = 1)

new_record_pred <- predict(regression_tree, newdata = new_record)
new_record_pred
# $7951.2 is the estimated Price


# CLASSIFICATION TREE-----------------------
cars2 <- cars
cars2$cat_price <- ifelse(cars2$Price <= mean(cars2$Price, na.rm = TRUE), '0', '1')

cars2$cat_price <- as.factor(cars2$cat_price)
str(cars2)

cars2 <- cars2[, -c(16)]
#training and valid split
train_index <- sample(1:nrow(cars2), 0.6 * nrow(cars2))
valid_index <- setdiff(1:nrow(cars2), train_index)

train_df_cat <- cars2[train_index, ]
valid_df_cat <- cars2[valid_index, ]

nrow(train_df_cat)
nrow(valid_df_cat)

str(train_df_cat)
str(valid_df_cat)


class_tree <- rpart(cat_price ~ ., data = train_df_cat, method = 'class', maxdepth = 30)

prp(class_tree, cex = 0.8, tweak = 1)

# confusion matrix
class_tr_train_predict <- predict(class_tree, train_df_cat, type = 'class')

confusionMatrix(class_tr_train_predict, train_df_cat$cat_price)

# predicting new record
new_record_pred_cat <- predict(class_tree, newdata = new_record)
new_record_pred_cat


# This tree predicted that the price would be low, which is accurate considering it was on the lower side. 
# Both trees started with age as their first node. Age seems to be a huge predicting factor on the price.
# The predictions were similar. This new record seemed to be on the cheaper side. 

# If I was running a business, I would likely use the regression tree as it just provides more information 
# and point estimates are nice to have. In a business like this (car sales), it's nice for the customer to
# see an actual dollar value estimate for the car.
















