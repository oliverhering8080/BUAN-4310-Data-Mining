# Linear Regression Lab
# Oliver Hering
# BUAN 4310 Data Mining

airfares <- read.csv("Airfares.csv", header = TRUE)
str(airfares)
head(airfares, 10)
t(t(names(airfares)))

# REMOVING VARIABLES
airfares <- airfares[, -c(1:4)]

# TRAINING VALIDATION SPLIT
set.seed(666)

train_index <- sample(1:nrow(airfares), 0.6 * nrow(airfares))
valid_index <- setdiff(1:nrow(airfares), train_index)

train_df <- airfares[train_index, ]
valid_df <- airfares[valid_index, ]

nrow(train_df)
nrow(valid_df)

t(t(names(train_df)))
t(t(names(valid_df)))

str(train_df)
# EXPLORATORY
# corrgram
install.packages("corrgram")

library(corrgram)
corrgram(train_df)

#ggpubr package installation and libraries
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library(ggplot2)
library(ggpubr)

# Distance vs. Fare scatterplot
ggplot(data = train_df) + aes(x = DISTANCE, y = FARE) + 
  geom_point() + 
  ggtitle("Scatter Plot of Fare vs Distance") + 
  geom_smooth(method=lm, se=TRUE) + 
  stat_cor(method = "pearson", label.x = 2000, label.y = 3.8)


# Linear Regression Model
airfare_model <- lm(FARE ~ ., data = train_df)
summary(airfare_model)

# Strong model, it is signifcant due to very low p value.
# VACATIONYes, SWYes, HI, S_INCOME, E_INCOME, S_POP, E_POP, 
# SLOTFree, GATEFree, DISTANCE, and PAX are all significant.
# DISTANCE does show to be the most significant due to lowest p.


# Predicting valid_df
library(forecast)

airfare_model_prediction <- predict(airfare_model, valid_df)
accuracy(airfare_model_prediction, valid_df$FARE)
sd(valid_df$FARE)

# RMSE is less than the st dev of the output variable indicating strong model.


# NEW RECORD
new_record <- data.frame(COUPON = 1.202, NEW = 3,  
                         VACATION = "Yes", SW = "Yes",  
                         HI = 4442.141, S_INCOME = 28760,  
                         E_INCOME = 27664, S_POP = 4557004,  
                         E_POP = 3195503, SLOT = "Free",  
                         GATE = "Free", PAX = 12782,  
                         DISTANCE = 1976) 

new_record_predication <- predict(airfare_model, new_record)
new_record_predication

# New record predicted fare price is $171.70

# Some of the information used may be private or changing in
# a real world context. It may be hard to get access to some 
# of this information before the flight, or it might change
# as gates and flights are always changing and being rescheduled.
# DISTANCE is a pretty objective and flight-specific measurement 
# though so it would generally be available and accurate to use. 




