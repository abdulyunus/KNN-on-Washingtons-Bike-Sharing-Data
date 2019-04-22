
# Title   : Bike Sharing in Washington D.C. Demand Forecast
# Author  : Abdul Yunus
# Version : 1.2
# Date    : 21th April 2019
# Algorithm : K-Nearest Neighbour

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(caret)
packages(caTools)
packages(pROC)
packages(mlbench)

path <- "C:/Users/Abdul_Yunus/Desktop/Yunus_Personal/Learning/Case Study/JDA"

setwd(path)

# Import the day and Hour data

day_df <- read.csv('day.csv', header = TRUE)
hour_df <- read.csv('hour.csv', header = TRUE)


# Lets have a look at the data

head(day_df)
head(hour_df)

# hour_df data has same variables (columns) as day_df data except the additional 'hr' column.
# Lets check the structure of both the data sets.

str(day_df)
str(hour_df)

# There are some variables, that needs to be as factor. Lets convert some of numerical variable into factors into both the data.

# Convert the numeric variable into factor for day_df data
day_df$season <- as.factor(day_df$season)
day_df$yr <- as.factor(day_df$yr)
day_df$mnth <- as.factor(day_df$mnth)
day_df$holiday <- as.factor(day_df$holiday)
day_df$weekday <- as.factor(day_df$weekday)
day_df$workingday <- as.factor(day_df$workingday)
day_df$weathersit <- as.factor(day_df$weathersit)

# Convert the numeric variable into factor for hour_df data
hour_df$season <- as.factor(hour_df$season)
hour_df$yr <- as.factor(hour_df$yr)
hour_df$mnth <- as.factor(hour_df$mnth)
hour_df$holiday <- as.factor(hour_df$holiday)
hour_df$weekday <- as.factor(hour_df$weekday)
hour_df$workingday <- as.factor(hour_df$workingday)
hour_df$weathersit <- as.factor(hour_df$weathersit)
hour_df$hr <- as.factor(hour_df$hr)

# We can see that the dteday variable is chr variable in the data, it should be in the data format, lets convert it.

day_df$dteday <- as.Date(day_df$dteday)
hour_df$dteday <- as.Date(hour_df$dteday)


# Column 'instant' is simply the serial numbers, we can remove the column from both the data
day_df <- day_df[,-1]
hour_df <- hour_df[,-1]

# Fitting the Model
# I have observe that there are many categorical variable we have in the data which is labled.
# We can do the one-hot encoding on those variables.

# Lets do all this on the  hour_df data set as we are using this dataset for model building

# One Hot Encoding for season
hour_df$season_2 <- ifelse(hour_df$season ==2,1,0)
hour_df$season_3 <- ifelse(hour_df$season ==3,1,0)
hour_df$season_4 <- ifelse(hour_df$season ==4,1,0)


# One Hot Encoding for weathersit
hour_df$weathersit_2 <- ifelse(hour_df$weathersit ==2,1,0)
hour_df$weathersit_3 <- ifelse(hour_df$weathersit ==3,1,0)
hour_df$weathersit_4 <- ifelse(hour_df$weathersit ==4,1,0)


# Since we have created dummy variables for season, weathersit.
# Lets remove these variables from data and keep only respective dummy variables


final_df <- subset(hour_df, select = - c(season, weathersit))

# Since we have all information regarding date, month and time, we can remove the dteday column from the data
# Column cnt is the sum of  casual and registered, we can also remove these variables from our data.

final_df <- subset(final_df, select = -c(dteday, casual, registered))

# Data partitioning


set.seed(123)
id <- sample.split(Y = final_df$cnt, SplitRatio = 0.75)
train_df <- subset(final_df, id == "TRUE")
test_df <- subset(final_df, id == "FALSE")

# Before we made K nearest neighbor model, lets specify train COntrol, this will be used in the model
trControl = trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 3)

set.seed(123)
# #knn1 = train(cnt ~., data = train_df,
#             method = 'knn',
#             tuneLength = 20,
#             trControl = trControl,
#             tuneGrid = expand.grid(k = 1:60))

# The tuneLength parameter tells the algorithm to try different default values for the main parameter
# In this case we used 20 default values

# The tuneGrid parameter lets us decide which values the main parameter will take
# While tuneLength only limit the number of default parameters to use.

knn1
plot(knn1)

varImp(knn1)

attributes(knn1)
knn1$call
knn1$finalModel
# Final KNN Model will be using K = 3



# Lets predict the result using the train and test data set and then check the RMSE.

pred_knn_train = predict(object = knn1, newdata = train_df)
pred_knn_test = predict(object = knn1, newdata = test_df)

# Since we are dealing with numerical variable prediction (Regression problem) therefore, now we find the RMSE for accuracy. We will not do the confusion matrix here

train_df_Val <- cbind(train_df,pred_knn_train )
test_df_Val <- cbind(test_df,pred_knn_test )


# On Train data - Error calculation and check if there is any negative value.
train_df_Val$NegativeVal <- ifelse(train_df_Val$pred_knn_train < 0,1,0)
sum(train_df_Val$NegativeVal)

train_df_Val$Error <- train_df_Val$cnt - train_df_Val$pred_knn_train

RMSE_train_knn1 <- sum(sqrt(mean((train_df_Val$Error)^2)))

MAD_train_knn1  <- sum(abs(train_df_Val$pred_knn_train - mean(train_df_Val$pred_knn_train, na.rm = T))/length(train_df_Val$pred_knn_train))
MAPE_train_knn1 <- mean(abs((train_df_Val$cnt - train_df_Val$pred_knn_train)/train_df_Val$cnt)*100)

# On Test data - Error calculation and check if there is any negative value.
test_df_Val$NegativeVal <- ifelse(test_df_Val$pred_knn_test < 0,1,0)
sum(test_df_Val$NegativeVal)



test_df_Val$Error <- test_df_Val$cnt - test_df_Val$pred_knn_test

RMSE_test_knn1 <- sum(sqrt(mean((test_df_Val$Error)^2)))

MAD_test_knn1  <- sum(abs(test_df_Val$pred_knn_test - mean(test_df_Val$pred_knn_test, na.rm = T))/length(test_df_Val$pred_knn_test))
MAPE_test_knn1 <- mean(abs((test_df_Val$cnt - test_df_Val$pred_knn_test)/test_df_Val$cnt)*100)

#RMSE_train_knn <- RMSE(pred = pred_knn_train, train_df$cnt)
#RMSE_test_knn <- RMSE(pred = pred_knn_test, test_df$cnt)

Accuracy_table_RMSE <- data.frame(RMSE_train_knn1,RMSE_test_knn1)
Accuracy_table_MAD <- data.frame(MAD_train_knn1,MAD_test_knn1)
Accuracy_table_RMSE
Accuracy_table_MAD
