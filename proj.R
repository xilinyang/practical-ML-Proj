library(caret)
library(kernlab)
library(randomForest)



#read data 
raw_data <- read.csv("pml-training.csv", na.strings= c("NA",""," "))

# clean data
na_data <- apply(raw_data, 2, function(x) {sum(is.na(x))})
clean_data <- raw_data[,which(na_data == 0)]

# clean out irrelavent variables 
clean_train_data <- clean_data[8:length(clean_data)]


# create training set and cross validation set
train <- createDataPartition(y = clean_train_data$classe, p = 0.7, list = FALSE)
training_set <- clean_train_data[train, ]
crossvalidation_set <- clean_train_data[-train, ]


# fit a RF model  

#model <- train(training_set$classe ~ ., method = "rf",training_set)
model <- randomForest(classe ~ ., data = training_set)
# cross validation
Crossvalidation_predict <- predict(model, crossvalidation_set)
cm<-confusionMatrix(crossvalidation_set$classe, Crossvalidation_predict)

# test data prediction
raw_test <- read.csv("pml-testing.csv", na.strings= c("NA",""," "))
raw_test_na <- apply(raw_test, 2, function(x) {sum(is.na(x))})
test_clean1 <- raw_test[,which(raw_test_na == 0)]
test_clean <- test_clean1[8:length(test_clean1)]

test_pred <- predict(model, test_clean)