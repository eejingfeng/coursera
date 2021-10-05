library(ggplot2)
library(caret)
library(randomForest)

train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

train$classe <- as.factor(train$classe)
test$classe <- as.factor(test$classe)

head(train)
summary(train)
dim(train)

# Removing columns with NA and columns not relevant to analysis 
good <- complete.cases(t(train)) & complete.cases(t(test))
good[c(1,3,4,5,6)] <- FALSE
train <- train[,good]
test <- test[,good]

head(train)
summary(train)
dim(train)

set.seed(13)
trainset <- createDataPartition(train$classe, p=0.6, list=FALSE)
validation <- train[-trainset,]
train <- train[trainset,]

randomforestmod <- randomForest(classe~., data=train)

trainpred <- predict(randomforestmod,newdata=train)
trainacc <- sum(trainpred == train$classe)/dim(train)[1]
paste("Accuracy on training set =",trainacc )

validpred <- predict(randomforestmod,newdata=validation)
validacc <- sum(validpred == validation$classe)/dim(validation)[1]
paste("Accuracy on validation set =",validacc )

testpred <- predict(randomforestmod,newdata=test)
paste("Classifications on test set:");testpred
