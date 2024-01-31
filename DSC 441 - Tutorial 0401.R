## SVM
# Straightforward SVM Training
head(iris)

str(iris)

library(caret)
library(e1071)

svm1 <- train(Species ~., data = iris, method = "svmLinear")
svm1

# Setting the parameters
train_control = trainControl(method = "cv", number = 10)
preproc = c("center", "scale")
svm2 <- train(Species ~., data = iris, method = "svmLinear", 
              trControl = train_control, preProcess = preproc)
svm2

## Evaluation
# Cross Validation & LOOXV
train_control_cv = trainControl(method = "cv", number = 10)
svm3 <- train(Species ~., data = iris, method = "svmLinear", 
              trControl = train_control_cv)
svm3

train_control_loocv = trainControl(method = "LOOCV", number = 10)
svm4 <- train(Species ~., data = iris, method = "svmLinear",
              trControl = train_control_loocv)
svm4

folds <- 10
idx <- createFolds(iris$Species, folds, returnTrain = T)
train_control_strat <- trainControl(index = idx, method = 'cv', number = folds)
svm_strat <- train(Species ~., data = iris, method = "svmLinear",
                   trControl = train_control_strat)
svm_strat

# Segmentation for testing
set.seed(123)
index = createDataPartition(y=iris$Species, p = 0.7, list = FALSE)
train_set = iris[index,]
test_set = iris[-index,]
svm_split <- train(Species ~., data = train_set, method = "svmLinear")
pred_split <- predict(svm_split, test_set)
sum(pred_split == test_set$Species) / nrow(test_set)

train_control_boot = trainControl(method = "boot", number = 100)
svm5 <- train(Species ~., data = iris, method = "svmLinear", 
              trControl = train_control_boot)
svm5

# Confusion Matrix
pred_split <- predict(svm_split, test_set)
confusionMatrix(test_set$Species, pred_split)

pred_split_train <- predict(svm_split, train_set)
confusionMatrix(train_set$Species, pred_split_train)

## Grid search
grid <- expand.grid(C = 10 ^ seq(-5,2,0.5))
svm_grid <- train(Species ~., data = iris, method = "svmLinear", 
                  trControl = train_control, tuneGrid = grid)
svm_grid
