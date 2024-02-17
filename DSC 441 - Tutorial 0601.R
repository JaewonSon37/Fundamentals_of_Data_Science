## Decision Tree Tuning
# Parameter Space Outcomes
library(caret)
library(rpart)
library(tidyverse)
library(ggplot2)
library(rattle)

head(diamonds)

nrow(diamonds)

set.seed(94)
train_control = trainControl(method = "cv", number = 10)
diamonds$cut <- as.factor(diamonds$cut)
tree1 <- train(cut ~., data = diamonds, method = "rpart1SE", trControl = train_control)
tree1

pred_tree <- predict(tree1, diamonds)
confusionMatrix(diamonds$cut, pred_tree)

fancyRpartPlot(tree1$finalModel, caption = "")

train_control = trainControl(method = "cv", number = 10)
tree_caret <- train(cut ~., data = diamonds, method = "rpart", trControl = train_control)
tree_caret

tree2 <- train(cut ~., data = diamonds, control = rpart.control(maxdepth = 3),
               trControl = train_control, method = "rpart1SE")
tree2

pred_tree2 <- predict(tree2, diamonds)
confusionMatrix(diamonds$cut, pred_tree2)

fancyRpartPlot(tree2$finalModel, caption = "")

hypers = rpart.control(minsplit =  5000, maxdepth = 4, minbucket = 2500)
tree2 <- train(cut ~., data = diamonds, control = hypers, trControl = train_control, method = "rpart1SE")
pred_tree2 <- predict(tree2, diamonds)
confusionMatrix(diamonds$cut, pred_tree2)

fancyRpartPlot(tree2$finalModel, caption = "")

# Test vs Train Performances
index = createDataPartition(y = diamonds$cut, p = 0.7, list = FALSE)
train_set = diamonds[index, ]
test_set = diamonds[-index, ]

tree1 <- train(cut ~., data = train_set, method = "rpart1SE", trControl = train_control)
pred_tree <- predict(tree1, test_set)
confusionMatrix(test_set$cut, pred_tree)

fancyRpartPlot(tree1$finalModel, caption = "")

hypers = rpart.control(minsplit =  5000, maxdepth = 4, minbucket = 2500)
tree2 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")
pred_tree <- predict(tree2, test_set)
confusionMatrix(test_set$cut, pred_tree)

fancyRpartPlot(tree2$finalModel, caption = "")

## General Model Comparison & Visualization
train_control = trainControl(method = "cv", number = 10)

hypers = rpart.control(minsplit =  2, maxdepth = 1, minbucket = 2)
tree1 <- train(cut ~., data = train_set, control = hypers,
               trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree1, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree1, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree1$finalModel$frame)

comp_tbl <- data.frame("Nodes" = nodes, "TrainAccuracy" = a_train, "TestAccuracy" = a_test,
                       "MaxDepth" = 1, "Minsplit" = 2, "Minbucket" = 2)

hypers = rpart.control(minsplit =  5, maxdepth = 2, minbucket = 5)
tree2 <- train(cut ~., data = train_set, control = hypers, 
               trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree2, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree2, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree2$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 2, 5, 5))

hypers = rpart.control(minsplit = 50, maxdepth = 3, minbucket = 50)
tree4 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree4, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree4, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree4$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 3, 50, 50))

hypers = rpart.control(minsplit = 100, maxdepth = 4, minbucket = 100)
tree7 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree7, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree7, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree7$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 100, 100))

hypers = rpart.control(minsplit = 1000, maxdepth = 4, minbucket = 1000)
tree8 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree8, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree8, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree8$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 1000, 1000))

hypers = rpart.control(minsplit = 5000, maxdepth = 8, minbucket = 5000)
tree10 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree10, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree10, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree10$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 8, 5000, 5000))

hypers = rpart.control(minsplit = 10000, maxdepth = 25, minbucket = 10000)
tree11 <- train(cut ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

pred_tree <- predict(tree11, train_set)
cfm_train <- confusionMatrix(train_set$cut, pred_tree)

pred_tree <- predict(tree11, test_set)
cfm_test <- confusionMatrix(test_set$cut, pred_tree)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree11$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 25, 10000, 10000))

comp_tbl

ggplot(comp_tbl, aes(x = Nodes)) + 
  geom_point(aes(y = TrainAccuracy), color = "red") + 
  geom_point(aes(y = TestAccuracy), color = "blue") +
  ylab("Accuracy")

ggplot(comp_tbl, aes(x = Nodes)) + 
  geom_line(aes(y = TrainAccuracy), color = "red") + 
  geom_line(aes(y = TestAccuracy), color = "blue") +
  ylab("Accuracy")

## Feature Selection
# Relevance analysis (variable importance score)
colnames(diamonds)

tree1 <- train(cut ~., data = train_set, method = "rpart1SE", trControl = train_control)
var_imp <- varImp(tree1, scale = FALSE)
var_imp

importance <- varImp(tree1, scale = FALSE)
print(importance)

plot(importance)

# Recursive feature elimination
library(mlbench)
library(randomForest)

data("PimaIndiansDiabetes")

index = createDataPartition(y=PimaIndiansDiabetes$diabetes, p = 0.7, list = FALSE)
train_set = PimaIndiansDiabetes[index, ]
test_set = PimaIndiansDiabetes[-index, ]

control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(x = PimaIndiansDiabetes[,1:8], y = PimaIndiansDiabetes[,9], sizes = c(1:8), rfeControl = control)
print(results)

predictors(results)

plot(results)