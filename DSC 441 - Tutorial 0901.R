library(caret)
library(rpart)
library(tidyverse)
library(rattle)
library(ggplot2)
library(pROC)

data("Sacramento")
head(Sacramento)

set.seed(37)
index <- createDataPartition(y = Sacramento$type, p = 0.7, list = FALSE)
train_sc <- Sacramento[index, ]
test_sc <- Sacramento[-index, ]

train_control = trainControl(method = "cv", number = 10)
tree <- train(type ~ ., data = train_sc, method = "rpart", trControl = train_control)
tree

pred_sc <- predict(tree, test_sc)
cm <- confusionMatrix(test_sc$type, pred_sc)
cm

## Scoring Metrics
metrics <- as.data.frame(cm$byClass)
metrics

# Precision
metrics %>% select(c(Precision))

# Recall
metrics %>% select(c(Recall))

# Specificity
metrics %>% select(c(Specificity))

# F1 Score
metrics %>% select(c(F1))

# Balanced Accuracy
metrics %>% select(c(`Balanced Accuracy`))

## ROC Curves
library(mlbench)

data("PimaIndiansDiabetes")
head(PimaIndiansDiabetes)

str(PimaIndiansDiabetes$diabetes)

index = createDataPartition(y=PimaIndiansDiabetes$diabetes, p = 0.7, list = FALSE)
train_pima = PimaIndiansDiabetes[index, ]
test_pima = PimaIndiansDiabetes[-index, ]

train_control = trainControl(method = "cv", number = 10)
knn <- train(diabetes ~ ., data = train_pima, method = "knn", trControl = train_control, tuneLength = 20)
knn

pred_pima <- predict(knn, test_pima)
confusionMatrix(test_pima$diabetes, pred_pima)

pred_prob <- predict(knn, test_pima, type = "prob")
head(pred_prob)

roc_obj <- roc((test_pima$diabetes), pred_prob[, 1])
plot(roc_obj, print.auc = TRUE)

train_control = trainControl(method = "cv", number = 10)
dtree <- train(diabetes ~ ., data = train_pima, method = "rpart", trControl = train_control)
dtree

pred_pima2 <- predict(dtree, test_pima)
confusionMatrix(test_pima$diabetes, pred_pima2)

pred_prob2 <- predict(dtree, test_pima, type = "prob")
head(pred_prob2)

roc_obj2 <- roc((test_pima$diabetes), pred_prob2[, 1])
plot(roc_obj2, print.auc = TRUE)