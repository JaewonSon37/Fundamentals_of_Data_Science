# Problem 1
breast_cancer <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 5\\Data File\\breast_cancer_updated.csv")
head(breast_cancer)

breast_cancer <- subset(breast_cancer, select = - IDNumber)
breast_cancer <- na.omit(breast_cancer)
sum(is.na(breast_cancer))

# Problem 1.a
library(caret)

set.seed(37)
train_control <- trainControl(method = "cv", number = 10)
tree1 <- train(Class ~., data = breast_cancer, method = "rpart", trControl = train_control)
tree1

# Problem 1.b
library(rattle)

fancyRpartPlot(tree1$finalModel, caption = "")

# Problem 1.c
library(rpart.plot)

rules <- rpart.rules(tree1$finalModel)
rules

# Problem 2
library(dplyr)

head(storms)

storms <- na.omit(storms)
sum(is.na(storms))

storms <- subset(storms, select = - name)
storms$category <- as.factor(storms$category)

# Problem 2.a
set.seed(38)
train_control <- trainControl(method = "cv", number = 10)
hypers = rpart.control(minsplit =  5, maxdepth = 2, minbucket = 3)
tree2 <- train(category ~., data = storms, control = hypers,
               trControl = train_control, method = "rpart1SE")
tree2

# Problem 2.b
set.seed(39)
index <- createDataPartition(y = storms$category, p = 0.7, list = FALSE)
train_set <- storms[index, ]
test_set <- storms[-index, ]

tree3 <- train(category ~., data = train_set, control = hypers,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree3, train_set)
confusionMatrix(train_set$category, pred_train)

pred_test <- predict(tree3, test_set)
confusionMatrix(test_set$category, pred_test)

# Problem 3.a
set.seed(40)
index2 <- createDataPartition(y = storms$category, p = 0.8, list = FALSE)
train_set2 <- storms[index2, ]
test_set2 <- storms[-index2, ]

# Problem 3.b
train_control = trainControl(method = "cv", number = 10)

hypers4 = rpart.control(minsplit =  2, maxdepth = 1, minbucket = 2)
tree4 <- train(category ~., data = train_set2, control = hypers4,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree4, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree4, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree4$finalModel$frame)

comp_tbl <- data.frame("Nodes" = nodes, "TrainAccuracy" = a_train, "TestAccuracy" = a_test,
                       "MaxDepth" = 1, "Minsplit" = 2, "Minbucket" = 2)

hypers5 = rpart.control(minsplit =  3, maxdepth = 2, minbucket = 3)
tree5 <- train(category ~., data = train_set2, control = hypers5,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree5, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree5, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree5$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 2, 3, 3))

hypers6 = rpart.control(minsplit =  3, maxdepth = 3, minbucket = 3)
tree6 <- train(category ~., data = train_set2, control = hypers6,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree6, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree6, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree6$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 3, 3, 3))

hypers7 = rpart.control(minsplit =  5, maxdepth = 3, minbucket = 5)
tree7 <- train(category ~., data = train_set2, control = hypers7,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree7, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree7, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree7$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 3, 5, 5))

hypers8 = rpart.control(minsplit =  7, maxdepth = 4, minbucket = 7)
tree8 <- train(category ~., data = train_set2, control = hypers8,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree8, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree8, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree8$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 7, 7))

hypers9 = rpart.control(minsplit =  30, maxdepth = 6, minbucket = 30)
tree9 <- train(category ~., data = train_set2, control = hypers9,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree9, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree9, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree9$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 6, 30, 30))

hypers10 = rpart.control(minsplit =  50, maxdepth = 3, minbucket = 50)
tree10 <- train(category ~., data = train_set2, control = hypers10,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree10, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree10, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree10$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 3, 50, 50))

hypers11 = rpart.control(minsplit =  100, maxdepth = 4, minbucket = 100)
tree11 <- train(category ~., data = train_set2, control = hypers11,
               trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree11, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree11, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree11$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 100, 100))

hypers12 = rpart.control(minsplit =  1000, maxdepth = 4, minbucket = 1000)
tree12 <- train(category ~., data = train_set2, control = hypers12,
                trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree12, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree12, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree12$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 1000, 1000))

hypers13 = rpart.control(minsplit =  5000, maxdepth = 8, minbucket = 5000)
tree13 <- train(category ~., data = train_set2, control = hypers13,
                trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree13, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree13, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree13$finalModel$frame)

comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 8, 5000, 5000))

hypers14 = rpart.control(minsplit =  10000, maxdepth = 25, minbucket = 10000)
tree14 <- train(category ~., data = train_set2, control = hypers14,
                trControl = train_control, method = "rpart1SE")

pred_train <- predict(tree14, train_set2)
cfm_train <- confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree14, test_set2)
cfm_test <- confusionMatrix(test_set2$category, pred_test)

a_train <- cfm_train$overall[1]
a_test <- cfm_test$overall[1]
nodes <- nrow(tree14$finalModel$frame)

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

# Problem 3.c
pred_train <- predict(tree5, train_set2)
confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree5, test_set2)
confusionMatrix(test_set2$category, pred_test)

train_control2 <- trainControl(method = "cv", number = 5)
hypers5 = rpart.control(minsplit =  3, maxdepth = 2, minbucket = 3)
tree5 <- train(category ~., data = train_set2, control = hypers5,
               trControl = train_control2, method = "rpart1SE")

pred_train <- predict(tree5, train_set2)
confusionMatrix(train_set2$category, pred_train)

pred_test <- predict(tree5, test_set2)
confusionMatrix(test_set2$category, pred_test)

# Problem 4
bank_modified <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 5\\Data File\\Bank_Modified.csv")
head(bank_modified)

bank_modified <- subset(bank_modified, select = - X)
bank_modified$approval <- as.factor(bank_modified$approval)
bank_modified <- na.omit(bank_modified)
sum(is.na(bank_modified))

# Problem 4.a
train_control <- trainControl(method = "cv", number = 10)
hypers15 = rpart.control(minsplit =  10, maxdepth = 20)
tree15 <- train(approval ~., data = bank_modified, control = hypers15,
                trControl = train_control, method = "rpart1SE")
tree15

# Problem 4.b
var_imp <- varImp(tree15, scale = FALSE)
var_imp

# Problem 4.c
plot(var_imp)

# Problem 4.d
tree16 <- train(approval ~ bool1 + cont4 + bool2 + ages + cont3 + cont6,
                data = bank_modified, control = hypers15,
                trControl = train_control, method = "rpart1SE")
tree16

# Problem 4.e
library(rattle)

fancyRpartPlot(tree15$finalModel, caption = "")

fancyRpartPlot(tree16$finalModel, caption = "")
