library(caret)
library(dplyr)

data(Sacramento)
head(Sacramento)

ggplot(Sacramento, aes(beds)) +
  geom_histogram(binwidth = 1)

Sacramento <- Sacramento %>% filter(beds < 6)
Sacramento$beds <- as.factor(Sacramento$beds)

ggplot(Sacramento, aes(beds)) + 
  geom_bar()

## Decision Trees
train_control <- trainControl(method = "cv", number = 10)
tree1 <- train(beds ~., data = Sacramento, method = "rpart", trControl = train_control)
tree1

## Classification
pred_tree <- predict(tree1, Sacramento)
confusionMatrix(Sacramento$beds, pred_tree)

## Visualization
library(rattle)

fancyRpartPlot(tree1$finalModel, caption = "")
