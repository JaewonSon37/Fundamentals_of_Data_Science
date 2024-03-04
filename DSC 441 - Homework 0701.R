# Problem 1.a
library(tidyverse)

redwine <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 7\\Data File\\winequality-red.csv", header = TRUE, sep = ";")
whitewine <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 7\\Data File\\winequality-white.csv", header = TRUE, sep = ";")

redwine$type <- 'red'
whitewine$type <- 'white'

wine <- full_join(redwine, whitewine, by = NULL)
wine$quality <- as.numeric(wine$quality)
wine$type <- as.factor(wine$type)
str(wine)

# Problem 1.b
library(caret)

pca <- prcomp(select(wine, -type), scale. = TRUE)
pca_wine <- as.data.frame(pca$x)
pca_wine$type <- wine$type

ggplot(pca_wine, aes(x = PC1, y = PC2, color = type)) + geom_point()

# Problem 1.d
set.seed(37)
index <- createDataPartition(pca_wine$type, p = 0.7, list = FALSE)
train_set <- pca_wine[index, ]
test_set <- pca_wine[-index, ]

train_control <- trainControl(method = "cv", number = 10)

knn <- train(type ~ ., data = train_set, method = "knn", trControl = train_control)
knn_pred <- predict(knn, newdata = test_set)
confusionMatrix(test_set$type, knn_pred)

decisiontrees <- train(type ~ ., data = train_set, method = "rpart", trControl = train_control)
decisiontrees_pred <- predict(decisiontrees, newdata = test_set)
confusionMatrix(test_set$type, decisiontrees_pred)

svm <- train(type ~ ., data = train_set, method = "svmLinear", trControl = train_control)
svm_pred <- predict(svm, newdata = test_set)
confusionMatrix(test_set$type, svm_pred)

# Problem 1.e
test_set$Color1 <- knn_pred
test_set$Color2 <- decisiontrees_pred
test_set$Color3 <- svm_pred

ggplot(data = test_set, aes(x = PC1, y = PC2, col = Color1)) + 
  geom_point(alpha = 0.3) + ggtitle("kNN")

ggplot(data = test_set, aes(x = PC1, y = PC2, col = Color2)) + 
  geom_point(alpha = 0.3) + ggtitle("Decision Trees")

ggplot(data = test_set, aes(x = PC1, y = PC2, col = Color3)) + 
  geom_point(alpha = 0.3) + ggtitle("SVM")

# Problem 2.a
library(tidyverse)
library(caret)

data("Sacramento")
str(Sacramento)

dummies_sacramento <- model.matrix( ~ . -1, data = Sacramento[, c("city", "zip")])
sacramento <- cbind(Sacramento, dummies_sacramento) %>%
  select(-c(city, zip))

zero_variance <- nearZeroVar(sacramento)
sacramento <- sacramento[, -zero_variance]
head(sacramento)

# Problem 2.c
library(kknn)

set.seed(37)
train_control <- trainControl(method = "cv", number = 10) 
tuneGrid <- expand.grid(kmax = 3:7, kernel = c("rectangular", "cos"), distance = 1:3)
knn <- train(type ~ ., data = sacramento, method = "kknn", trControl = train_control,
              preProcess = c("center", "scale"), tuneGrid = tuneGrid)
knn

# Problem 3.a
library(factoextra)

clusting_wine <- subset(wine, select = -c(type))

fviz_nbclust(clusting_wine, kmeans, method = "wss")

fviz_nbclust(clusting_wine, kmeans, method = "wss")

fviz_nbclust(clusting_wine, kmeans, method = "wss")

fviz_nbclust(clusting_wine, kmeans, method = "silhouette")

fviz_nbclust(clusting_wine, kmeans, method = "silhouette")

fviz_nbclust(clusting_wine, kmeans, method = "silhouette")

# Problem 3.b
library(cluster)

dist1 <- dist(clusting_wine, method = "euclidean")
dist2 <- dist(clusting_wine, method = "manhattan")

hac1 <- hclust(dist1, method = "single")
plot(hac1)

h3 <- cutree(hac1, k = 3)
silhouette_plot1 <- silhouette(h3, dist1)
fviz_silhouette(silhouette_plot1)

hac2 <- hclust(dist1, method = "complete")
plot(hac2)

h3 <- cutree(hac2, k = 3)
silhouette_plot2 <- silhouette(h3, dist1)
fviz_silhouette(silhouette_plot2)

hac3 <- hclust(dist2, method = "single")
plot(hac3)

h3 <- cutree(hac3, k = 3)
silhouette_plot3 <- silhouette(h3, dist2)
fviz_silhouette(silhouette_plot3)

hac4 <- hclust(dist2, method = "complete")
plot(hac4)

h3 <- cutree(hac4, k = 3)
silhouette_plot4 <- silhouette(h3, dist2)
fviz_silhouette(silhouette_plot4)

# Problem 3.c
best_hac_clusters <- cutree(hac1, k = 3)
best_kmeans_clusters <- kmeans(clusting_wine, center = 3, nstart = 10)

cross_tabulation <- data.frame(Type = wine$type, HAC3 = best_hac_clusters, Kmeans = best_kmeans_clusters$cluster)
head(cross_tabulation)

cross_tabulation %>% group_by(HAC3) %>% select(HAC3, Type) %>% table()

cross_tabulation %>% group_by(Kmeans) %>% select(Kmeans, Type) %>% table()

# Problem 3.d
pca2 <- prcomp(clusting_wine, scale. = TRUE)
prc_wine2 <- as.data.frame(pca2$x)
prc_wine2$Type <- wine$type
prc_wine2$HAC3 <- as.factor(cross_tabulation$HAC3)
prc_wine2$Kmeans <- as.factor(cross_tabulation$Kmeans)

ggplot(data = prc_wine2, aes(x = PC1, y = PC2, col = Type)) + 
  geom_point(alpha = 0.3)

ggplot(data = prc_wine2, aes(x = PC1, y = PC2, col = HAC3)) + 
  geom_point(alpha = 0.3)

ggplot(data = prc_wine2, aes(x = PC1, y = PC2, col = Kmeans)) + 
  geom_point(alpha = 0.3)

# Problem 4
library(dplyr)

starwars <- starwars
head(starwars)

starwars_clean <- starwars %>%
  select(-c(name, films, vehicles, starships)) %>%
  na.omit()
head(starwars_clean)

str(starwars_clean)

starwars_clean$hair_color <- as.factor(starwars_clean$hair_color)
starwars_clean$skin_color <- as.factor(starwars_clean$skin_color)
starwars_clean$eye_color <- as.factor(starwars_clean$eye_color)
starwars_clean$sex <- as.factor(starwars_clean$sex)
starwars_clean$gender <- as.factor(starwars_clean$gender)
starwars_clean$homeworld <- as.factor(starwars_clean$homeworld)
starwars_clean$species <- as.factor(starwars_clean$species)

str(starwars_clean)

# Problem 4.a
library(cluster)

starwars_matrix <- daisy(starwars_clean, metric = "gower")
summary(starwars_matrix)

hac5 <- hclust(starwars_matrix, method = "average")

# Problem 4.b
plot(hac5)

# Problem 4.c
dummy_starwars <- as.data.frame(model.matrix(~ . - 1, data = starwars_clean))

fviz_nbclust(dummy_starwars, kmeans, method = "wss")

fviz_nbclust(dummy_starwars, kmeans, method = "silhouette")

# Problem 4.d
best_hac_clusters2 <- cutree(hac5, k = 4)
best_kmeans_clusters2 <- kmeans(dummy_starwars, centers = 3, nstart = 10)

cross_tabulation2 <- data.frame(HAC4 = best_hac_clusters2, Kmeans = best_kmeans_clusters2$cluster)
head(cross_tabulation2)

cross_tabulation2 %>% group_by(HAC4) %>% select(HAC4) %>% table()

cross_tabulation2 %>% group_by(Kmeans) %>% select(Kmeans) %>% table()