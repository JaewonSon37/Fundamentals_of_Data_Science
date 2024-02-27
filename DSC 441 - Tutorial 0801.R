## Clustering
library(tidyverse)
library(caret)

df <- storms
df <- na.omit(df)
head(df)

predictors <- df %>% select(-c(name, status, tropicalstorm_force_diameter, hurricane_force_diameter))
predictors$category <- as.integer(predictors$category)
head(predictors)

set.seed(37)
preproc <- preProcess(predictors, method = c("center", "scale"))
predictors <- predict(preproc, predictors)

# K Means
library(stats)
library(factoextra)
library(ggplot2)

fviz_nbclust(predictors, kmeans, method = "wss")

fviz_nbclust(predictors, kmeans, method = "silhouette")

fit <- kmeans(predictors, center = 4, nstart = 25)
fit

fviz_cluster(fit, data = predictors)

pca = prcomp(predictors)
rotated_data = as.data.frame(pca$x)
rotated_data$Color <- df$status
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Color)) +
  geom_point(alpha = 0.3)

rotated_data$Clusters = as.factor(fit$cluster)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) +
  geom_point()

# Hierarchical Cluster Analysis
library(cluster)

dist_mat <- dist(predictors, method = "euclidean")
hfit <- hclust(dist_mat, method = "complete")
plot(hfit)

fviz_nbclust(predictors, FUN = hcut, method = "wss")

fviz_nbclust(predictors, FUN = hcut, method = "silhouette")

h3 <- cutree(hfit, k = 3)

fviz_cluster(list(data = predictors, cluster = h3))

rotated_data$Clusters = as.factor(h3)

ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point()

ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Color)) + 
  geom_point()

result <- data.frame(Status = df$status, HAC3 = h3, Kmeans = fit$cluster)
head(result, n = 100)

result %>% group_by(HAC3) %>% 
  select(HAC3, Status) %>% 
  table()

result %>% group_by(Kmeans) %>% 
  select(Kmeans, Status) %>% 
  table()

# Hierarchical Cluster Agglomeration Methods
hfit <- hclust(dist_mat, method = 'average')
h8 <- cutree(hfit, k = 8)
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point(alpha = 0.5)

hfit <- hclust(dist_mat, method = 'median')
h8 <- cutree(hfit, k = 8)
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point(alpha = 0.5)

hfit <- hclust(dist_mat, method = 'centroid')
h8 <- cutree(hfit, k = 8)
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point(alpha = 0.5)

hfit <- hclust(dist_mat, method = 'single')
h8 <- cutree(hfit, k = 8)
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point(alpha = 0.5)

hfit <- hclust(dist_mat, method = 'complete')
h8 <- cutree(hfit, k = 8)
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point(alpha = 0.5)

hfit <- hclust(dist_mat, method = 'ward.D')
h8 <- cutree(hfit, k = 8)
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) + 
  geom_point(alpha = 0.5)

# Hierarchical Cluster Analysis - Combination of both numerical and categorical predictors
dist_mat2 <- daisy(iris)
summary(dist_mat2)