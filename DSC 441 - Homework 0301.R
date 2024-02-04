bank <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 3\\Data File\\BankData.csv")

# Problem 1.a
library(ggplot2)

cont1 <- bank$cont1
hist(cont1)

cont2 <- ggplot(bank, aes(x = cont2)) + 
  geom_density()
cont2

cont3 <- ggplot(bank, aes(x = cont3)) + 
  geom_density()
cont3

bool1 <- ggplot(bank, aes(x = bool1)) + 
  geom_bar()
bool1

cont4 <- as.numeric(bank$cont4)
barplot(cont4)

cont5 <- ggplot(bank, aes(x = cont5)) + 
  geom_density()
cont5

cont6 <- bank$cont6
barplot(cont6)
hist(cont6)

credit_score <- ggplot(bank, aes(x = credit.score)) + 
  geom_density()
credit_score

age <- bank$ages
hist(age)

# Problem 1.b
standarization <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
credit_score_standarization <- standarization(bank$credit.score)
head(credit_score_standarization)

min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
cont1_min_max <- min_max(bank$cont1)
head(cont1_min_max)

decimal <- function(x) {
  return((x / 100))
}
age_decimal <- decimal(bank$ages)
head(age_decimal)

# Problem 1.c
ggplot(bank, aes(x = credit.score)) +
  geom_density() +
  ggtitle("Original Distribution of Credit Score")

ggplot(data.frame(credit_score_standarization), aes(x = credit_score_standarization)) +
  geom_density() +
  ggtitle("Distribution of Credit Score after Standardization")

ggplot(bank, aes(x = cont1)) +
  geom_density() +
  ggtitle("Original Distribution of cont1")

ggplot(data.frame(cont1_min_max), aes(x = cont1_min_max)) +
  geom_density() +
  ggtitle("Distribution of cont1 after Min-Max Normalization")

ggplot(bank, aes(x = ages)) +
  geom_density() +
  ggtitle("Original Distribution of Ages")

ggplot(data.frame(age_decimal), aes(x = age_decimal)) +
  geom_density() +
  ggtitle("Distribution of Ages after Decimal Scaling")

# Problem 1.d
length(bank$ages)
max(bank$ages)
min(bank$ages)

ages_bins <- cut(bank$ages,
                 breaks = c(11, 20, 29, 38, 47, 56, 65, 74, 84),
                 include.lowest = TRUE,
                 right = FALSE)
head(ages_bins)

# Problem 1.e
find_mean <- function(x) {
  bin_mean <- mean(bank$ages[ages_bins == x])
  return(rep(bin_mean, sum(ages_bins == x)))
}
ages_bins_smoothing <- unlist(lapply(unique(ages_bins), find_mean))
cat("Smoothed Data:", ages_bins_smoothing)

# Problem 2.a
sum(is.na(bank))
bank_clean <- na.omit(bank)
sum(is.na(bank_clean))

library(caret)
library(e1071)

train_control_cv = trainControl(method = "cv", number = 10)
svm_bank <- train(approval ~., data = bank_clean, method = "svmLinear",
                  trControl = train_control_cv)
svm_bank

# Problem 2.b
grid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100))
svm_grid_bank <- train(approval ~., data = bank_clean, method = "svmLinear",
                       trControl = train_control_cv, tuneGrid = grid)
svm_grid_bank

# Problem 3.a
library(dplyr)
library(e1071)
library(caret)

starwars <- starwars
head(starwars)

starwars_clean <- starwars %>%
  select(-c(name, films, vehicles, starships)) %>%
  na.omit()
head(starwars_clean)

dummies <- dummyVars(gender ~ ., data = starwars_clean)
starwars_dummies <- as.data.frame(predict(dummies, newdata = starwars_clean))
starwars_dummies$gender <- starwars_clean$gender
head(starwars_dummies)

# Problem 3.b
set.seed(37)

index = createDataPartition(y = starwars_dummies$gender, p = 0.7, list = FALSE)
train_set = starwars_dummies[index, ]
test_set = starwars_dummies[-index, ]

svm_starwars <- train(gender ~ ., data = train_set, method = "svmLinear")
svm_predict_starwars <- predict(svm_starwars, test_set)
sum(svm_predict_starwars == test_set$gender) / nrow(test_set)

# Problem 3.c
starwars_clean_for_pca <- starwars %>%
  select(-c(name, films, vehicles, starships, gender)) %>%
  na.omit()
head(starwars_clean_for_pca)

dummies2 <- dummyVars( ~ ., data = starwars_clean_for_pca)
starwars_dummies_for_pca <- as.data.frame(predict(dummies2, newdata = starwars_clean_for_pca))

starwars_pca <- prcomp(starwars_dummies_for_pca)
summary(starwars_pca)

screeplot(starwars_pca, type = "l") + title(xlab = "PCs")

cumulative_variance <- cumsum(starwars_pca$sdev ^ 2 / sum(starwars_pca$sdev ^ 2))
num_components <- which(cumulative_variance > 0.95)[1]
num_components

starwars_dummies_for_pca$gender <- starwars_clean$gender

# Problem 3.d
preProc <- preProcess(starwars_dummies_for_pca, method = "pca", pcaComp = 3)
starwars.pc <- predict(preProc, starwars_dummies_for_pca)
starwars.pc$gender <- starwars_dummies_for_pca$gender
head(starwars.pc)

set.seed(38)

index <- createDataPartition(y = starwars.pc$gender, p = 0.7, list = FALSE)
train_set <- starwars.pc[index, ]
test_set <- starwars.pc[-index, ]

svm_starwars_split <- train(gender ~ ., data = starwars.pc, method = "svmLinear")
svm_starwars_pred_split <- predict(svm_starwars_split, test_set)
test_set$gender <- factor(test_set$gender, levels = levels(svm_starwars_pred_split))
confusionMatrix(test_set$gender, svm_starwars_pred_split)

train_control_cv = trainControl(method = "cv", number = 10)
train_control_loocv = trainControl(method = "LOOCV", number = 10)
grid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100))

svm_starwars_pca_cv1 <- train(gender ~ ., data = starwars.pc, method = "svmLinear",
                             trControl = train_control_cv, tuneGrid = grid)
svm_starwars_pca_cv1

svm_starwars_pca_cv1_pred_test <- predict(svm_starwars_pca_cv1, newdata = test_set)
confusionMatrix(data = svm_starwars_pca_cv1_pred_test, reference = test_set$gender)

svm_starwars_pca_cv2 <- train(gender ~ ., data = starwars.pc, method = "svmLinear",
                             trControl = train_control_loocv, tuneGrid = grid)
svm_starwars_pca_cv2

svm_starwars_pca_cv2_pred_test <- predict(svm_starwars_pca_cv2, newdata = test_set)
confusionMatrix(data = svm_starwars_pca_cv2_pred_test, reference = test_set$gender)

# Bonus Problem 1
library(ggplot2)

mycars <- mtcars 
mycars$folds <- 0

flds <- createFolds(1:nrow(mycars), k = 5, list = TRUE) 

for (i in 1:5) { 
  mycars$folds[flds[[i]]] <- i
}

ggplot(mycars, aes(factor(folds), fill = factor(gear))) +
  geom_bar(position = "dodge") +
  labs(x = "Fold Index", y = "Count", fill = "Gears") +
  ggtitle("Distribution of Gears Variable Across Folds")
