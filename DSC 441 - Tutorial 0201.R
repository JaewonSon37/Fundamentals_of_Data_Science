## Data Visualization
# Line Graph
library(astsa)
library(ggplot2)

head(gdp)

df <- as.data.frame(gdp)
df$quarter <- seq(1947.25, 2018.75, by = 0.25)

plt <- ggplot(df, aes(x = quarter, y = x))
plt + geom_line() + 
  xlab("Time") +
  ylab("GDP") +
  ggtitle("GDP Time Series")

# Bar Chart
ggplot(diamonds, aes(x = clarity)) + geom_bar()

p <- ggplot(diamonds, aes(x = clarity, fill = cut))
p + geom_bar(position="stack")

p <- ggplot(diamonds, aes(x = clarity, y = price, fill = cut))
p + geom_col()

# Histogram
ggplot(mtcars, aes(hp)) + geom_histogram(binwidth = 60)

p <- ggplot(data = mtcars, aes(x = hp)) + 
  geom_histogram(binwidth = 30) +
  facet_wrap(~cyl)
p

# Box Plot
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot() +
  xlab("cyl")

# Scatter Plot
ggplot(mtcars, aes(mpg, hp)) + geom_point()

ggplot(mtcars, aes(mpg, hp)) + 
  geom_point() + 
  geom_smooth(method = lm)

ggplot(mtcars, aes(mpg, hp)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm)

# Contingency Plot
ggplot(mtcars, aes(x = cyl, y = gear, fill = wt)) +
  geom_tile() 

# QQ Plot
ggplot(mtcars, aes(sample = hp)) +
  stat_qq() +
  stat_qq_line(col = "red")

## Integration - Joining and Finding Errors
# Missing Values
library(hflights)

data("hflights")
flights = hflights
summary(flights$DepTime)

sum(is.na(flights$DepTime))

# Joins
library(dplyr)

authors <- data.frame(
  name = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil", "Gurkan"),
  nationality = c("US", "Australia", "US", "UK", "Australia", "Turkey"),
  retired = c("yes", rep("no", 4), "yes"))
books <- data.frame(
  name = c("Tukey", "Venables", "Tierney", "Ripley", "Ripley", "McNeil"),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA))

head(authors)

head(books)

authors %>% inner_join(books, by = "name")

authors %>% left_join(books, by = "name")

## Cleaning - Missing Values, Data Integrity, Normalization
# Handling Missing Data
library(tidyr)

summary(flights$DepTime)

clean_flights <- flights %>% drop_na(DepTime)
summary(clean_flights$DepTime)

summary(flights$TaxiIn)

clean_flights$TaxiIn <- ifelse(is.na(clean_flights$TaxiIn),
                               mean(clean_flights$TaxiIn, na.rm = TRUE),
                               clean_flights$TaxiIn)
summary(clean_flights$TaxiIn)

# Handling Nonsensical Values and Placeholders
grades = sample(1:100, 90, replace = TRUE)
grades[91:100] = 99999
grades = data.frame("ID" = 1001:1100, "Grades" = grades)
summary(grades$Grades)

nrow(grades)

clean_grades <- grades %>% filter(Grades <= 100)
nrow(clean_grades)

## Sampling
set.seed(19)

mtcars_indexed <- mtcars %>% mutate(id = row_number())

train <- mtcars_indexed %>% sample_frac(.75)
test <- anti_join(mtcars_indexed, train, by = 'id')

nrow(mtcars)
nrow(test)

## Normalization
library(caret)

preproc1 <- preProcess(mtcars, method = c("center", "scale"))
norm1 <- predict(preproc1, mtcars)
summary(norm1)

preproc2 <- preProcess(mtcars, method = c("range"))
norm2 <- predict(preproc2, mtcars)
summary(norm2)

## Binning and Smoothing
# Binning
mycars <- mtcars
mycars %>%
  mutate(hpfactor = cut(hp,
                        breaks=c(-Inf, 120, 200, Inf),
                        labels=c("low", "medium", "high"))) %>%
  head()

# Smoothing
mycars <- mtcars %>%
  mutate(hpfactor = cut(hp, 
                        breaks = 3,
                        labels=c("low", "medium", "high")))
head(mycars)

low <- mycars %>% 
  filter(hpfactor == 'low') %>% 
  mutate(hp = mean(hp, na.rm = T))

medium <- mycars %>% 
  filter(hpfactor == 'medium') %>% 
  mutate(hp = mean(hp, na.rm = T))

high <- mycars %>% 
  filter(hpfactor == 'high') %>% 
  mutate(hp = mean(hp, na.rm = T))

bind_rows(list(low, medium, high))

## Feature extraction
head(storms)

storm <- storms %>% select(-c("name"))
storm$category <- ordered(storm$category)
storm$category[is.na(storm$category)] <- sample(-1:5, sum(is.na(storm$category)), replace = TRUE)
storm <- na.omit(storm)
head(storm)

dummy <- dummyVars(category ~ ., data = storm)
dummies <- as.data.frame(predict(dummy, newdata = storm))
head(dummies)

nzv <- nearZeroVar(dummies)
length(nzv)

storm.pca <- prcomp(dummies)
summary(storm.pca)

screeplot(storm.pca, type = "l") + title(xlab = "PCs")

target <- storm %>% dplyr::select(category) 

preProc <- preProcess(dummies, method = "pca", pcaComp = 2)
storm.pc <- predict(preProc, dummies)
storm.pc$category <- storm$category
head(storm.pc)

library(caret)
library(e1071)

storm_dummies <- dummies
storm_dummies$category <- storm$category

train_control = trainControl(method = "cv", number = 5)

svm_storm <- train(category ~., data = storm_dummies, method = "svmLinear", trControl = train_control)
svm_storm

svm_storm2 <- train(category ~., data = storm.pc, method = "svmLinear", trControl = train_control)
svm_storm2