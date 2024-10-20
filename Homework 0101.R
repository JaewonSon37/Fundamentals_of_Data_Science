adult <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 1\\Data File\\adult.csv")
population_even <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 1\\Data File\\population_even.csv")
population_odd <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 1\\Data File\\population_odd.csv")

# Problem 1.a
summary(adult)

# Problem 1.b
age <- adult$age
hist(age, main = "Histogram of Age")

final_weight_record <- adult$fnlwgt
hist(final_weight_record, main = "Histogram of Final Weight of the Record")

# Problem 1.c
library(dplyr)
library(tidyverse)

adult_numerical_variable <- adult %>% select_if(is.numeric)
head(adult_numerical_variable)

pairs(adult_numerical_variable[1:500, 1:6], main = "Scatter Plot Matriz")

# Problem 1.d
library(ggplot2)

adult_categorical_variable <- adult %>% select_if(is.character)
head(adult_categorical_variable)

ggplot(adult, aes(x = marital.status)) + geom_bar()

ggplot(adult, aes(x = race)) + geom_bar()

workclass <- adult %>% group_by(workclass) %>% count()
workclass

native_country <- adult %>% group_by(native.country) %>% count()
native_country

# Problem 1.e
library(ggplot2)

cross_tabulation <- table(adult$workclass, adult$race)
cross_tabulation

ggplot(adult_categorical_variable, aes(x = workclass, fill = race)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Relationship between Workclass and Race",
       x = "Workclass",
       y = "Race") + 
  theme_classic()

# Problem 2.a
head(population_even)
head(population_odd)

joined_table <- merge(population_even, population_odd, by = "STATE")
head(joined_table)

# Problem 2.b.a
joined_table$NAME.y <- NULL
names(joined_table)[2] <- "NAME"
head(joined_table)

# Problem 2.b.b
library(data.table)

setnames(joined_table, old = c('POPESTIMATE2010',
                               'POPESTIMATE2011',
                               'POPESTIMATE2012',
                               'POPESTIMATE2013',
                               'POPESTIMATE2014',
                               'POPESTIMATE2015',
                               'POPESTIMATE2016',
                               'POPESTIMATE2017',
                               'POPESTIMATE2018',
                               'POPESTIMATE2019'),
         new = c('2010', '2011', '2012', '2013', '2014',
                 '2015', '2016', '2017', '2018', '2019'))
head(joined_table)

# Problem 2.b.c
joined_table <- joined_table[, order(as.numeric((colnames(joined_table))))]
joined_table <- joined_table %>%
  relocate(c(STATE, NAME))
head(joined_table)

# Problem 2.c
findNA = function(x) {nrow = dim(x)[1]
                      ncol = dim(x)[2]
                      for (i in 1:nrow) {
                        for (j in 1:ncol) {
                          if(is.na(x[i,j])) {
                            print(paste(i,',',j))}
                          }
                        }
                      }
findNA(joined_table)

NA1 <- (joined_table[3, 3] + joined_table[3, 5]) / 2
NA2 <- (joined_table[13, 7] + joined_table[13, 9]) / 2
NA3 <- (joined_table[27, 9] + joined_table[27, 11]) / 2
NA4 <- (joined_table[36, 5] + joined_table[36, 7]) / 2
NA5 <- (joined_table[50, 10] + joined_table[50, 11]) / 2

joined_table[3, 4] <- NA1
joined_table[13, 8] <- NA2
joined_table[27, 10] <- NA3
joined_table[36, 6] <- NA4
joined_table[50, 12] <- NA5

sum(is.na(joined_table))

# Problem 2.d.a
library(dplyr)

each_state_max_population <- apply(joined_table[, 3:ncol(joined_table)], 1, max)
joined_table <- mutate(joined_table, 'Max Population' = each_state_max_population)
head(joined_table)

# Problem 2.d.b
each_state_total_population <- apply(joined_table[, 3:(ncol(joined_table) - 1)], 1, sum)
joined_table <- mutate(joined_table, 'Tota Population' = each_state_total_population)
head(joined_table)

# Problem 2.e
total_population <- sum(joined_table$'2019')
total_population

# Problem 3.a
library(stringr)

three_state_data <- joined_table[, -(13:14)]
three_state_data <- three_state_data[c(9, 27, 35), ]

reshape_df <- gather(three_state_data, key = "year", value = "population", -STATE, -NAME)
reshape_df$year <- as.integer(str_extract(reshape_df$year, "\\d+"))

ggplot(reshape_df, aes(x = year, y = population, color = NAME)) +
  geom_line() +
  labs(x = "Year", y = "Population", title = "Population Change Over 10 Years") +
  theme_minimal()
