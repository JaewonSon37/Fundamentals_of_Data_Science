install.packages("tidyverse")
install.packages("psych")
install.packages("hflights")

library(tidyverse)
library(psych)
library(hflights)


# Data Frames and Indexing
head(mtcars)
summary(mtcars)
names(mtcars)
mtcars$cyl

mycars <- mtcars[mtcars$cyl == 4 & mtcars$mpg > 18, c("cyl", "mpg", "hp", "wt")]
head(mycars)

mtcars[1:6, 1:6]

mycars$mpgPerWeight = mycars$mpg / mycars$wt
head(mycars)

boxplot(mtcars$hp, mtcars$disp)


# Descriptive Statistics
mean(mtcars$cyl)
median(mtcars$cyl)
sd(mtcars$cyl)
IQR(mtcars$cyl)

summary(mtcars)
describe(mtcars)

boxplot(mtcars)

hist(mtcars$cyl, nclass = 4)


# Tidyverse Basics and Piping
mycars <- filter(mtcars, cyl == 4 & mpg > 18)
mycars <- select(mycars, cyl, mpg, hp, wt)
mycars <- mutate(mycars, mpgPerWeight = mpg / wt)
head(mycars)

mycars <- mtcars %>%
  filter(cyl == 4 & mpg > 18) %>%
  select(cyl, mpg, hp, wt) %>%
  mutate(mpgPerWeight = mpg / wt)
head(mycars)

mycars %>% rename_with(tolower)

mycars %>% relocate(mpgPerWeight, .after = mpg)


# Exploratory Analysis with Tidyverse Aggregation 
data(hflights)
flights = hflights

flights %>%
  group_by(Dest) %>%
  summarise("count" = n())

flights %>%
  group_by(Dest) %>%
  summarize(avg_delay = mean(ArrDelay, na.rm = TRUE))
  
flights %>%
  group_by(Dest) %>%
  select(Dest, Cancelled) %>%
  table() %>%
  head()

flights %>% filter(Dest == "AEX") %>%
  rowwise() %>%
  mutate(TotalDelay = sum(ArrDelay, DepDelay)) %>%
  summarise(TotalDelay)


# Pivoting
arrest <- USArrests[, 1:3]

arrest <- arrest %>%
  rownames_to_column(var = "State") %>%
  pivot_longer(cols = c("Murder", "Assault"), names_to = "Crime", values_to = "value")
head(arrest)