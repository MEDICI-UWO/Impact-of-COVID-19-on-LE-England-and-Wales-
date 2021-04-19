library(dplyr)
library(tidyr)
source('R/life_table_functions.R')

n <- c(1, 4, rep(5, 17), 10000)

population <- read.csv('Data/population_est.csv')
deaths <- read.csv('Data/deaths_est.csv')

gender <- unique(population$Gender)
age.group <- unique(population$Age.Group)

population <- population %>%
  spread(Fit.Bound, Population)
deaths <- deaths %>%
  spread(Fit.Bound, Deaths)

LE.est <- le.summary(deaths, population, n, age.group)

write.csv(LE.est, 'Data/LE_est.csv', row.names = FALSE)

LE.est <- as.data.frame(LE.est)
LE.est <- LE.est %>%
  spread(Fit.Bound, LE)
LE.est$Gender <- factor(LE.est$Gender, levels = gender)
LE.est$Age.Group <- factor(LE.est$Age.Group, levels = age.group)
LE.est <- LE.est %>%
  arrange(Gender, Age.Group)

write.csv(LE.est, 'Manuscript Tables/Life Expectancy Estimate (2020).csv', row.names = FALSE)
