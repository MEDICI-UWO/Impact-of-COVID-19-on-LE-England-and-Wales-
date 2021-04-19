library(dplyr)
library(tidyr)

source('R/life_table_functions.R')

n <- c(1, 4, rep(5, 17), 10000)

total.deaths <- read.csv('Data/Total Deaths.csv')
total.deaths$Total.Deaths <- rowSums(total.deaths[, 3:ncol(total.deaths)])
total.deaths <- total.deaths %>%
  select(Gender, Age.Group, Total.Deaths)

covid.deaths <- read.csv('Data/COVID-19 Deaths.csv')
covid.deaths$COVID.19.Deaths <- rowSums(covid.deaths[, 3:ncol(covid.deaths)])
covid.deaths <- covid.deaths %>%
  select(Gender, Age.Group, COVID.19.Deaths)

gender <- unique(total.deaths$Gender)
age.group <- unique(total.deaths$Age.Group)

ons.deaths <- merge(covid.deaths, total.deaths, by = c('Gender', 'Age.Group'))
ons.deaths$Age.Group <- factor(ons.deaths$Age.Group, levels = age.group)
ons.deaths <- ons.deaths %>%
  arrange(Gender, Age.Group)
ons.deaths <- ons.deaths %>%
  mutate(
    Non.COVID.19.Deaths = Total.Deaths - COVID.19.Deaths
  )

population <- read.csv('Data/population_est.csv')
xs.mortality <- read.csv('Data/Excess Mortality.csv')
xs.mortality$Excess.Mortality[xs.mortality$Excess.Mortality < 0] <- 0
adj.xs.mortality <- xs.mortality %>%
  spread(Fit.Bound, Excess.Mortality)
names(adj.xs.mortality) <- c('Gender', 'Age.Group', 'fit', 'upr', 'lwr')
adj.xs.mortality <- adj.xs.mortality %>%
  gather(Fit.Bound, Excess.Mortality, fit, lwr, upr)

adj.pop <- merge(population, adj.xs.mortality, by = c('Gender', 'Age.Group', 'Fit.Bound'))
adj.pop <- adj.pop %>%
  mutate(
    Adjusted.Population = round(Population - Excess.Mortality/2)
  ) %>%
  select(-c(Population, Excess.Mortality)) %>%
  spread(Fit.Bound, Adjusted.Population)

# Approximation of life expectancy in England and Wales with COVID-19
names(total.deaths) <- c('Gender', 'Age.Group', 'Deaths')
LE <- ons.le.summary(total.deaths, adj.pop, n, age.group)
write.csv(LE, 'Data/LE_ons.csv', row.names = FALSE)

# Make version for the supplementary material of the manuscript
LE.csv <- as.data.frame(LE) %>%
  spread(Fit.Bound, LE)
LE.csv$Gender <- factor(LE.csv$Gender, levels = gender)
LE.csv$Age.Group <- factor(LE.csv$Age.Group, levels = age.group)
LE.csv <- LE.csv %>%
  arrange(Gender, Age.Group)
write.csv(LE.csv, 'Manuscript Tables/Life Expectancy (2020).csv', row.names = FALSE)

# Approximation of life expectancy in England and Wales without COVID-19
non.covid.deaths <- ons.deaths %>%
  select(Gender, Age.Group, Non.COVID.19.Deaths) %>%
  rename(Deaths = Non.COVID.19.Deaths)

population <- population %>%
  spread(Fit.Bound, Population)

LE.non.covid <- ons.le.summary(non.covid.deaths, population, n, age.group)
write.csv(LE.non.covid, 'Data/LE_non_covid.csv', row.names = FALSE)

# Make version for the supplementary material of the manuscript
LE.non.covid.csv <- as.data.frame(LE) %>%
  spread(Fit.Bound, LE)
LE.non.covid.csv$Gender <- factor(LE.non.covid.csv$Gender, levels = gender)
LE.non.covid.csv$Age.Group <- factor(LE.non.covid.csv$Age.Group, levels = age.group)
LE.non.covid.csv <- LE.non.covid.csv %>%
  arrange(Gender, Age.Group)
write.csv(LE.non.covid.csv, 'Manuscript Tables/Life Expectancy - non-COVID-19 (2020).csv', row.names = FALSE)
