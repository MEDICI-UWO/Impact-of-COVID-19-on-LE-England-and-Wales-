# Calculate the life expectancy decrease in England and Wales due to net, direct, and indirect effects of COVID-19
library(dplyr)
library(tidyr)
library(ggplot2)
source('R/life_table_functions.R')

# Mid-Year Population estimated by the ONS
population <- read.csv('Data/population_ons.csv')

# number of years in each age cohort (for life expectancy calculations)
n <- c(1, 4, rep(5, 17), 10000)

# get order of age groups
age.group <- unique(population$Age.Group)

population$Age.Group <- factor(population$Age.Group, levels = age.group)
population <- population %>%
  arrange(Gender, Age.Group)

# Number of total deaths reported by the ONS
total.deaths <- read.csv('Data/Total Deaths.csv')
total.deaths$Total.Deaths <- rowSums(total.deaths[, 3:ncol(total.deaths)])
total.deaths <- total.deaths %>%
  select(Gender, Age.Group, Total.Deaths)

# Number of COVID-19 deaths reported by the ONS
covid.deaths <- read.csv('Data/COVID-19 Deaths.csv')
covid.deaths$COVID.Deaths <- rowSums(covid.deaths[, 3:ncol(covid.deaths)])
covid.deaths$midyear.COVID.Deaths <- rowSums(covid.deaths[, 3:29])
covid.deaths <- covid.deaths %>%
  select(Gender, Age.Group, midyear.COVID.Deaths, COVID.Deaths)

# Merge number of total and covid-19 deaths together into one data frame and calculate the difference (to get the indirect impact)
deaths <- merge(total.deaths, covid.deaths, by = c('Gender', 'Age.Group'))
deaths <- deaths %>%
  mutate(
    Deaths.wo.COVID = Total.Deaths - COVID.Deaths
  )
deaths$Age.Group <- factor(deaths$Age.Group, levels = age.group)
deaths <- deaths %>%
  arrange(Gender, Age.Group)

# Baseline number of deaths
baseline.deaths <- read.csv('Data/deaths_est.csv')
baseline.deaths$Age.Group <- factor(baseline.deaths$Age.Group, levels = age.group)
baseline.deaths <- baseline.deaths %>%
  arrange(Gender, Fit.Bound, Age.Group)

# Excess mortality that we estimated earlier (but only need net effects)
excess.mortality <- read.csv('Data/Excess Mortality.csv')
excess.mortality <- excess.mortality %>%
  filter(Effect == "Net Effects") %>%
  mutate(
    half.excess.mortality = round(Excess.Mortality/2)
  )
# Divide the number of excess mortality in half (this will be used to adjust the population for expected LE without the pandemic)
excess.mortality$half.excess.mortality[excess.mortality$half.excess.mortality < 0] <- 0
excess.mortality <- excess.mortality %>%
  select(
    -c(Effect, Excess.Mortality)
  )
excess.mortality$Age.Group <- factor(excess.mortality$Age.Group, levels = age.group)
excess.mortality <- excess.mortality %>%
  arrange(Gender, Fit.Bound, Age.Group)

# Actual Life Expectancy for 2020
current.LE <- function(deaths, population, n) {
  genders <- c('Total', 'Male', 'Female')
  LE <- c()
  for (g in genders) {
    deaths.temp <- deaths %>%
      filter(Gender == g)
    deaths.temp <- deaths.temp$Total.Deaths
    population.temp <- population %>%
      filter(Gender == g)
    population.temp <- population.temp$Population
    temp <- life.table(deaths.temp, population.temp, n)[1, 'Years.remaining']
    LE <- append(LE, temp)
  }
  df <- data.frame(Gender = genders, Current.LE = LE)
  return(df)
}

current.LE.df <- current.LE(deaths, population, n)

# Expected Life Expectancy for 2020
expected.LE <- function(deaths, population, n) {
  bounds <- c('fit', 'lwr', 'upr')
  genders <- c('Total', 'Male', 'Female')
  fit <- c()
  lwr <- c()
  upr <- c()
  for (g in genders) {
    deaths.temp <- deaths %>%
      filter(Gender == g)
    population.temp <- population %>%
      filter(Gender == g)
    
    # fit
    deaths.fit <- deaths.temp %>%
      filter(Fit.Bound == 'fit')
    deaths.fit <- deaths.fit$Deaths
    population.fit <- population.temp %>%
      filter(Fit.Bound == 'fit')
    population.fit <- population.fit$Adjusted.Pop
    temp <- life.table(deaths.fit, population.fit, n)[1, 'Years.remaining']
    fit <- append(fit, temp)
    
    # lwr
    deaths.lwr <- deaths.temp %>%
      filter(Fit.Bound == 'upr')
    deaths.lwr <- deaths.lwr$Deaths
    population.lwr <- population.temp %>%
      filter(Fit.Bound == 'lwr')
    population.lwr <- population.lwr$Adjusted.Pop
    temp <- life.table(deaths.lwr, population.lwr, n)[1, 'Years.remaining']
    lwr <- append(lwr, temp)
    
    # upr
    deaths.upr <- deaths.temp %>%
      filter(Fit.Bound == 'lwr')
    deaths.upr <- deaths.upr$Deaths
    population.upr <- population.temp %>%
      filter(Fit.Bound == 'upr')
    population.upr <- population.upr$Adjusted.Pop
    temp <- life.table(deaths.upr, population.upr, n)[1, 'Years.remaining']
    upr <- append(upr, temp)
  }
  df.spread <- data.frame(Gender = genders, fit = fit, lwr = lwr, upr = upr)
  df <- df.spread %>%
    gather(Fit.Bound, Expected.LE, fit, lwr, upr)
  return(df)
}

adjusted.pop <- merge(population, excess.mortality, by = c('Gender', 'Age.Group'))
adjusted.pop <- adjusted.pop %>%
  mutate(
    Adjusted.Pop = Population + half.excess.mortality
  ) %>%
  select(Gender, Age.Group, Fit.Bound, Adjusted.Pop) %>%
  arrange(Gender, Fit.Bound, Age.Group)

expected.LE.df <- expected.LE(baseline.deaths, adjusted.pop, n)

# LE without COVID-19 deaths
no.COVID.LE <- function(deaths, population, n) {
  genders <- c('Total', 'Male', 'Female')
  LE <- c()
  for (g in genders) {
    deaths.temp <- deaths %>%
      filter(Gender == g)
    deaths.temp <- deaths.temp$Deaths.wo.COVID
    population.temp <- population %>%
      filter(Gender == g)
    population.temp <- population.temp$Adjusted.Pop
    temp <- life.table(deaths.temp, population.temp, n)[1, 'Years.remaining']
    LE <- append(LE, temp)
  }
  df <- data.frame(Gender = genders, no.COVID.LE = LE)
  return(df)
}

adjusted.pop.no.COVID <- merge(population, deaths, by = c('Gender', 'Age.Group'))
adjusted.pop.no.COVID <- adjusted.pop.no.COVID %>%
  mutate(
    Adjusted.Pop = Population + midyear.COVID.Deaths
  ) %>%
  select(Gender, Age.Group, Adjusted.Pop) %>%
  arrange(Gender, Age.Group)

no.COVID.LE.df <- no.COVID.LE(deaths, adjusted.pop.no.COVID, n)

all.LE <- merge(current.LE.df, no.COVID.LE.df, by = 'Gender')
all.LE <- merge(expected.LE.df, all.LE, by = 'Gender')
all.LE <- all.LE %>%
  arrange(Gender, Fit.Bound)

all.LE.plot <- all.LE %>%
  rename(
    `Baseline LE` = Expected.LE,
    `Actual LE` = Current.LE,
    `No COVID-19 LE` = no.COVID.LE
  ) %>%
  gather(`LE Calculation`, LE, `Baseline LE`, `Actual LE`, `No COVID-19 LE`) %>%
  spread(Fit.Bound, LE)

ggplot(data=all.LE.plot, aes(x = Gender, y = fit, fill = `LE Calculation`)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Sex',
       y = 'Life Expectancy at Birth (years)') +
  theme_minimal()

LE.decrease <- all.LE %>%
  mutate(
    `Net Effect` = Expected.LE - Current.LE,
    `Direct Effect` = no.COVID.LE - Current.LE,
    `Indirect Effect` = Expected.LE - no.COVID.LE
  ) %>%
  select(Gender, Fit.Bound, `Net Effect`, `Direct Effect`, `Indirect Effect`)

LE.decrease <- LE.decrease %>%
  gather(Effect, LE.decrease, `Net Effect`, `Direct Effect`, `Indirect Effect`)

LE.decrease.percent <- LE.decrease %>%
  filter(Fit.Bound == 'fit') %>%
  spread(Effect, LE.decrease) %>%
  mutate(
    direct.percent = `Direct Effect`/`Net Effect`,
    indirect.percent = `Indirect Effect`/`Net Effect`
  ) %>%
  select(Gender, direct.percent, indirect.percent)

LE.decrease <- LE.decrease %>%
  spread(Fit.Bound, LE.decrease)

LE.decrease.net <- LE.decrease %>%
  filter(Effect == 'Net Effect') %>%
  select(Gender, lwr, upr)

LE.decrease.plot <- LE.decrease %>%
  filter(Effect != 'Net Effect') %>%
  select(Gender, Effect, fit)

LE.decrease.plot <- merge(LE.decrease.plot, LE.decrease.net, by = 'Gender')

# Plot of decrease in life expectancy in England and Wales
# error bars correspond to the uncertainty of the net effects of the COVID-19 pandemic
ggplot(LE.decrease.plot, aes(x = Gender, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5) +
  labs(x = 'Sex',
       y = 'Decrease in Life Expectancy (years)') +
  theme_minimal()
