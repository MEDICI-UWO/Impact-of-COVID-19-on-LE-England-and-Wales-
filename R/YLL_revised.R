# Calculate the years of life lost (YLL) for England and Wales in 2020
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

# Baseline number of deaths
baseline.deaths <- read.csv('Data/deaths_est.csv')
baseline.deaths$Age.Group <- factor(baseline.deaths$Age.Group, levels = age.group)
baseline.deaths <- baseline.deaths %>%
  arrange(Gender, Fit.Bound, Age.Group)

# Excess mortality that we estimated earlier (but only need net effects)
excess.mortality.all <- read.csv('Data/Excess Mortality.csv')
excess.mortality.all$Age.Group <- factor(excess.mortality.all$Age.Group, levels = age.group)
excess.mortality.all <- excess.mortality.all %>%
  arrange(Gender, Fit.Bound, Age.Group)

excess.mortality <- excess.mortality.all %>%
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

# Expected Life Expectancy for 2020
expected.LE <- function(deaths, population, n) {
  bounds <- c('fit', 'lwr', 'upr')
  genders <- c('Total', 'Male', 'Female')
  df <- NULL
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
    temp <- life.table(deaths.fit, population.fit, n)[, 'Years.remaining']
    fit.df <- data.frame(Gender = g, Fit.Bound = 'fit', Age.Group = age.group, Years.Remaining = temp)
    
    # lwr
    deaths.lwr <- deaths.temp %>%
      filter(Fit.Bound == 'upr')
    deaths.lwr <- deaths.lwr$Deaths
    population.lwr <- population.temp %>%
      filter(Fit.Bound == 'lwr')
    population.lwr <- population.lwr$Adjusted.Pop
    temp <- life.table(deaths.lwr, population.lwr, n)[, 'Years.remaining']
    lwr.df <- data.frame(Gender = g, Fit.Bound = 'lwr', Age.Group = age.group, Years.Remaining = temp)

    # upr
    deaths.upr <- deaths.temp %>%
      filter(Fit.Bound == 'lwr')
    deaths.upr <- deaths.upr$Deaths
    population.upr <- population.temp %>%
      filter(Fit.Bound == 'upr')
    population.upr <- population.upr$Adjusted.Pop
    temp <- life.table(deaths.upr, population.upr, n)[, 'Years.remaining']
    upr.df <- data.frame(Gender = g, Fit.Bound = 'upr', Age.Group = age.group, Years.Remaining = temp)
    
    df <- rbind(df, fit.df, lwr.df, upr.df)
  }
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

YLL.df <- merge(excess.mortality.all, expected.LE.df, by = c('Gender', 'Fit.Bound', 'Age.Group'))
YLL.df <- YLL.df %>%
  mutate(
    YLL = Excess.Mortality * Years.Remaining
  ) %>%
  select(-c(Excess.Mortality, Years.Remaining)) %>%
  spread(Fit.Bound, YLL)

# YLL for total population
YLL.total <- YLL.df %>%
  filter(Gender == 'Total')
ggplot(data=YLL.total, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Age Group',
       y = 'Years of Life Lost (person years)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# YLL for male population
YLL.male <- YLL.df %>%
  filter(Gender == 'Male')
ggplot(data=YLL.male, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Age Group',
       y = 'Years of Life Lost (person years)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# YLL for female population
YLL.female <- YLL.df %>%
  filter(Gender == 'Female')
ggplot(data=YLL.female, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Age Group',
       y = 'Years of Life Lost (person years)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Total YLL for total, male, and female population
YLL.sum <- YLL.df %>%
  group_by(Gender, Effect) %>%
  summarise(
    fit = sum(fit),
    lwr = sum(lwr),
    upr = sum(upr)
  )

YLL.sum.net <- YLL.sum %>%
  filter(Effect == 'Net Effects') %>%
  select(Gender, lwr, upr)

YLL.sum <- YLL.sum %>%
  filter(Effect != 'Net Effects') %>%
  select(Gender, Effect, fit)

YLL.sum <- merge(YLL.sum, YLL.sum.net, by = 'Gender')

ggplot(YLL.sum, aes(x = Gender, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5) +
  labs(x = 'Sex',
       y = 'Years of Life Lost (person years)') +
  theme_minimal()

# YLL due to net effects of COVID-19 for male and female population
YLL.net <- YLL.df %>%
  filter(
    Effect == 'Net Effects',
    Gender != 'Total'
  )

ggplot(data=YLL.net, aes(x = Age.Group, y = fit, fill = Gender)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Age Group',
       y = 'Years of Life Lost (person years)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# YLL due to direct effects of COVID-19 for male and female population
YLL.direct <- YLL.df %>%
  filter(
    Effect == 'Direct Effects',
    Gender != 'Total'
  )

ggplot(data=YLL.direct, aes(x = Age.Group, y = fit, fill = Gender)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Age Group',
       y = 'Years of Life Lost (person years)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# YLL due to indirect effects of COVID-19 for male and female population
YLL.indirect <- YLL.df %>%
  filter(
    Effect == 'Indirect Effects',
    Gender != 'Total'
  )

ggplot(data=YLL.indirect, aes(x = Age.Group, y = fit, fill = Gender)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9))+
  labs(x = 'Age Group',
       y = 'Years of Life Lost (person years)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))
