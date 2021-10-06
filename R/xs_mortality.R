library(dplyr)
library(tidyr)
library(ggplot2)

deaths.est <- read.csv('Data/deaths_est.csv')

age.group <- unique(deaths.est$Age.Group)

total.deaths <- read.csv('Data/Total Deaths.csv')
total.deaths$Total.Deaths <- rowSums(total.deaths[, 3:ncol(total.deaths)])
total.deaths <- total.deaths %>%
  select(Gender, Age.Group, Total.Deaths)

xs.mortality <- merge(deaths.est, total.deaths, by = c('Gender', 'Age.Group'))
xs.mortality <- xs.mortality %>%
  mutate(
    Difference = Total.Deaths - Deaths
  ) %>%
  select(-c(Total.Deaths, Deaths))
xs.mortality <- xs.mortality %>%
  spread(Fit.Bound, Difference)
names(xs.mortality) <- c('Gender', 'Age.Group', 'fit', 'upr', 'lwr')
xs.mortality$Age.Group <- factor(xs.mortality$Age.Group, levels = age.group)
xs.mortality <- xs.mortality %>%
  arrange(Gender, Age.Group)

# Excess mortalities due to COVID-19 by gender
ggplot(xs.mortality, aes(x = Age.Group, y = fit, fill = Gender)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Excess Mortality attributed to COVID-19 by Gender and Age Group \nin England and Wales in 2020', y = 'Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Establish the proportion of excess mortalities are due to direct and indirect effects of COVID-19
covid.deaths <- read.csv('Data/COVID-19 Deaths.csv')
covid.deaths$COVID.19.Deaths <- rowSums(covid.deaths[, 3:ncol(covid.deaths)])
covid.deaths <- covid.deaths %>%
  select(Gender, Age.Group, COVID.19.Deaths)

non.covid.deaths <- merge(total.deaths, covid.deaths, by = c('Gender', 'Age.Group'))
non.covid.deaths <- non.covid.deaths %>%
  mutate(
    Non.COVID.19.Deaths = Total.Deaths - COVID.19.Deaths
  ) %>%
  select(-c(Total.Deaths, COVID.19.Deaths))

xs.mortality.indirect <- merge(deaths.est, non.covid.deaths, by = c('Gender', 'Age.Group'))
xs.mortality.indirect <- xs.mortality.indirect %>%
  mutate(
    Difference = Non.COVID.19.Deaths - Deaths
  ) %>%
  select(-c(Non.COVID.19.Deaths, Deaths)) %>%
  spread(Fit.Bound, Difference)
names(xs.mortality.indirect) <- c('Gender', 'Age.Group', 'fit', 'upr', 'lwr')

xs.mortality.indirect <- xs.mortality.indirect %>%
  gather(Fit.Bound, `Indirect Effects`, fit, upr, lwr)

xs.mortality <- xs.mortality %>%
  gather(Fit.Bound, `Net Effects`, fit, lwr, upr)

xs.mortality.effect <- merge(xs.mortality, xs.mortality.indirect, by = c('Gender', 'Age.Group', 'Fit.Bound'))
xs.mortality.effect <- merge(xs.mortality.effect, covid.deaths, by = c('Gender', 'Age.Group'))
xs.mortality.effect <- xs.mortality.effect %>%
  rename(
    `Direct Effects` = COVID.19.Deaths
  )
xs.mortality.effect <- xs.mortality.effect %>%
  gather(Effect, `Excess Mortality`, `Direct Effects`, `Indirect Effects`, `Net Effects`)

xs.mortality.effect$Age.Group <- factor(xs.mortality.effect$Age.Group, levels = age.group)
xs.mortality.effect <- xs.mortality.effect %>%
  arrange(Gender, Fit.Bound, Effect, Age.Group)

# write.csv(xs.mortality.effect, 'Data/Excess Mortality.csv', row.names = FALSE)

xs.mortality.effect <- xs.mortality.effect %>%
  spread(Fit.Bound, `Excess Mortality`)

total.xs.mort.effect <- xs.mortality.effect %>%
  filter(Gender == 'Total')
# Excess mortality due to direct and indirect effects of COVID-19 for the total population 
ggplot(total.xs.mort.effect, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  # labs(title = 'Excess Mortality attributed to the direct vs indirect effects of COVID-19 \nby Age Group for the Total Population of England and Wales in 2020', y = 'Deaths', x = 'Age Group') +
  labs(y = 'Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))
ggsave('New Figures/xs_mortality_effects_total.png')

male.xs.mort.effect <- xs.mortality.effect %>%
  filter(Gender == 'Male')
# Excess mortality due to direct and indirect effects of COVID-19 for the male population 
ggplot(male.xs.mort.effect, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Excess Mortality attributed to the direct vs indirect effects of COVID-19 \nby Age Group for the Male Population of England and Wales in 2020', y = 'Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

female.xs.mort.effect <- xs.mortality.effect %>%
  filter(Gender == 'Female')
# Excess mortalities due to direct and indirect effects of COVID-19 for the female population 
ggplot(female.xs.mort.effect, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Excess Mortality attributed to the direct vs indirect effects of COVID-19 \nby Age Group for the Female Population of England and Wales in 2020', y = 'Deaths', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))
