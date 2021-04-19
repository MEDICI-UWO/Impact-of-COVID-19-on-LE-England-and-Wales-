library(ggplot2)
library(dplyr)
library(tidyr)

LE.est <- read.csv('Data/LE_est.csv')
LE.ons <- read.csv('Data/LE_ons.csv')
LE.non.covid <- read.csv('Data/LE_non_covid.csv')

LE <- merge(LE.est, LE.ons, by = c('Gender', 'Age.Group', 'Fit.Bound'))
LE <- merge(LE, LE.non.covid, by = c('Gender', 'Age.Group', 'Fit.Bound'))
names(LE) <- c('Gender', 'Age.Group', 'Fit.Bound', 'Reference', 'With COVID-19', 'Without COVID-19')

LE.birth <- LE %>%
  filter(Age.Group == '<1') %>%
  select(-Age.Group) %>%
  gather(Classification, LE, Reference, `With COVID-19`, `Without COVID-19`) %>%
  spread(Fit.Bound, LE)

ggplot(LE.birth, aes(x = Classification, y = fit, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Life Expectancy at Birth in England and Wales in 2020', y = 'Life Expectancy (years)', x = 'Group') +
  theme_minimal()

LE.diff <- LE %>%
  mutate(
    `Net Effects` = `Reference` - `With COVID-19`,
    `Direct Effects` = `Without COVID-19` - `With COVID-19`,
    `Indirect Effects` = `Reference` - `Without COVID-19`
  ) %>%
  filter(Age.Group == '<1') %>%
  select(-c(Age.Group, Reference, `With COVID-19`, `Without COVID-19`)) %>%
  gather(Effects, LE.decrease, `Direct Effects`, `Indirect Effects`, `Net Effects`) %>%
  spread(Fit.Bound, LE.decrease)

ggplot(LE.diff, aes(x = Gender, y = fit, fill = Effects)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Direct vs Indirect Effects of COVID-19 in Life Expectancy at Birth \nin England and Wales',
       y = 'Decrease in Life Expectancy (years)') +
  theme_minimal()

LE.diff.net <- LE.diff %>%
  filter(Effects == 'Net Effects') %>%
  select(-c(Effects, fit))

LE.diff.combine <- LE.diff %>%
  filter(Effects != 'Net Effects') %>%
  select(-c(lwr, upr))

LE.diff.combine <- merge(LE.diff.combine, LE.diff.net, by = 'Gender')

ggplot(LE.diff.combine, aes(x = Gender, y = fit, fill = Effects)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5) +
  labs(title = 'Decrease in Life Expectancy attributed to direct vs indirect effects of COVID-19 \nin England and Wales',
       x = 'Sex',
       y = 'Decrease in Life Expectancy (years)') +
  theme_minimal()
