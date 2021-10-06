library(ggplot2)
library(dplyr)
library(tidyr)

LE <- read.csv('Data/LE_est.csv')
xs.mortality <- read.csv('Data/Excess Mortality.csv')

age.group <- unique(LE$Age.Group)

yll <- merge(LE, xs.mortality, by = c('Gender', 'Age.Group', 'Fit.Bound'))
yll <- yll %>%
  mutate(
    YLL = Excess.Mortality*LE
  ) %>%
  select(-c(Excess.Mortality, LE))

# Total YLLs
total.yll <- yll %>%
  group_by(Gender, Fit.Bound, Effect) %>%
  summarise(
    YLL = sum(YLL)
  ) %>%
  spread(Fit.Bound, YLL)

ggplot(total.yll, aes(x = Gender, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Direct vs Indirect Effects of COVID-19 in Years of Life Lost in England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
  theme_minimal()

net.yll <- total.yll %>%
  filter(Effect == 'Net Effects') %>%
  select(-c(Effect, fit))

separate.effect.yll <- total.yll %>%
  filter(Effect != 'Net Effects') %>%
  select(-c(lwr, upr))

stack.yll <- merge(separate.effect.yll, net.yll, by = 'Gender')

ggplot(stack.yll, aes(x = Gender, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5) +
  # labs(title = 'Direct vs Indirect Effects of COVID-19 in Years of Life Lost in England and Wales', y = 'YLLs (person-years)', x = 'Sex') +
  labs(y = 'YLLs (person-years)', x = 'Sex') +
  theme_minimal()
ggsave('New Figures/yyls_england_wales.eps')
ggsave('New Figures/yyls_effect.png')

# Coloured by Gender
# Direct effects
yll.direct <- yll %>%
  filter(Effect == 'Direct Effects') %>%
  spread(Fit.Bound, YLL)
yll.direct$Age.Group <- factor(yll.direct$Age.Group, levels = age.group)

ggplot(yll.direct, aes(x = Age.Group, y = fit, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'YLLs due to the Direct Effects of COVID-19 in England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Net effects
yll.net <- yll %>%
  filter(Effect == 'Net Effects') %>%
  spread(Fit.Bound, YLL)
yll.net$Age.Group <- factor(yll.net$Age.Group, levels = age.group)

ggplot(yll.net, aes(x = Age.Group, y = fit, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'YLLs due to Direct and Indirect Effects of COVID-19 in England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Coloured by Effect
# Total population
yll.total <- yll %>%
  filter(Gender == 'Total') %>%
  spread(Fit.Bound, YLL)
yll.total$Age.Group <- factor(yll.total$Age.Group, levels = age.group)

# ggplot(yll.total, aes(x = Age.Group, group = Effect)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Effect), alpha = 0.3) +
#   geom_line(aes(y = fit, col = Effect), size = 1) +
#   labs(title = 'Direct vs Indirect Effects due to COVID-19 in Years of Life Lost \nfor the Total Population of England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

ggplot(yll.total, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  # labs(title = 'Direct vs Indirect Effects due to COVID-19 in Years of Life Lost \nfor the Total Population of England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
  labs(y = 'YLLs (person-years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))
ggsave('New Figures/yyl_effect_total.png')

# Male population
yll.male <- yll %>%
  filter(Gender == 'Male') %>%
  spread(Fit.Bound, YLL)
yll.male$Age.Group <- factor(yll.male$Age.Group, levels = age.group)

# ggplot(yll.male, aes(x = Age.Group, group = Effect)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Effect), alpha = 0.3) +
#   geom_line(aes(y = fit, col = Effect), size = 1) +
#   labs(title = 'Direct vs Indirect Effects due to COVID-19 in Years of Life Lost \nfor the Male Population of England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

ggplot(yll.male, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Direct vs Indirect Effects due to COVID-19 in Years of Life Lost \nfor the Male Population of England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

# Female population
yll.female <- yll %>%
  filter(Gender == 'Female') %>%
  spread(Fit.Bound, YLL)
yll.female$Age.Group <- factor(yll.female$Age.Group, levels = age.group)

# ggplot(yll.female, aes(x = Age.Group, group = Effect)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Effect), alpha = 0.3) +
#   geom_line(aes(y = fit, col = Effect), size = 1) +
#   labs(title = 'Direct vs Indirect Effects due to COVID-19 in Years of Life Lost \nfor the Female Population of England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 270, vjust = 0.5))

ggplot(yll.female, aes(x = Age.Group, y = fit, fill = Effect)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .2, position = position_dodge(.9)) +
  labs(title = 'Direct vs Indirect Effects due to COVID-19 in Years of Life Lost \nfor the Female Population of England and Wales', y = 'YLLs (person-years)', x = 'Age Group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))
