library(dplyr)
library(tidyr)

pop <- read.csv('Data/Population (2011 - 2019).csv')
names(pop)[3:ncol(pop)] <- lapply(names(pop)[3:ncol(pop)], substring, 2)
pop <- pop %>%
  gather(Year, Population, names(pop)[3:ncol(pop)])
pop$Year <- as.numeric(pop$Year)

gender <- unique(pop$Gender)
age.group <- unique(pop$Age.Group)

pop.predict <- data.frame(Year = 2020)

pop.est <- NULL
for (g in gender) {
  for (age in age.group) {
    temp <- pop %>%
      filter(Gender == g, Age.Group == age)
    r <- lm(Population~poly(Year, 3), data = temp)
    # plot(temp$Year, temp$Population)
    # order.year <- order(temp$Year)
    # lines(x = temp$Year[order.year], y = fitted(r)[order.year], col = 'red')
    p <- predict(r, pop.predict, interval = 'prediction')
    pop.est <- rbind(pop.est, data.frame(Gender = g, Age.Group = age, p))
  }
}
row.names(pop.est) <- NULL
pop.est <- pop.est %>%
  gather(Fit.Bound, Population, fit, lwr, upr)
pop.est$Population <- round(as.numeric(pop.est$Population))

write.csv(pop.est, 'Data/population_est.csv', row.names = FALSE)

# Rearrange for easier transfer to supplementary material
pop.est <- pop.est %>%
  spread(Fit.Bound, Population)
pop.est$Gender <- factor(pop.est$Gender, levels = gender)
pop.est$Age.Group <- factor(pop.est$Age.Group, levels = age.group)
pop.est <- pop.est %>%
  arrange(Gender, Age.Group)
write.csv(pop.est, 'Manuscript Tables/Population Estimates (2020).csv', row.names = FALSE)
