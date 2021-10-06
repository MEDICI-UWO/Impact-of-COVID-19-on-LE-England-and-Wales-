library(dplyr)
library(tidyr)

deaths <- read.csv('Data/Deaths (2010 - 2019).csv')
names(deaths)[3:ncol(deaths)] <- lapply(names(deaths)[3:ncol(deaths)], substring, 2)
deaths <- deaths %>%
  gather(Year, Deaths, names(deaths)[3:ncol(deaths)])
deaths$Year <- as.numeric(deaths$Year)

gender <- unique(deaths$Gender)
age.group <- unique(deaths$Age.Group)

deaths.predict <- data.frame(Year = 2020)

deaths.est <- NULL
for (g in gender) {
  for (age in age.group) {
    temp <- deaths %>%
      filter(Gender == g, Age.Group == age)
    r <- lm(Deaths~poly(Year, 3), data = temp)
    # plot(temp$Year, temp$Deaths)
    # order.year <- order(temp$Year)
    # lines(x = temp$Year[order.year], y = fitted(r)[order.year], col = 'red')
    p <- predict(r, deaths.predict, interval = 'prediction')
    deaths.est <- rbind(deaths.est, data.frame(Gender = g, Age.Group = age, p))
  }
}
row.names(deaths.est) <- NULL

deaths.est <- deaths.est %>%
  gather(Fit.Bound, Deaths, fit, lwr, upr)
deaths.est$Deaths <- round(as.numeric(deaths.est$Deaths))

write.csv(deaths.est, 'Data/deaths_est_linear.csv', row.names = FALSE)

# # Rearrange for easier transfer to supplementary material
# deaths.est <- deaths.est %>%
#   spread(Fit.Bound, Deaths)
# deaths.est$Gender <- factor(deaths.est$Gender, levels = gender)
# deaths.est$Age.Group <- factor(deaths.est$Age.Group, levels = age.group)
# deaths.est <- deaths.est %>%
#   arrange(Gender, Age.Group)
# 
# write.csv(deaths.est, 'Manuscript Tables/Death Estimates (2020).csv', row.names = FALSE)
