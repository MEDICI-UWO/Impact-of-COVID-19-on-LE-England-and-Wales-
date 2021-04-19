library(dplyr)
library(tidyr)

source('R/life_table_functions.R')

population <- read.csv('Data/Population (2011 - 2019).csv', stringsAsFactors = FALSE)
names(population)[3:length(names(population))] <- substring(names(population)[3:length(names(population))], 2)
population <- population %>%
  gather(Year, Population, names(population)[3:length(names(population))])
population$Year <- as.integer(population$Year)

age.group <- unique(population$Age.Group)
gender <- unique(population$Gender)
years <- unique(population$Year)

n <- c(1, 4, rep(5, 17), 10000)

deaths <- read.csv('Data/Deaths (2010 - 2019).csv', stringsAsFactors = FALSE)
names(deaths)[3:length(names(deaths))] <- substring(names(deaths)[3:length(names(deaths))], 2)
deaths <- deaths %>%
  gather(Year, Deaths, names(deaths)[3:length(names(deaths))]) %>%
  filter(Year != '2010')
deaths$Year <- as.integer(deaths$Year)

historical.LE <- NULL
for (y in years) {
  for (g in gender) {
    pop.df <- population %>%
      filter(Year == y, Gender == g)
    d.df <- deaths %>%
      filter(Year == y, Gender == g)
    le <- life.table(d.df$Deaths, pop.df$Population, n)[1, 'Years.remaining']
    historical.LE <- rbind(historical.LE, data.frame(Year = y, Gender = g, LE = le))
  }
}

extrap.LE <- data.frame(Year = 1990:2010)

extrap.historical.LE <- NULL
for (g in gender) {
  temp <- historical.LE %>%
    filter(Gender == g)
  r <- lm(LE~Year, data = temp)
  p <- cbind(extrap.LE, Gender = g, LE = predict(r, extrap.LE))
  extrap.historical.LE <- rbind(extrap.historical.LE, p)
}

LE.ons <- read.csv('Data/LE_ons.csv', stringsAsFactors = FALSE)
LE.ons <- LE.ons %>%
  filter(Age.Group == '<1') %>%
  select(-Age.Group)

historical.year.df <- NULL
for (i in 1:nrow(LE.ons)) {
  g <- LE.ons[i, 'Gender']
  temp <- extrap.historical.LE %>%
    filter(Gender == g)
  historical.year <- max(temp$Year[temp$LE < LE.ons[i, 'LE']])
  historical.year.df <- rbind(historical.year.df, data.frame(LE.ons[i, c('Gender', 'Fit.Bound')], Year = historical.year))
}
historical.year.df <- historical.year.df %>%
  mutate(
    Years.reverted = 2020 - Year
  )

# Reorganize data frame to copy to manuscript
# historical.LE <- historical.LE %>%
#   spread(Gender, LE)
# 
# extrap.historical.LE <- extrap.historical.LE %>%
#   spread(Gender, LE)
# 
# historical.LE.combined <- rbind(extrap.historical.LE, historical.LE)
# 
# write.csv(historical.LE.combined, 'Data/Historical LE.csv', row.names = FALSE)
# 
# historical.year.df <- historical.year.df %>%
#   spread(Fit.Bound, Year)
# 
# write.csv(historical.year.df, 'Data/Years reverted in LE gains.csv', row.names = FALSE)
