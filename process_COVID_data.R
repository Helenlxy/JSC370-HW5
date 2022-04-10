
library(data.table)
library(dtplyr)
library(dplyr)
library(ggplot2)
#library(mgcv)
library(knitr)
#library(magrittr)
library(lubridate)
library(tidyverse)
library(plotly)
library(widgetframe)

covid_data <- data.table::fread("https://api.covidtracking.com/v1/states/daily.csv")
covid_data <- covid_data %>%
  subset(date >= 20210103 & date <= 20210306) #Choose these two dates as they are the beginning and the end of two weeks
covid_data <- covid_data[, .(positiveIncrease, totalTestResultsIncrease, date, state)]
# Coerce to Date class
covid_data[, Date := as.Date(x=as.character(date), format='%Y%m%d',origin = lubridate::origin)]

# Extract day of the week (Saturday = 6)
covid_data[, Week_Day := as.numeric(format(covid_data$Date, format='%w'))]

# Adjust end-of-week date (first saturday from the original Date)
covid_data[, End_of_Week := covid_data$Date + (6 - covid_data$Week_Day)]

# Aggregate over week and climate division
covid_data <- covid_data[, .(positiveIncrease = mean(positiveIncrease),
                             totalTestResultsIncrease = mean(totalTestResultsIncrease)),
                         by = .(state, End_of_Week)]

covid_data[, positiveIncreaseRate := ifelse(!totalTestResultsIncrease, 0, positiveIncrease / totalTestResultsIncrease)]

vacc_data <- data.table::fread("https://data.cdc.gov/resource/unsk-b7fc.csv?date=2021-01-03T00:00:00.000")
vacc_data <- vacc_data[, .(location, admin_per_100k)]

library(rvest)
doc <- read_html("https://developers.google.com/public-data/docs/canonical/states_csv")
states <- doc %>% html_table(fill=TRUE)
states <- states[[1]]

non_continuous <- c("AK", "HI", "PR", "DC")

states <- subset(states, !(state %in% non_continuous))

# data.frame(variable = names(states),
#            type = sapply(states, typeof),
#            first_values = sapply(states, function(x) paste0(head(x),  collapse = ", ")),
#            row.names = NULL) %>% 
#   kable(caption = "A variable table for the state dataset")


merged_data <- merge(covid_data, vacc_data, by.x="state", by.y = "location")
merged_data <- merge(merged_data, states, by="state")

# load state population data
### FINISH THE CODE HERE ###
state_pops_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv"))

state_pops <- state_pops_readin
state_pops$abb <- state_pops$state
state_pops$state <- state_pops$state_name
state_pops$state_name <- NULL

merged_data <- merge(merged_data, state_pops_readin, by="state")

#test
merged_data[, positivePopRate := ifelse(!population, 0, positiveIncrease / population)]

#knitr::kable(summary(merged_data), caption = "A summary table for merged dataset")

tab <- merged_data[, .(
  mean_positiveIncreaseRate = mean(positiveIncreaseRate),
  mean_positivePopRate = mean(positivePopRate),
  admin_per_100k = unique(admin_per_100k),
  lat = unique(latitude),
  lon = unique(longitude),
  name = unique(name),
  pop_density = unique(pop_density),
  population = unique(population)
), by = state]

tab <- tab[order(mean_positiveIncreaseRate)]