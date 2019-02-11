# Idea: Try to predict if there is going to be a follow-up attack

library(tidyverse)
library(lubridate)

read_csv('./globalterrorismdb_0718dist.csv') -> df

# Lets focus only on data available after >= 2013
small_set <- df %>% filter(iyear >= 2013, iday != 0, imonth != 0)

# Select only some features that we are going to use to model against
small_set <- small_set %>% 
  select(eventid, iyear, imonth, iday, country, country_txt, region_txt, region) %>% 
  mutate(imonth = as.factor(imonth), iday = as.factor(iday))

# Introduce a date column
small_set <- small_set %>% 
  mutate(
    date = make_date(
      iyear, 
      as.numeric(levels(imonth))[imonth], # Proper way of converting factor to number
      as.numeric(levels(iday))[iday]) # Proper way of converting factor to number
  ) %>% 
  select(eventid, date, everything())

# Introduce something crazy -- number of days until next incident has happened ...
# NOTE: I'm pretty sure there's a much faster way to do this ... but to hell with it :-)
small_set <- small_set %>% add_column(next_incident_after_days = -1)
small_set$next_incident_after_days = -1

# Heavy operation below ... I've stored the results into a file, which we'll use for loading the data
# for (i in 1:dim(small_set)[1]) {
#   current_date <- small_set$date[i]
#   
#   # Next incident
#   next_incident <- slice(small_set, i:n()) %>% filter(country == small_set$country[i], eventid > small_set$eventid[i])
#   next_incident_date <- next_incident$date[1]
#   
#   days_diff <- as.numeric(difftime(next_incident_date, current_date, units="days"))
#   
#   small_set$next_incident_after_days[i] = days_diff
# }

# Plot a number of attacks on yearly basis ...

# Lets see number of attacks per year ...
ggplot(small_set, aes(x = year(date))) + geom_bar() + facet_wrap(~ region_txt)

# What's interesting to be seen here is that Middle East & North Africa have enormous amounts of attacks
# Let's play with that subset
middle_east <- filter(small_set, region == 10) # 10 == Middle East & North Africa

ggplot(middle_east, aes(x = year(date))) + geom_bar()

middle_east <- middle_east %>% add_column(next_incident_after_days = -1)

