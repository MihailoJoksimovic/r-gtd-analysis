library(tidyverse)
library(lubridate)
library(modelr)
library(randomForest)

read_csv('./globalterrorismdb_0718dist.csv') -> df

small_set <- df %>% 
  filter(iyear >= 2013, iday != 0, imonth != 0) %>% 
  mutate(
    imonth = as.factor(imonth), 
    iday = as.factor(iday),
    success = as.factor(success)
  ) %>% 
  mutate(
    date = make_date(
      iyear, 
      as.numeric(levels(imonth))[imonth], # Proper way of converting factor to number
      as.numeric(levels(iday))[iday]) # Proper way of converting factor to number
  ) %>% 
  select(eventid, date, everything())

# Columns: Contry, year, month, date, # of attacks

by_region <- small_set %>% 
  group_by(region, date) %>% 
  summarise(region_txt = first(region_txt), n = n()) %>% 
  ungroup()

middle_east <- by_region %>% filter(region == 10) %>% select(date, n)
