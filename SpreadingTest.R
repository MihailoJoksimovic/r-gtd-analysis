library(tidyverse)
library(lubridate)
library(modelr)
library(randomForest)
library(forecast)
library(tseries)
library(caret)

read_csv('./globalterrorismdb_0718dist.csv') -> df

df <- df %>% 
  mutate(
    date = make_date(
      iyear, 
      imonth, # Proper way of converting factor to number
      iday # Proper way of converting factor to number
    ),
    month = month(imonth, label = TRUE)
  )

# Some of the variables have to be converted to factors
factor_cols = c("country", "region", "specificity", "attacktype1", 
                "attacktype2", "attacktype3", "weaptype1", "weapsubtype1", "targtype1", "targsubtype1", 
                "property", "propextent", "ishostkid", "INT_LOG", "INT_IDEO", "vicinity", "success", 
                "suicide", "claimed")

df[factor_cols] = lapply(df[factor_cols], factor)

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

# Todo: Train KNN and see which countries are similar

