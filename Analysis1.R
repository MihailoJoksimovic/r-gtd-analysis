library(tidyverse)

read_csv('./globalterrorismdb_0718dist.csv') -> df;

small_set <- df %>% 
  select(eventid, iyear, imonth, iday, country, country_txt, region_txt, region) %>% 
  mutate(imonth = as.factor(imonth), iday = as.factor(iday))

by_country <- group_by(small_set, country_txt);

summarise(by_country, count = n()) %>% 
  filter(count > 1000) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x= reorder(country_txt, count), y= count), stat = "identity") + 
  coord_flip();

# ggplot(data = df, mapping = aes(x = iyear, y = imonth)) + geom_tile(mapping = aes(fill = n))

small_set

# Convert Month to categorical var
small_set %>% 
  filter(imonth != 0, iyear >= 2010) %>% 
  mutate(imonth = as.factor(imonth), iyear = as.factor(iyear)) -> small_set

# Plots number of attacks per month and year
ggplot(data = small_set) + geom_hex(mapping = aes(x = iyear, y = imonth))
ggplot(data = small_set) + geom_bin2d(mapping = aes(x = iyear, y = imonth))




