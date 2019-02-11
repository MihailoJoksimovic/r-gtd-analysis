library(tidyverse)
library(lubridate)

read_csv('./globalterrorismdb_0718dist.csv') -> df;

small_set <- df %>% 
  select(eventid, iyear, imonth, iday, country, country_txt, region_txt, region) %>% 
  mutate(imonth = as.factor(imonth), iday = as.factor(iday))


# General trend of # of attacks in world
ggplot(small_set) + geom_bar(mapping = aes(x = iyear))

# For some reason, we have missing data in 1993 ... Not sure why but this should be kept in mind.
# In general though, we could see that trend is increasing.

# Let's try as a histogram ...
ggplot(small_set) + geom_histogram(binwidth = 3, mapping = aes(x = iyear))

# This doesn't give much answers ... Trend is definitely increasing ...

# I'm just curious ... does it depend on the month ?
# Ok so it turns out that up until 2011, when month and day were unknown, it was recorded as 0. After 2011, 
# month was recorded as midpoint based on data available

# How many 0 months are there at all?
small_set %>% filter(imonth == 0) %>%  group_by(iyear, imonth) %>% summarise(count = n())

# ... or as a bar chart ...
ggplot(data = filter(small_set, imonth == 0), aes(iyear)) + geom_bar()

# Total
count(filter(small_set, imonth == 0))

# What about missing days? Are there any?
count(filter(small_set, iday == 0))

# There are some ... 871 to be exact ... Should we remove or impute them? I'm not sure ...
# How many rows in total are we going to remove if we get rid of missing both month and day?
count(filter(small_set, (iday == 0 | imonth == 0))) # 891 ...

# So 871 row is missing the the date, and for 20 more both month and date are missing ...
# For now, let's ignore both of them, but an interesting idea would be to actually explore the rows
# where this data is missing. Maybe we can find the info online and fill in this data? Like - search
# for that attack and impute the data and make it more complete ... But, again, for now, let's just 
# ignore it
small_set <- filter(small_set, imonth != 0 & iday != 0)

# Let's explore the trend by months
ggplot(data = small_set, mapping = aes(x = iyear)) + geom_freqpoly(mapping =  aes(colour = imonth))

# There doesnt seem to be a clear trend, at least when it comes to global data ...
# Let's see if there's some better trend if we look at >= 2010 only
ggplot(data = filter(small_set, iyear >= 2010), mapping = aes(x = iyear)) + 
  geom_freqpoly(mapping =  aes(colour = imonth), binwidth = 1)

# Still there's no clear trend to be seen ... Let's see if Bar chart provides any better insight
ggplot(data = filter(small_set), mapping = aes(x = imonth)) + 
  geom_bar()

# Nope, it doesn't ... Let's try something else... Let's make a date out of the columns I have, and then 
# plot them as freqplot ... Let's see if it gives any better insight

# First let's make a date column
small_set <- small_set %>% 
  mutate(
    date = make_date(
      iyear, 
      as.numeric(levels(imonth))[imonth], # Proper way of converting factor to number
      as.numeric(levels(iday))[iday]) # Proper way of converting factor to number
    ) %>% 
  select(eventid, date, everything())

# Let's see if there is any difference based on the week day?

daily <- small_set %>% group_by(date) %>% summarise(count = n())

# Introduce a weekday column first ...
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

# Now draw a boxplot
ggplot(daily, aes(wday, count)) + 
  geom_boxplot()

# As it can be seen, there doesn't seem to be nothing spectacular about the day of the month
# and number of attacks ...

# Let's see if there is something behind the actual month?
monthly <- small_set %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  group_by(iyear, month) %>% 
  summarise(count = n())

ggplot(monthly, aes(month, count)) + 
  geom_boxplot()

# And it doesn't seem there's anything spectacular behind a month ...

#
# Let's try introducing Region into the game now ...
#

# General trend of # of attacks in world
ggplot(small_set) + geom_bar(mapping = aes(x = iyear))

