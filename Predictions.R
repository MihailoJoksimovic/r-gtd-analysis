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

# Some of the variables have to be converted to factors
factor_cols = c("country", "region", "specificity", "attacktype1", 
                "attacktype2", "attacktype3", "weaptype1", "weapsubtype1", "targtype1", "targsubtype1", 
                "property", "propextent", "ishostkid", "INT_LOG", "INT_IDEO", "vicinity", "success", 
                "suicide", "claimed")

small_set[factor_cols] = lapply(small_set[factor_cols], factor)

# Split into train, CV and test sets
train_set <- slice(small_set, 1:50000)
cv_set <- slice(small_set, 50001:65000) 
test_set <- slice(small_set, 65000:n())

# How many attacks successed vs failed at all?
train_set %>% group_by(success) %>% summarise(n = n()) %>% mutate(ratio = n * 100 / sum(n))

# 86% of attacks were successful, 13% failed ... That's an interesting stat

# 1. Success of attack

# Was there effect of year against success of attack?
ggplot(train_set, aes(x = iyear)) + geom_bar(aes(fill = success))

# No direct effect. What if faceted against region?
ggplot(train_set, aes(x = iyear)) + geom_histogram(aes(fill = success)) + facet_wrap(~region_txt)

# Same ... no change ... Is there a trend? Let's plot by datetime
daily <- train_set %>% group_by(date) %>% summarise(count = n())

ggplot(daily, aes(x = date, y = count)) + geom_line()

# Doesn't seem to be giving any special clue ...
# Let's try with month actually ...

ggplot(train_set) + geom_bar(aes(x = month(date))) + facet_wrap(~ region_txt)

# There is no special correlation between month and # of attacks

# Is there a correlation between region and successfulnes of attack?
ggplot(train_set) + geom_bar(aes(x = iyear, fill = success)) + facet_wrap(~ region_txt)

# Doesn't seem to be a strong one ...

# Is there are relationship between day of week and successfullnes of attack?
# Or day of week and number of attacks?
train_set <- train_set %>% mutate(dow = wday(date, label=TRUE))

ggplot(train_set, aes(x=dow)) + geom_bar()

ggplot(train_set) + geom_bar(aes(x=dow)) + facet_wrap(~ region_txt)
ggplot(train_set) + geom_bar(aes(x=dow, fill = success)) + facet_wrap(~ region_txt)

#########

by_attacktype <- train_set %>% group_by(attacktype1_txt) %>% summarise(count = n())

# Is there a correlation between attack type & success?
ggplot(by_attacktype) + 
  geom_bar(aes(x = reorder(attacktype1_txt, count), y = count), stat = "identity") + 
  coord_flip()

ggplot(train_set) + 
  geom_bar(aes(x = attacktype1_txt, fill = success)) + 
  coord_flip()

train_model <- function(formula) {
  model <- glm(formula, data = train_set, family=binomial(link='logit'))
}

get_train_score <- function(model) {
  get_f1_score_on_set(model, train_set)
}

get_cv_score <- function(model) {
  get_f1_score_on_set(model, cv_set)
}

get_test_score <- function(model) {
  get_f1_score_on_set(model, test_set)
}

get_f1_score_on_set <- function(model, set) {
  predicted <- predict(model, set)
  predicted <- ifelse(predicted > 0.5, 1, 0)
  
  t1<-table(predicted, set$success)
  
  presicion<- t1[1,1]/(sum(t1[1,]))
  recall<- t1[1,1]/(sum(t1[,1]))
  
  F1<- 2*presicion*recall/(presicion+recall)
  
  F1
}

get_accuracies <- function(model) {
  # model <- train_model(formula)
  print(paste("Train F1 score: ", get_train_score(model)))
  print(paste("CV F1 score: ", get_cv_score(model)))
  print(paste("Test F1 score: ", get_test_score(model)))
}

mod1 <- glm(success ~ attacktype1 + iday + imonth + INT_LOG + vicinity + claimed, data = train_set, family=binomial(link='logit'))
get_accuracies(mod1)

# We've got some really bad scores:
# [1] "Train F1 score:  0.226227738801943"
# [1] "CV F1 score:  0.157751091703057"
# [1] "Test F1 score:  0.237539766702015"

#
# Let's try using random forrest
#

mod2 <- randomForest(success ~ attacktype1 + iday + imonth + INT_LOG + vicinity + claimed, data = train_set)


######
######
######

# Ok, at this point I came to conclusion that I'm trying to predict on categorical variables only, 
# and that doesn't seem to make much sense ...

