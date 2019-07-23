library(tidyverse)
library(dslabs)
data(heights)
s <- heights %>%
  filter(sex == "Male") %>%
  summarise(average = mean(height), standard_deviation = sd(height))
s$average
s$standard_deviation

heights %>% 
  filter(sex == "Male") %>%
  summarise(median = median(height),
            minimum = min(height),
            maximum = max(height))

data("murders")
murders <- murders %>% mutate(murder_rate = total/population * 100000)
summarise(murders, mean(murder_rate))

us_murder_rate <- murders %>% 
  summarise(rate = sum(total) / sum(population) * 100000)
us_murder_rate
class(us_murder_rate)
us_murder_rate %>% .$rate
class(us_murder_rate %>% .$rate)

us_murder_rate <- murders %>% 
  summarise(rate = sum(total) / sum(population) * 100000) %>%
  .$rate
class(us_murder_rate)

data("heights")
heights %>% group_by(sex)

heights %>%
  group_by(sex) %>%
  summarise(average = mean(height), standard_deviation = sd(height))

murders %>%
  group_by(region) %>%
  summarise(median_rate = median(murder_rate))

murders %>% arrange(population) %>% head()

murders %>% arrange(murder_rate) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()

murders %>% arrange(region, murder_rate) %>% head()


murders %>% top_n(10, murder_rate)
murders %>%
  arrange(desc(murder_rate)) %>% 
  top_n(10)
