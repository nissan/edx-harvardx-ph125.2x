library(dslabs)
library(tidyverse)
data(gapminder)
head(gapminder)

gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year==1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

filter(gapminder, year==1962) %>%
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point()

filter(gapminder, year%in%c(1962,2012)) %>%
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() + 
  facet_grid(continent~year)

filter(gapminder, year%in%c(1962,2012)) %>%
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() + 
  facet_grid(.~year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col=continent)) +
  geom_point() +
  facet_wrap(.~year)

gapminder %>% filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group=country)) +
  geom_line()

gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col=country)) +
  geom_line()

labels <- data.frame(country = countries, x=c(1975, 1965), y=c(60,72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) + 
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) + 
  theme(legend.position = "none")

gapminder <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color= "black")

gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color= "black")

gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color= "black") + 
  scale_x_continuous(trans="log2")

length(levels(gapminder$region))
?levels
levels(gapminder$region)
class(gapminder$region)

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN=mean)
levels(fac)

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot(aes(region, dollars_per_day, fill=continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  xlab("")
p
p + scale_y_continuous(trans = "log2")
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)



