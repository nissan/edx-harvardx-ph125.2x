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
