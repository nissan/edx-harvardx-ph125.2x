library(dslabs)
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
