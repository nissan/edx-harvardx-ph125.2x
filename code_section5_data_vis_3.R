library(tidyverse)
library(dslabs)
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"), location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

data("us_contagious_diseases")
str(us_contagious_diseases)
the_disease = "Measles"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii", "Alaska") & disease==the_disease) %>%
  mutate(rate = count/population * 10000) %>%
  mutate(state = reorder(state,rate))
dat %>% filter(state == "California") %>%
  ggplot(aes(year, rate)) +
  geom_line() + ylab("Cases per 10,000") +
  geom_vline(xintercept = 1963, col = "blue")

library(RColorBrewer)
display.brewer.all(type="seq")
display.brewer.all(type="div")

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color= "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors=brewer.pal(9,"Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") + 
  theme_minimal()+ theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarise(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm = TRUE) *10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")