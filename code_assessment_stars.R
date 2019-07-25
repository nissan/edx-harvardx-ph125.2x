library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits
str(stars)
mean(stars$magnitude)
sd(stars$magnitude)

stars %>% ggplot() + geom_density(aes(magnitude))

stars %>% ggplot() + geom_histogram(aes(temp)) 

stars %>% ggplot(aes(temp, magnitude)) + geom_point()

stars %>% ggplot(aes(temp, magnitude)) + geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()

stars %>% ggplot(aes(temp, magnitude)) + geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()

stars %>% filter(star%in%c("Antares", "Castor", "Mirfak", "Polaris", "van Maanen's Star")) %>% 
  ggplot(aes(temp, magnitude)) + geom_point() + geom_text(aes(label=star))
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()
  
  stars %>% filter(star%in%c("Rigel", "Deneb", "SiriusB", "van Maanen's Star", "Alnitak", "Alnitam", "Betelgeuse", "Antares", "Wolf359", "G51-I5")) %>% 
    ggplot(aes(temp, magnitude)) + geom_point() + geom_text(aes(label=star))
  scale_y_reverse() +
    scale_x_log10() +
    scale_x_reverse()
  
  stars %>% filter(star%in%c("Sun")) %>% 
    ggplot(aes(temp, magnitude)) + geom_point() + geom_text(aes(label=star))
  scale_y_reverse() +
    scale_x_log10() +
    scale_x_reverse()
  
  stars %>% filter(star%in%c("van Maanen's Star")) %>% 
    ggplot(aes(temp, magnitude)) + geom_point() + geom_text(aes(label=star))
  scale_y_reverse() +
    scale_x_log10() +
    scale_x_reverse()
  
  stars  %>% 
    ggplot(aes(temp, magnitude)) + geom_point() + geom_text(aes(label=star))
  scale_y_reverse() +
    scale_x_log10() +
    scale_x_reverse()
  
  stars %>% ggplot(aes(temp, magnitude)) + geom_point(aes(color=type)) +
    scale_y_reverse() +
    scale_x_log10() +
    scale_x_reverse()
  
  