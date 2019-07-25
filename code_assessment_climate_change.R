library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>% .$year %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(year)
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year)

first_record_years <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% arrange(.$year) %>% head()
first_record_years
last_record_years <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% arrange(.$year) %>% tail()
last_record_years

9855/3

str(temp_carbon)

first_record_years <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% arrange(.$year) %>% head()
first_record_years
last_record_years <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% arrange(.$year) %>% tail()
last_record_years

p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_point()
p

p <- p + geom_vline(aes(xintercept = 0), col = "blue")
p

p <- p + geom_hline(aes(y = 0), col = "blue")
p

p <- p + geom_hline(aes(yintercept = 0, col = blue))
p

p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p

p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_point()
p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")


p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_point()
p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p <- p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p + geom_line(aes(year,ocean_anomaly), col="red") + geom_line(aes(year, land_anomaly), col="blue")


str(greenhouse_gases)
str(temp_carbon)
head(greenhouse_gases, 40)
tail(greenhouse_gases,20)

greenhouse_gases %>%ggplot(aes(year, concentration)) + geom_line() +
  facet_grid(gas~., scales="free") +
  geom_vline(xintercept=1850) + 
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") + 
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%ggplot(aes(year, carbon_emissions )) + geom_point() +
  geom_vline(xintercept=1850) 

str(historic_co2)
head(historic_co2)
co2_time <- historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(year, co2, colour=source)) + geom_line()
co2_time + xlim(-800000,-775000)
co2_time + xlim(-375000,-330000)
co2_time + xlim(-140000,-120000)
co2_time + xlim(-3000,2018)

