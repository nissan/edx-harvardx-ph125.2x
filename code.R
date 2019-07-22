library(tidyverse)
library(dslabs)
data(murders)
p <- ggplot(data=murders)
class(p)
print(p)
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y=total))
p + geom_point(aes(population/10^6, total), size=3) +
  geom_text(aes(population/10^6, total, label=abb), nudge_x = 1)
args(ggplot)

p <- murders %>% ggplot(aes(population/10^6, total, label=abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1)

# local aesthetics override global aesthetics
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10")
# efficient log scaling of the axes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010")

p <- murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010")

p + geom_point(size = 3, color="blue")

p + geom_point(aes(col=region), size = 3)

r <- murders %>% 
  summarise( rate= sum(total) / sum(population) * 10^6) %>% .$rate

p + geom_point(aes(col=region), size = 3) + 
  geom_abline(intercept = log10(r))

p <- p + 
  geom_abline(intercept = log10(r), lty = 2, color= "darkgrey") +
  geom_point(aes(col=region), size=3)

p <- p + scale_color_discrete(name="Region")

p

ds_theme_set()
install.packages("ggthemes")
library(ggthemes)
p + theme_economist()

p + theme_fivethirtyeight()

install.packages("ggrepel")
library(ggrepel)

# Now from scratch
library(ggthemes)
library(ggrepel)
### First define the slope of the line
r <- murders %>% 
  summarise( rate = sum(total) / sum(population) * 10^6) %>% .$rate
## Now make the plot ##
murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_abline(intercept = log10(r), lty=2, color="darkgrey") + 
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() + 
  scale_x_log10() +
  scale_y_log10() + 
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010") +
  scale_color_discrete(name="Region") +
  theme_economist()

geom_label()
