p <- heights %>%
  filter(sex=="Male") %>%
  ggplot(aes(x=height))

p + geom_histogram()
p + geom_histogram(binwidth = 1)
p + geom_histogram(bindwidth=1, fill="blue", col="black") + 
  xlab("Male heights in inches") +
  ggtitle("Histogram")

p + geom_density(fill="blue")

p <- heights %>% filter (sex=="Male") %>%
  ggplot(aes(sample=height))
p + geom_qq()

params <- heights %>% 
  filter(sex=="Male") %>% 
  summarise(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params)

p + geom_qq(dparams = params) +
  geom_abline()

heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col="black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)