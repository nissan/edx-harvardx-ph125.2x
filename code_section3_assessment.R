install.packages("NHANES")
library(NHANES)
data(NHANES)
library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)
mean(na_example, na.rm=TRUE)
sd(na_example, na.rm=TRUE)
library(dplyr)
library(NHANES)
data(NHANES)
colnames(NHANES)
head(NHANES)
tab <- NHANES %>% filter(Gender=="female")
head(tab)
tab <- NHANES %>% filter(Gender=="female" & AgeDecade==" 20-29")
head(tab)
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarise(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm=TRUE))
ref

ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  .$average
ref_avg