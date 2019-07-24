install.packages("titanic")
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train
str(titanic)
head(titanic)

titanic %>%
  ggplot() + geom_density(aes(Age, group=Sex, color=Sex))

totalMale <- nrow(titanic %>% filter(Sex=="male"))
totalFemale <- nrow(titanic %>% filter(Sex=="female"))

totalMale
totalFemale

nrow(titanic %>% filter(Sex=="male" & Age==40))
nrow(titanic %>% filter(Sex=="female" & Age==40))

targetMale <- nrow(titanic %>% filter(Sex=="male" & Age<=35 & Age>=18))
targetFemale <- nrow(titanic %>% filter(Sex=="female" & Age<=35 & Age>=18))

targetMale
targetFemale

targetMale/totalMale
targetFemale/totalFemale

targetMale <- nrow(titanic %>% filter(Sex=="male" & Age<17))
targetFemale <- nrow(titanic %>% filter(Sex=="female" & Age<=17))

targetMale
targetFemale

targetMale/totalMale
targetFemale/totalFemale

?arrange
oldestPassenger = titanic %>% arrange(desc(Age))
head(oldestPassenger,20)

head(titanic)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) +
  geom_qq(dparams=params) + geom_abline()

titanic %>% ggplot() + geom_bar(aes(Survived, fill=Sex), position=position_dodge())

titanic %>%
  ggplot() + geom_density(aes(Age, ..count.., group=Survived, color=Survived),alpha=0.2)

titanic %>% filter(Fare>0) %>%
  ggplot() + geom_boxplot(aes(Survived, Fare)) + geom_jitter(aes(Survived, Fare))

titanic %>% ggplot() + geom_bar(aes(Pclass, fill=Survived))
titanic %>% ggplot() + geom_bar(aes(Pclass, fill=Survived), position=position_fill())
titanic %>% ggplot() + geom_bar(aes(Survived, fill=Pclass), position=position_fill())

titanic %>% ggplot(aes(Age, ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(Sex~Pclass)
