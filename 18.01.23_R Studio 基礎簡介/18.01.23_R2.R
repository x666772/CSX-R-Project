titanic_train <- read.csv('D:/P/train.csv')

head(titanic_train, n = 5)

summary(titanic_train)

str(titanic_train)

library(dplyr)

titanic_train <- titanic_train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

str(titanic_train)

titanic.female <- filter(titanic_train, Sex == 'female' & Pclass == '2')

head(titanic.female)

head(select(titanic_train, Survived, Pclass, Sex, Age))

head(select(titanic_train, Survived, Pclass, Sex, Age),n=10)

unique(titanic_train$Sex)

distinct(titanic_train, Sex)

titanic_train %>%
  group_by(Sex) %>%
  summarise(count = n())

titanic_train %>%
  group_by(Pclass, Sex) %>%
  summarise(fare = mean(Fare),
            no.ppl = n(),
            survive.rate = sum(as.integer(as.character(Survived))) / n()) %>%
  arrange(desc(survive.rate))

par

hist(titanic_train$Age)

hist(titanic_train$Fare)  

plot(x = titanic_train$Sex, y = titanic_train$Survived)

library(ggplot2)

temp <- titanic_train %>%
  group_by(Sex, Survived) %>%
  summarise(count = n())

ggplot(data = temp, aes(x = Sex, y = count, fill = Survived)) + 
  geom_bar(position = 'fill', stat = 'identity')

ggplot(data = titanic_train, aes(x = Sex, fill = Survived)) +
  geom_bar(position = 'fill', stat = 'count')

ggplot(data = titanic_train, aes(x = Pclass, fill = Survived))+
  geom_bar(position = 'fill', stat = 'count')

ggplot(data = titanic_train, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = 'fill', stat = 'count')

ggplot(titanic_train) +
  geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 1, size = 1)

ggplot(titanic_train) +
  geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 1, size = 1)

boxplot(titanic_train$Age, main = 'Age')

ggplot(titanic_train, aes(x = 'Age', y = Age)) +
  geom_boxplot(color = 'red')

ggplot(titanic_train, aes(x = 'Fare', y = Fare)) +
  geom_boxplot(color = 'blue') +
  scale_y_log10()

ggplot(titanic_train, aes(x = Pclass, y = Fare, colour = Survived)) +
  geom_boxplot() +
  scale_y_log10() # take log10

titanic_train$Age[is.na(titanic_train$Age)] <- mean(titanic_train$Age, na.rm = TRUE)

titanic_train$Age <- ifelse(is.na(titanic_train$Age), median(titanic_train$Age, na.rm = TRUE), titanic_train$Age)

sum(is.na(titanic_train$Age))

table(titanic_train$Embarked)

titanic_train$Embarked[titanic_train$Embarked == ''] <- 'S'

titanic_train$Embarked[titanic_train$Embarked == ''] <- names(which.max(table(titanic_train$Embarked)))

summary(titanic_train)

library(forcats)

library(corrplot)

titanic_train %>%
  select(-PassengerId, -Name, -Cabin, -Ticket) %>%
  mutate(Sex = fct_recode(Sex,
                          "0" = "male",
                          "1" = "female")
  ) %>%
  mutate(Sex = as.integer(Sex),
         Pclass = as.integer(Pclass),
         Survived = as.integer(Survived),
         Embarked = as.integer(Embarked)) %>%
  cor(use="complete.obs") %>%
  corrplot(type="lower", diag=FALSE)
