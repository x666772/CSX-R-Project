#Read csv file
library(stringr)
library(dplyr)
data = read.csv("D:/CSX_Lyhs/week_3/hw_3/googleplaystore.csv",encoding='UTF-8')   
View(data)
names(data)[names(data) == 'X.U.FEFF.App'] <- 'App'
str(data)
gsub('^<+|>$','',data$App)
summary(data$Rating)
data$App<-as.character(data$App)
data$App <- str_replace_all(data$App, "[><]", "")
filter(data, Rating>5)
View(as.numeric(data$Rating))
help("as.numeric")
#繪圖物件
library(ggplot2)
ggplot(data, aes(x= Type, y=Rating ))+ geom_point()
     help("ggplot")

#散佈圖圖層
plot2 <- plot + layer(
  geom = "point",
  stat = "identity",
  position = "identity",
  params = list(na.rm = FALSE)
)
plot2
#簡化:
my.plot3 <- ggplot(mydata, aes(X900, X905.67, colour= SPEECH_WITH_NOISE)) + geom_point()
my.plot3

#直方圖圖層
my.plot4 <- ggplot(mydata, aes(x = X900))
my.plot4 <- my.plot4 + layer(
  geom = "bar",
  stat = "bin",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.2,
    na.rm = FALSE
  )
)
my.plot4
##簡化:
my.plot5 <- ggplot(mydata, aes(x = X900))
my.plot5 <- my.plot5 +
  geom_histogram(binwidth = 0.2, fill = "steelblue")
my.plot5

#平滑曲線
my.plot6 <- ggplot(mydata, aes(X900, X905.67, colour= SPEECH_WITH_NOISE)) + geom_point() + geom_smooth()
my.plot6
summary(my.plot6)

#其他
ggplot(mydata, aes(X900, X905.67))+ geom_boxplot()
ggplot(mydata, aes(X900, X905.67, colour= SPEECH_WITH_NOISE))+ geom_line()
install.packages('hexbin')
library(hexbin)
ggplot(mydata, aes(X900, X905.67, colour= SPEECH_WITH_NOISE))+ geom_hex()
View(iris)       
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour= Species))+ geom_hex()
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour= Species))+ geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour= Species))+ geom_point()+ geom_smooth()
ggplot(iris, aes(Sepal.Length, colour= Species))+ geom_histogram()
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour= Species))+ geom_line()
