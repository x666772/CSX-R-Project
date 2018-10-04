#Read csv file
library(stringr)
library(dplyr)
data = read.csv("D:/CSX_Lyhs/week_3/hw_3/googleplaystore.csv",encoding='UTF-8')   
View(data)
names(data)[names(data) == 'X.U.FEFF.App'] <- 'App'
str(data)

data$Reviews<-as.numeric(as.character(data$Reviews))
data$Size<-as.character(data$Size)
data$Size<-str_replace_all(data$Size, 'M','')
data$Size<-as.numeric(data$Size)

data$Installs<-as.character(data$Installs)
data$Installs<-gsub('[[:punct:]]','',data$Installs)
#View(data$Installs)
data$Installs<-as.numeric(data$Installs)

data$Price<-as.character(data$Price)
data$Price<-substr(data$Price,2,6)
data$Price<- as.numeric(data$Price)
data$Price[is.na(data$Price)] <-0
data$Price

#繪圖物件
library(ggplot2)
ggplot(data, aes(x= Rating, y=Price ))+ geom_point()
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
