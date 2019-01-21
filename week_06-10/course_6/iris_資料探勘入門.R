#台大電資中心: R統計分析與資料探勘入門—以鳶尾花資料集為例
#source: http://www.cc.ntu.edu.tw/chinese/epaper/0031/20141220_3105.html

iris
#敘述統計
head(iris,5)
mean(iris$Sepal.Length)
round(mean(iris$Sepal.Length),2)
attach(iris)
round(mean(Sepal.Length),2)
summary(iris)

#繪圖功能
plot(Sepal.Length,Sepal.Width) 
plot(Species) 
plot(Species,Sepal.Length)
plot(iris)

plot(Sepal.Length[Species=="setosa"],Petal.Length[Species=="setosa"],
     pch=1 ,col="black", xlim=c(4,8), ylim= c(0,8), main="classified scatter plot",
     xlab="SLen", ylab="PLen")
points(Sepal.Length[Species=="virginica"], Petal.Length[Species== "virginica"],pch=3,col="green")
points(Sepal.Length[Species=="versicolor"], Petal.Length[Species== "versicolor"],pch=2,col="red")
legend(4,8,legend=c("setosa","versicolor","virginica"), col=c(1,2,3), pch=c(1,2,3))

#常態性檢定
shapiro.test(Sepal.Length)
library(nortest)
ad.test(Sepal.Length)
qqnorm(Sepal.Length)
qqline(Sepal.Length,col="red")
 #Sepal.Length的直方圖和常態曲線
hist(Sepal.Length,breaks=seq(4.0,8.0,0.25),prob=TRUE)
curve(dnorm(x,mean(Sepal.Length),sd(Sepal.Length)),4.0,8.0,add=TRUE, col="red")

#雙樣本均數檢定
xtabs(~Species)
setosa=subset(iris,Species=="setosa")
versicolor=subset(iris,Species=="versicolor")
var.test(setosa$Petal.Width, versicolor$Petal.Width)
t.test(setosa$Petal.Width, versicolor$Petal.Width, var.equal=FALSE)
 #方法二
iris2=subset(iris,Species!="virginica")
var.test(iris2$Petal.Width ~ iris2$Species)
t.test(iris2$Petal.Width ~ iris2$Species,var.equal=FALSE, conf=0.95)

#變異數分析
 #H0:μSetosa=μVersicolor=μVirginica
 #H1:至少有一種平均數和其他品種不相等

#迴歸分析
 #model=lm(Y~X1+X2+…+Xk, data=…)
Sepal.Length = 0.65084*Sepal.Width + 0.70913*Petal.Length - 0.55648*Petal.Width + 1.85600

#決策樹資料探勘
n=0.1*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train=iris[-test.index,]
iris.test=iris[test.index,]
library(tree)
iris.tree=tree(Species~ . ,data=iris.train)
iris.tree
plot(iris.tree)
text(iris.tree)

#Random Forest
library(randomForest)
set.seed(777)
iris.rf=randomForest(Species ~ ., data=iris.train,importane=T,proximity=T)
print(iris.rf)
round(importance(iris.rf),2)
names(iris.rf)
(table.rf=iris.rf$confusion)
sum(diag(table.rf)/sum(table.rf))
#predict
(rf.pred=predict(iris.rf,newdata=iris.test))
(table.test=table(Species=species.test,Predicted=rf.pred))
sum(diag(table.test)/sum(table.test))

(iris.urf=randomForest(iris[,-5]))
MDSplot(iris.urf,iris$Species,palette=rep(1,3),pch=as.numeric(iris$Species))
