#Diamond範例
View(diamonds)

#資料框
ggplot(diamonds, aes(carat, price, colour = cut))
my.plot <- ggplot(diamonds, aes(carat, price, colour = cut))

#散佈圖圖層
my.plot + layer(
  geom = "point",
  stat = "identity",
  position = "identity",
  params = list(na.rm = FALSE)
)

#長條圖
ggplot(diamonds, aes(x = carat))
my.plot2 <- ggplot(diamonds, aes(x = carat))
my.plot2 <- my.plot2 + layer(
  geom = "bar",
  stat = "bin",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.2,
    na.rm = FALSE
  )
)
my.plot2
#簡寫
ggplot(diamonds, aes(x = carat))+
  geom_histogram(binwidth = 0.2, fill = "steelblue")


#ggplot 與 qplot 的寫法比較。
# 做法一
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) +
  geom_point()
# 做法二
qplot(sleep_rem / sleep_total, awake, data = msleep)

#入平滑曲線的範例：
# 做法一
qplot(sleep_rem / sleep_total, awake, data = msleep) +
  geom_smooth()
# 做法二
qplot(sleep_rem / sleep_total, awake, data = msleep,
      geom = c("point", "smooth"))
# 做法三
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) +
  geom_point() + geom_smooth()

#summary
my.plot4 <- ggplot(msleep, aes(sleep_rem / sleep_total, awake))
summary(my.plot4)
my.plot4 <- my.plot4 + geom_point()
summary(my.plot4)

#以圖層當變數
bestfit <- geom_smooth(method = "lm", se = F,
                       color = alpha("steelblue", 0.5), size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit