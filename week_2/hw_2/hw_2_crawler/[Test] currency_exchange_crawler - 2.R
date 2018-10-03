Sys.getlocale("LC_ALL")

install.packages("XML")
install.packages("RCurl")
install.packages("httr")

rm(list=ls(all.names=TRUE)) #清空記憶體與載入套件
library(XML)
library(RCurl)
library(httr)

urlPath <- "https://rate.bot.com.tw/xrt?Lang=zh-TW.html"
temp    <- getURL(urlPath, encoding = "big5")
xmldoc  <- htmlParse(temp)
title   <- xpathSApply(xmldoc, "//div[@class=\"hidden-phone.print_show\"]", xmlValue)
title