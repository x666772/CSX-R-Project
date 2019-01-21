#WEB CRAWLER
library(rvest)
library(magrittr)
title=read_html("https://technews.tw/", options = 'NOBLANKS') %>%
  html_nodes(".entry-title a , #album_test1 a , #album_test1 h3") %>%
  html_text() %>% iconv("UTF-8")
str(title)
View(title)
title

#Cleaning
clean <- na.exclude(title)
clean <- gsub('\n', '', clean)
clean <- gsub('\t', '', clean)
clean <- gsub('也', '', clean)
clean <- gsub('及', '', clean)
clean <- gsub('只', '', clean)
clean <- gsub('了', '', clean)
clean <- gsub('仍', '', clean)
clean <- gsub('的', '', clean)
clean <- removeNumbers(clean)
clean

#Phrasing and Clean meaningless text
library("jiebaR")
library("tm")
Sys.setlocale(category = "LC_ALL", locale = "cht")
cc = worker()
new_user_word(cc,'Windows 10',"n")
new_user_word(cc,'戰機',"n")
cc[clean]

tabledata <-table(cc[clean])
tabledata

data.frame(tabledata)

#Show wordcloud
install.packages('wordcloud2')
library(wordcloud2)
wordcloud2(tabledata)
