#用 各個城市的位置去做查詢設定
#以 LA 為例

#0基本設定
rm(list=ls(all.names=TRUE)) #remove the list
library(devtools)
library(twitteR)
library(data.table)
library(maps)
library(mapproj)
library(plyr)
library(reshape2)
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)

#1載入API
consumerKey <- "O1PEoDol15NeMT9aMipuljzOz"
consumerSecret <- "WxMijfX7nptqq261LOf1NY7Iw7r38uE9iTFz9DnNIowJ7nlaou"
accessToken <- "1032167512186675200-V5eAAYm0fmT1jj8pJdyg7WyEgFF1SH"
accessSecret <- "xTP3a5CyGk01GKg639o8vkZXDA6jyyVcC7f4e3iNMdTzj"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

#1找尋設定
tweets <- searchTwitter('Taiwan', n=50, since = '2018-10-9', until = '2018-10-11', locale = 'UTF-8')
help("searchTwitter")
View(tweets)
tweets.list <- twListToDF(tweets)
View(tweets.list)

docs <- Corpus(VectorSource(tweets.list$text))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)
