install.packages(c('twitteR','ROAuth'))
library(twitteR)
library(ROAuth)
## Twitter authentication
consumerKey <- "O1PEoDol15NeMT9aMipuljzOz"
consumerSecret <- "WxMijfX7nptqq261LOf1NY7Iw7r38uE9iTFz9DnNIowJ7nlaou"
accessToken <- "1032167512186675200-V5eAAYm0fmT1jj8pJdyg7WyEgFF1SH"
accessSecret <- "xTP3a5CyGk01GKg639o8vkZXDA6jyyVcC7f4e3iNMdTzj"
setup_twitter_oauth(consumerKey , consumerSecret , accessToken , accessSecret )
## 3200 is the maximum to retrieve
tweets <- userTimeline("RDataMining", n = 3200)

n.tweet <- length(tweets)
