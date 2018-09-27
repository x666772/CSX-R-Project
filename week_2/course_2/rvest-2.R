#We install the "rvest" package to scrape data:
install.packages("rvest")

#Load the library:
library(rvest)

#Load HTML website:
html <- read_html("https://rate.bot.com.tw/xrt?Lang=zh-TW")

##Matrix Format

#Include relevant HTML nodes using CSS generator:
exchageratetable <- html_nodes(html, ".print_show")
View(exchageratetable)

#Determine table length
length(exchageratetable)

#Import table by html_text function
html_text(exchageratetable)

#Result
webtable<-html_text(exchageratetable)
View(exchageratetable)


##Data Frame Format

#Structure separate variables according to node
country <- html_nodes(html, ".print_show")
View(country)
cashbid <- html_nodes(html, ".phone-small-font+ .text-right")
View(cashbid)
numberofcampaigns <- html_nodes(html, ".odd .column-3, .even .column-3")
consumerrating <- html_nodes(html, ".odd .column-4, .even .column-4")

#Define separate variables
observationvalues<-html_text(observation)
marketingspendvalues<-html_text(marketingspend)
numberofcampaignsvalues<-html_text(numberofcampaigns)
consumerratingvalues<-html_text(consumerrating)

#Structure data frame and remove heading
df = data.frame(observationvalues, marketingspendvalues, numberofcampaignsvalues, consumerratingvalues)
df2<-df[-1, ]
df2
