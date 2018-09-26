#We install the "rvest" package to scrape data:
install.packages("rvest")

#Load the library:
library(rvest)

#Load HTML website:
html <- read_html("https://www.michaeljgrogan.com/rvest-web-scraping-using-r/")

##Matrix Format

#Include relevant HTML nodes using CSS generator:
marketingtable <- html_nodes(html, ".odd .column-4 , .odd .column-3 , .odd .column-2 , .odd .column-1, .even .column-4 , .even .column-3 , .even .column-2 , .even .column-1")

#Determine table length
length(marketingtable)

#Import table by html_text function
html_text(marketingtable)

#Result
webtable<-html_text(marketingtable)
webtable


##Data Frame Format

#Structure separate variables according to node
observation <- html_nodes(html, ".odd .column-1, .even .column-1")
marketingspend <- html_nodes(html, ".odd .column-2, .even .column-2")
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
