#We install the "rvest" package to scrape data:
install.packages("rvest")

#Load the library:
library(rvest)

#Load HTML website:
html <- read_html("https://rate.bot.com.tw/xrt?Lang=zh-TW")

##Data Frame Format

#Structure separate variables according to node
country <- html_nodes(html, ".print_show")
cashbuy <- html_nodes(html, ".phone-small-font+ .text-right")
cashbid <- html_nodes(html, ".print_hide+ .rate-content-cash.text-right")

#Check Length
length(country)
length(cashbuy)
length(cashbid)


#Define separate variables
countryvalues<-html_text(country)
cashbuyvalues<-html_text(cashbuy)
 temp<-matrix(cashbuyvalues,2,19)
 cashbuytrim<-temp[1,]
cashbidvalues<-html_text(cashbid)


cashbuytrim
#Structure data frame and remove heading
df = data.frame(countryvalues, cashbuytrim, cashbidvalues)
View(df)
