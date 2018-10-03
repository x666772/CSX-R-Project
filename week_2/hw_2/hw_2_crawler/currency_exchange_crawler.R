#We install the "rvest" package to scrape data:
install.packages("rvest")
install.packages('stringr')

#Load the library:
library(rvest)
library(stringr)

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
countryvalues<-str_replace_all(countryvalues, "[\r\n]", "")
countryvalues<-str_replace_all(countryvalues, " ", "")
cashbuyvalues<-html_text(cashbuy)
temp<-matrix(cashbuyvalues,2,19)
cashbuyvalues<-temp[1,]
cashbidvalues<-html_text(cashbid)

#Structure data frame and remove heading
df = data.frame(countryvalues, cashbuyvalues, cashbidvalues)
df
View(df)
