#We install the "rvest" package to scrape data:
install.packages("rvest")

#Load the library:
library(rvest)

#Load HTML website:
html <- read_html("/rvest-web-scraping-using-r/")

#Include relevant HTML nodes using CSS generator:
marketingtable <- html_nodes(html, ".odd .column-4 , .odd .column-3 , .odd .column-2 , .odd .column-1, .even .column-4 , .even .column-3 , .even .column-2 , .even .column-1")

#Determine table length
length(marketingtable)

#Import table by html_text function
html_text(marketingtable)