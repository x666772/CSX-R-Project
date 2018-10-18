#魔鏡歌詞網

#載入所需的套件包
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(rvest)
library(magrittr)
library(ggplot2)
library(tidytext)
library(stats)
library(proxy)
library(readtext)
library(slam)
library(Matrix)




#抓出排行榜上所有歌詞頁面連結所對應的網址
url  <- 'https://mojim.com/twzhot-song.htm'
html <- htmlParse( GET(url) )
xpath= '//*[(@id = "mx5_A") and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]'
url.list <- xpathSApply( html, xpath, xmlAttrs )
url.list
url.list= url.list[1,]
#黏上前段
urls= list()
haha= function(a){paste0('https://mojim.com',a)}
urls= lapply(url.list, haha)
urls= unlist(urls)
urls

#用網址載入歌詞
library(dplyr)
xpath2= '//*[(@id = "fsZx3")]'
xpath3= '//*[(@id = "fsZx2")]'
getdoc <- function(url){
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, xpath2, xmlValue )
  title= xpathSApply(html, xpath3, xmlValue)
  name <- paste0(title, ".txt")
  write(doc, name)
}
lapply(urls,getdoc)

#建立文本資料結構與基本文字清洗
dir = DirSource("./", encoding = "BIG-5")
dir
corpus = Corpus(dir)
length(corpus)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)})

#View(corpus[[1]])

# 斷詞
mixseg = worker()
jieba_tokenizer = function(d){
  unlist(segment(d[[1]], mixseg))
}
seg = lapply(corpus, jieba_tokenizer)
n = length(seg)
n
#詞頻向量
freqFrame = as.data.frame(table(unlist(seg)))
str(freqFrame)

#詞頻矩陣
d.corpus <- Corpus(VectorSource(seg))
d.corpus
tdm <- TermDocumentMatrix(d.corpus)
inspect(tdm)
print( tf <- as.matrix(tdm) )
DF <- tidy(tf)
DF

#將已建好的 TDM 轉成 TF-IDF
View(tdm)
N = tdm$ncol # Column數(資料數)
tf <- apply(tdm, 2, sum) # 2= 對每個Column 
tf #每筆資料的資料量
idfCal <- function(word_doc){ 
  log2( N / nnzero(word_doc) )    #'nnzero' = number of nonzero entries
}
idf <- apply(tdm, 1, idfCal) # 1= 對每個Row

doc.tfidf <- as.matrix(tdm)
doc.tfidf

for(x in 1:nrow(tdm)){
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

findZeroId = as.matrix(apply(doc.tfidf, 1, sum)) # 1=對每個Row
findZeroId
tfidfnn = doc.tfidf[-which(findZeroId == 0),]
View(tfidfnn)

#
freq=rowSums(as.matrix(tfidfnn))
head(freq,10)
tail(freq,10)
freq

#Plot those frequencies ordered.
plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

#See the ten most frequent terms.
tail(sort(freq),n=10)

#Show most frequent terms and their frequencies in a bar plot.
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df
hfp.df$names <- rownames(hfp.df) 
hfp.df
help(reorder)
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")
