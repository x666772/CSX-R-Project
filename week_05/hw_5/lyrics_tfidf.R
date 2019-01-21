#熱門歌曲排行

#載入套件包
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
library(dplyr)

#抓出「魔鏡歌詞網」排行榜上所有歌曲所對應的歌詞連結網址
url  <- 'https://edition.cnn.com/politics'
html <- htmlParse( GET(url) )
html
xpath= '//*[contains(concat( " ", @class, " " ), concat( " ", "cd__headline-text", " " ))]'
url.list <- xpathSApply( html, xpath, xmlAttrs )
View(url.list)
url.list= url.list[1,]
#貼上網址
urls= list()
haha= function(a){paste0('https://mojim.com',a)}
urls= lapply(url.list, haha)
urls= unlist(urls)
urls

#爬入歌詞並存檔
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
setwd("D:/CSX_Lyhs/week_5/hw_5/lyrics")
dir = DirSource("./", encoding = "BIG-5")
dir
corpus = Corpus(dir)
length(corpus)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)})
text= unlist(corpus)
write(text,'lyrics.txt')
#View(corpus[[1]])

#斷詞
mixseg = worker()
jieba_tokenizer = function(d){
  unlist(segment(d[[1]], mixseg))
}
seg = lapply(corpus, jieba_tokenizer)
str(seg)

#詞頻向量
freqFrame = as.data.frame(table(unlist(seg)))
tail(freqFrame)

#詞頻矩陣TDM
d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
tf <- as.matrix(tdm)
DF <- tidy(tf)
DF


#TDM 轉 TF-IDF
N = tdm$ncol
tf <- apply(tdm, 2, sum) 
idfCal <- function(word_doc){ 
  log2( N / nnzero(word_doc) )   
}
idf <- apply(tdm, 1, idfCal)
doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm)){
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}
findZeroId = as.matrix(apply(doc.tfidf, 1, sum)) # 1=對每個Row
tfidfnn = doc.tfidf[-which(findZeroId == 0),]
tfidfnn[1:7,1:7]

#找出熱門關鍵詞
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