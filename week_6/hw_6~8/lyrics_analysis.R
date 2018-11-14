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
library(shiny)
library(tidyverse)
library(wordcloud)
library(ggbiplot)
library(factoextra)
library(plotly)




#抓出「魔鏡歌詞網」排行榜上所有歌曲所對應的歌詞連結網址
url  <- 'https://mojim.com/twzhot-song.htm'
html <- htmlParse( GET(url) )
xpath= '//*[(@id = "mx5_A") and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]'
url.list <- xpathSApply( html, xpath, xmlAttrs )
url.list
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

#View(corpus[[1]])

#斷詞
mixseg = worker()
jieba_tokenizer = function(d){
  unlist(segment(d[[1]], mixseg))
}
seg = lapply(corpus, jieba_tokenizer)
str(seg)

#詞頻向量
count_token = function(d){
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)
str(tokens)

#詞頻矩陣TDM
n_tag = length(seg)
TDM = tokens[[1]]
tagNames <- list.files('./')
tagNames <- gsub(".txt", "", tagNames) #取代
for( id in c(2:n_tag) ){
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', tagNames[1:id])
}
TDM[is.na(TDM)] <- 0 #將NA填0
str(TDM)


#TDM 轉 TF-IDF
tf <- apply(as.matrix(TDM[,2:(n_tag + 1)]), 2, sum) #直向相加計算總數
idfCal <- function(word_doc, n){ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n_tag + 1)]), 1, idfCal, n <- n_tag)
help('idfCal')
tfidf <- TDM
tempY_tag = matrix(rep(c(as.matrix(tf)), each = length(idf)), 
                   nrow = length(idf))
tempX_tag = matrix(rep(c(as.matrix(idf)), each = length(tf)), 
                   ncol = length(tf), byrow = TRUE)
tfidf[,2:(n+1)] <- (tfidf[,2:(n +1)] / tempY_tag) * tempX_tag
View(tfidf)
str(tfidf)

#找出熱門關鍵詞
freq=rowSums(tfidf[,2:78])
wordfreq= data.frame(tfidf$d, freq)
str(wordfreq)
wordfreq= wordfreq[rev(order(wordfreq$freq)),]
colnames(wordfreq) = c('word', 'tfidfsum')
wordfreq
head(wordfreq,10)
tail(wordfreq,10)

ggplot(wordfreq[1:20,], aes(x = reorder(word, tfidfsum), y =tfidfsum)) + 
  geom_bar(stat = "identity", fill='lightblue') + 
  coord_flip()+
  labs(x='word', y='tfidfsum', title= '熱門關鍵詞')+
  theme(panel.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        axis.text.x = element_text(hjust = 1, size=15),
        axis.text.y = element_text(hjust = 1, size=15),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))

#Plot those frequencies ordered.
plot(sort(wordfreq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

#5. 找前10相似 (input.tag)
rownames(tfidf) = tfidf$d
tfidf <- tfidf[,2:78]
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
input= '漂向北方'
docs.cos.sim_tag <- apply(tfidf , 2, cos, y = tfidf[, c(input)])
sort(docs.cos.sim_tag, decreasing = TRUE)[1:10]

#6. 唐詩三百首文字雲 (所有tag相加)
#BY tf-idf
f_tag <- sort(rowSums(tfidf), decreasing = T)
docs.df_tag <- data.frame(
  word = names(f_tag),
  freq = f_tag
)
row.names(docs.df_tag)=NULL
wordcloud(docs.df_tag$word, docs.df_tag$freq, scale=c(3,0.1),max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=.1, colors=brewer.pal(8,"Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

#PCA
pca= prcomp(tfidf)
summary(pca)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
fviz_eig(pca)
fviz_pca_ind(pca, geom= c("point","text","arrow"), col.ind = "cos2")
fviz_pca_var(pca, col.var = "contrib")

#k-means
k_tag = 7
View(pca)
kmeansData_tag = pca$x[,1:2]
kmeansData_tag = kmeansData_tag[kmeansData_tag[,1] > -0.05, ]

cl_tag <- kmeans(kmeansData_tag, k_tag)
kmeansData_tag <- as.data.frame(kmeansData_tag) 
kmeansData_tag$cl <- as.factor(cl_tag$cluster)

plot_ly(kmeansData_tag, x= ~PC1, y=~PC2, type='scatter',
        mode='text', text=paste0("<b>",rownames(kmeansData_tag),"</b>"), 
        color = ~cl, colors="Set1", textfont = list(size = 14) )
