install.packages('jsonlite')
library(jsonlite)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)

# The TPE Bike opendata json url
url <- 'https://od.moi.gov.tw/api/v1/rest/datastore/A01010000C-001051-046'

#Get it with jsonlite package
jsonData <- fromJSON(url, flatten = T)
str(jsonData)

list<-as.list(jsonData$result$records$oc_p1)
docs <- Corpus(VectorSource(list))
docs
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)
docs <- tm_map(docs, toSpace, "區")
docs <- tm_map(docs, toSpace, "市")
docs <- tm_map(docs, toSpace, "鎮")

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:34),]
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

