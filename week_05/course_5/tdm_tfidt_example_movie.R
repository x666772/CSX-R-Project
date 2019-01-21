#Example of creating term document matrices with TF-IDF weights
#Raúl García-Castro (rgarcia@fi.upm.es)

#Load required libraries.
library(tm)
library(ggplot2)

#Set the working directory to text_sentoken
path = "./"

#Load corpus from local files.
#Load the Sentiment polarity dataset version 2.0 from the Movie review data.
#Once unzipped, access the positive reviews in the dataset.
dir = DirSource(paste(path,"pos/",sep=""), encoding = "UTF-8")
dir
corpus = Corpus(dir)

#Check how many documents have been loaded.
length(corpus)

#Access the document in the first entry.
corpus[[1]]

#Define custom stop words for our corpus.
myStopwords = c(stopwords(),"film","films","movie","movies")
myStopwords
stopwords()

#Create a TDM applying TF-IDF weighting instead of term frequency.
#This can be done as in previous cases but passing the weighting = weightTfIdf parameter.
tdm = TermDocumentMatrix(corpus,
                         control = list(weighting = weightTfIdf,
                                        stopwords = myStopwords, 
                                        removePunctuation = T,
                                        removeNumbers = T,
                                        stemming = T))
help("TermDocumentMatrix")

#Take a look at the summary of the TDM.
tdm
head(as.table(tdm))

#Take a look at a subset of the TDM.
inspect(tdm[2005:2015,100:103])

#Analyse how frequently terms appear by summing the content of all terms (i.e., rows).
freq=rowSums(as.matrix(tdm))
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
