
#-------------------Program---------------------------
# 載入套件包
source('w2v_global.R', local = TRUE)

##資料前處理
#建立結構與清洗
dir = DirSource("lyrics/", encoding = "BIG-5")
dir
corpus = Corpus(dir)

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word) })
corpus <- tm_map(corpus, function(word) {
  gsub("妳", "你", word) })
corpus <- tm_map(corpus, function(word) {
  gsub("\n", "", word) })

#斷詞
mixseg = worker()
new_user_word(mixseg,'為了',"n")
new_user_word(mixseg,'再也',"n")
new_user_word(mixseg,'讓人',"n")
new_user_word(mixseg,'想像',"n")
new_user_word(mixseg,'也不',"n")
new_user_word(mixseg,'這是',"n")
new_user_word(mixseg,'再這樣',"n")
jieba_tokenizer = function(d){
  unlist(segment(d[[1]], mixseg))
}
seg = lapply(corpus, jieba_tokenizer)
str(seg)
seg.ma= matrix(unlist(seg))
write(seg.ma,'seg.ma.txt')
TrainingFile2 <- system.file('ex', "seg.ma.txt", package = "tmcn.word2vec")
help("system.file")
ModelFile2 <- file.path(tempdir(), "output", "model2.bin")
??train_file
res2 <- word2vec(TrainingFile2, ModelFile2)

help("closest_to")
nearest_to(seg,seg$`01.txt`)
help(write)
write(seg,'seg.txt')



#詞頻向量
count_token = function(d){
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

#詞頻矩陣TDM
n_tag = length(seg)
n_tag
TDM = tokens[[1]]
tagNames=(c('Three Pass(3 pass)','一個人去巴黎','十年','女孩','不染','不僅僅是喜歡','天后','王牌冤家(& ice)','他不懂','以後別做朋友','平凡之路','末班車(Last Train)','目不轉睛','再見煙火','如果雨之後','年少有為','成全','有何不可','但願人長久','你好不好','你要的全拿走','你還要我怎樣','告白氣球','我以為','我在呢','我好想好想你','我們','我們不一樣','戒菸','那些年','那些你很冒險的夢','往後餘生','朋友','南山南','屋頂','星球墜落','洋蔥','修煉愛情','剛好遇見你','浪費','消愁','烏雲中','紙短情長','帶你去旅行','情非得已','涼涼(& 張碧晨)','畢竟深愛過','紳士','雪落下的聲音','魚仔','幾分之幾','斑馬斑馬','最笨的人是我','童話','等你下課 (& 楊瑞代)','答案','想你的夜','愛我別走','愛要怎麼說出口','愛過你這件事','煙火裡的塵埃','落葉歸根','像我這樣的人','漂向北方(& 王力宏)','漂向北方','演員','說謊(FAIRY TALE)','稻香','醉赤壁','學貓叫(& 小潘潘)','擁抱你離去','獨家記憶','輸了你 贏了世界又如何','謝謝妳愛我','離人愁','寶貝兒','讓我留在你身邊'))
for( id in c(2:n_tag) ){
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', tagNames[1:id])
}
TDM[is.na(TDM)] <- 0 #將NA填0

#TDM 轉 TFIDF
tf <- apply(as.matrix(TDM[,2:(n_tag + 1)]), 2, sum) #直向相加計算總數
idfCal <- function(word_doc, n){ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n_tag + 1)]), 1, idfCal, n <- n_tag)
tfidf <- TDM
tempY_tag = matrix(rep(c(as.matrix(tf)), each = length(idf)), 
                   nrow = length(idf))
tempX_tag = matrix(rep(c(as.matrix(idf)), each = length(tf)), 
                   ncol = length(tf), byrow = TRUE)
tfidf[,2:(n+1)] <- (tfidf[,2:(n +1)] / tempY_tag) * tempX_tag

##EDA
#找出熱門關鍵詞
freq=rowSums(tfidf[,2:n_tag+1])
wordfreq= data.frame(tfidf$d, freq)
wordfreq= wordfreq[rev(order(wordfreq$freq)),]
colnames(wordfreq) = c('word', 'tfidfsum')
ggplot(wordfreq[1:10,], aes(x = reorder(word, tfidfsum), y =tfidfsum)) + 
  geom_bar(stat = "identity", fill='lightblue') + 
  coord_flip()+
  labs(x='word', y='tfidfsum', title= '熱門關鍵詞')+
  theme(panel.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        axis.text.x = element_text(hjust = 1, size=15),
        axis.text.y = element_text(hjust = 1, size=15),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))

#IFIDF分布概況
plot(sort(wordfreq$tfidfsum, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

#文字雲
rownames(tfidf) = tfidf$d
tfidf <- tfidf[,2:n_tag+1]
f_tag <- sort(rowSums(tfidf), decreasing = T)
docs.df_tag <- data.frame(
  word = names(f_tag),
  freq = f_tag
)
row.names(docs.df_tag)=NULL
wordcloud(docs.df_tag$word, docs.df_tag$freq, scale=c(3,0.0001),max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=.1, colors=brewer.pal(8,"Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

##關聯分析
#找歌詞相似的歌
songs= colnames(tfidf)
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
input_sim= '漂向北方'
docs.cos.sim_tag <- apply(tfidf , 2, cos, y = tfidf[, c(input_sim)])
sim_songs= names(sort(docs.cos.sim_tag, decreasing = TRUE)[1:10])
sim_songs= as.data.frame(sim_songs)

#TFIDF -> PCA
pca= prcomp(tfidf)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
fviz_eig(pca)
fviz_pca_ind(pca, geom= c("point","text","arrow"), col.ind = "cos2")
fviz_pca_var(pca, col.var = "contrib")

#k-means分群(歌詞)
k_word = 7
kmeansData_word = pca$x[,1:2]
#kmeansData_word = kmeansData_word[kmeansData_word[,1] > -0.05, ]

cl_word <- kmeans(kmeansData_word, k_word)
kmeansData_word <- as.data.frame(kmeansData_word) 
kmeansData_word$cl <- as.factor(cl_word$cluster)

plot_ly(kmeansData_word, x= ~PC1, y=~PC2, type='scatter',
        mode='text', text=paste0("<b>",rownames(kmeansData_word),"</b>"), 
        color = ~cl, colors="Set1", textfont = list(size = 14) )

#k-means分群(歌曲)
k_song = 7
kmeansData_song = pca$rotation[,1:2]
#kmeansData_song = kmeansData_song[kmeansData_song[,1] > -0.05, ]

cl_song <- kmeans(kmeansData_song, k_song)
kmeansData_song <- as.data.frame(kmeansData_song) 
kmeansData_song$cl <- as.factor(cl_song$cluster)

plot_ly(kmeansData_song, x= ~PC1, y=~PC2, type='scatter',
        mode='text', text=paste0("<b>",rownames(kmeansData_song),"</b>"), 
        color = ~cl, colors="Set1", textfont = list(size = 14) )







#---------------- UI -----------------------------
ui <- navbarPage(
  
  theme = shinythemes::shinytheme("flatly"),
  
  # Application title
  "歌詞分析",
  
  tabPanel(
    "簡介&TEP",
    tags$h2("簡介&TEP"),br(),
    tags$h4("「魔鏡歌詞網」是台灣最大的免費歌詞網站，我也是忠實用戶之一，因此想利用這資源，"),br(),
    tags$h4("分析熱門歌曲排行中的歌曲歌詞，彼此是否存在什麼特性或關聯性"),br(),
    tags$h4("Task- 我想知道熱門歌曲的歌詞相似處或特性"),br(),
    tags$h4("Experience- 我覺得流行歌的歌詞都差不多"),br(),
    tags$h4("Performance- 是否真的存在工同特徵"),br()
  ),
  tabPanel(
    "EDA",
    tags$h2("關鍵歌詞雲"),br(),
    sidebarPanel(
      sliderInput("wc_max",
                  "字詞數量",
                  min = 1,
                  max = 50,
                  value =10)
    ),
    mainPanel(
      plotOutput("WordCloud_1")
    ),
    tags$h4("雖然結果中出現許多一般會被當成「停用字」的詞彙出現，但考量到歌詞並非文章，"),
    tags$h4("穿插無意義的字也可能對歌曲傳唱度有幫助，因此決定先保留原始斷辭結果"),br()
  ),
  tabPanel(
    "TFIDF與PCA",
    tags$h4("TFIDF分布"),br(),
    plotOutput("Plot_1"),
    tags$h4(" "),br(),
    tags$h4("PCA分布"),br(),
    plotOutput("Plot_2")
  ),
  tabPanel(
    "歌詞間的關聯性",
    tags$h1("歌詞間關聯性"),
    sidebarPanel(
      tags$h4("用K-means分群，我們可以看到相似歌詞的分布"),
      numericInput("k1",
                   "Number of k:",
                   min = 1,
                   max = 20,
                   value = 7),
      hr(),
      helpText('(結果要稍等歐)')
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("Plotly_KM1")
    )
  ),
  tabPanel(
    "歌曲間的關聯性",
    tags$h1("歌曲間的關聯性"),
    sidebarPanel(
      tags$h4("用K-means分群，我們可以看到用詞相似的歌曲"),
      numericInput("k2",
                   "Number of k:",
                   min = 1,
                   max = 20,
                   value = 7),
      hr(),
      helpText('(結果要稍等歐)')
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("Plotly_KM2")
    )
  ),
  tabPanel(
    "最相關歌曲",
    tags$h2('最相關歌曲'),br(),
    sidebarPanel(
      selectInput("sim", "歌曲:", 
                  choices=songs),
      hr(),
      helpText("列出用詞最相似的十首歌")
      
    ),
    mainPanel(
      tableOutput("text")
    )
  )
)

#----------------- server -----------------------------
server <- function(input, output) {
  output$WordCloud_1 <- renderPlot({
    wordcloud(docs.df_tag$word, docs.df_tag$freq, scale=c(3,0.000001),max.words= input$wc_max,
              random.order=FALSE, random.color=TRUE, 
              rot.per=.1, colors=brewer.pal(8,"Dark2"),
              ordered.colors=FALSE,use.r.layout=FALSE,
              fixed.asp=TRUE)
  })
  output$Plot_1 <- renderPlot({
    plot(sort(wordfreq$tfidfsum, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")
  })
  output$Plot_2 <- renderPlot({
    fviz_eig(pca)
  })
  output$Plotly_KM1 <- renderPlotly({
    k_word = input$k1
    
    kmeansData_word = pca$x[,1:2]
    #kmeansData_word = kmeansData_word[kmeansData_word[,1] > -0.05, ]
    
    cl_word <- kmeans(kmeansData_word, k_word)
    kmeansData_word <- as.data.frame(kmeansData_word) 
    kmeansData_word$cl <- as.factor(cl_word$cluster)
    
    plot_ly(kmeansData_word, x= ~PC1, y=~PC2, type='scatter',
            mode='text', text=paste0("<b>",rownames(kmeansData_word),"</b>"), 
            color = ~cl, colors="Set1", textfont = list(size = 14) )
  })
  output$Plotly_KM2 <- renderPlotly({
    k_song = input$k2
    kmeansData_song = pca$rotation[,1:2]
    #kmeansData_song = kmeansData_song[kmeansData_song[,1] > -0.05, ]
    
    cl_song <- kmeans(kmeansData_song, k_song)
    kmeansData_song <- as.data.frame(kmeansData_song) 
    kmeansData_song$cl <- as.factor(cl_song$cluster)
    
    plot_ly(kmeansData_song, x= ~PC1, y=~PC2, type='scatter',
            mode='text', text=paste0("<b>",rownames(kmeansData_song),"</b>"), 
            color = ~cl, colors="Set1", textfont = list(size = 14) )
  })
  output$text <- renderTable({
    input_sim= input$sim
  docs.cos.sim_tag <- apply(tfidf , 2, cos, y = tfidf[, c(input_sim)])
  sim_songs= as.data.frame(names(sort(docs.cos.sim_tag, decreasing = TRUE)[1:10]))
  colnames(sim_songs)<- NULL
  sim_songs
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#tmp.enc <- options()$encoding
#options(encoding = "UTF-8")
#deployApp()
#options(encoding = tmp.enc)

