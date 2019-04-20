---
title: "魔鏡歌詞網歌詞分析"
author: "Sam Liao"
date: "2018年11月14日"
output: html_document
---


## 問題定義
Task: 我想知道熱門歌曲的歌詞相似處或特性

Experience: 我覺得流行歌的歌詞都差不多

Performance: 是否真的存在工同特徵

## 載入套件包

```r
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
```

## 資料取得：從「魔鏡歌詞網」得排行榜上爬下

抓出對應的歌詞連結網址




























