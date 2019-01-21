x <- 1000:9999
answer <- sample(x, size = 1, replace=T)
answer
answer <- as.character(answer)
answer
answer <- c(substr(answer,1,1),substr(answer,2,2),substr(answer,3,3),substr(answer,4,4))
answer
help('substr')
time <- 0
repeat{
  
  enter <- as.numeric(readline("請輸入一組四位數字:"))
  while(enter < 1000 || enter >9999){
    cat("輸入錯誤!")
    time <- time + 1
    enter <- as.numeric(readline("請輸入一組四位數字:"))
  }
  
  enter <- as.character(enter)
  enter <- c(substr(enter,1,1),substr(enter,2,2),substr(enter,3,3),substr(enter,4,4))
  
  a <- 0
  b <- 0
  for(i in 1:4){
    if(enter[i] == answer[i]){
      a <- a + 1
    }else{
      for(j in 1:4){
        if(enter[j] == answer[i] ){
          b <- b + 1
        }
      }
    }
  }
  cat(a,"A",b,"B","\n")
  time <- time + 1
  if(a == 4){cat("猜對!","共猜",time, "次", "\n")
    break}
}

help('cat')
