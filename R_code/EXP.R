library(tidyverse)
exp<-read.csv(file = "C:/Users/82105/Desktop/2021-1/R/EXP.csv", header = T)

level<- 1:10
map_dbl(1:100,~sample(level,1,prob = exp$x141))
exp_f<-function(try){
  x<-141
  i<-0
  while(x<200){
    i<-i+1
    x<-x+sample(level,1,prob = exp[,paste0("x",x)])
  }
  tibble(try=i,
         x=x)
}

exp_l<-function(Z){
  x<-141
  for(i in 1:(z-1)){
    i<-i+1
    x<-x+sample(level,1,prob = exp[,paste0("x",x)])
  }
  tibble(try=i,
         x=x)
}



map_df(1:1000, exp_f)%>%
  summarise("익성비 몇개 필요?"=mean(try))
