library(tidyverse)

# Data load
kbo<- read.csv('C:/Users/82105/Desktop/2021-1/R/R_simulation/Function/kbo.csv')%>% as_tibble()
kbo

# Regression -> R-square
lm(formula = 타석당득점 ~ 타율, data = kbo)%>%
  summary()%>%
  .$r.squared

# repeat
a<- c(1,2,3,4,5)
add_one<-function(x){x+1}
for(i in 1:5){
  print(a[i]+1)
}

lapply(a, function(x){x+1}) # list
sapply(a, function(x){x+1}) #vector

#purrr
map(a, function(x){x+1}) #list
map_dbl(a, function(x){x+1}) #vector
map_dbl(a, add_one)
map_dbl(a, ~.x+1)

b<-c(1,22,333,4444,55555)
map_int(b, str_length)
map_dbl(b, str_length)
map_lgl(b, is.numeric)

c<-c('abc','def','ghi')
map_chr(c, ~paste0(.x,'z'))

#Two variables -> map2
d<- c(5,4,3,2,1)
map2(a,d,sum)
map2_dbl(a,d,sum)
map2_dbl(a,d,~.x+.y)

#more variables -> pmap
pmap_dbl(list(a,b,d),~..2-..1+..3)
