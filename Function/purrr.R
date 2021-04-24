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


#dataframe
f<-tibble(a=c(17,23,4,10,11),
          b=c(24,5,6,12,18),
          c=c(1,7,13,19,25),
          d=c(8,14,20,21,2),
          e=c(15,16,22,3,9))
f
#sum of row
f%>%
  mutate(sum=a+b+c+d+e)

#max of row
f%>%
  rowwise()%>%
  mutate(max=max(a,b,c,d,e))

#sum of column
map(f,sum)
map_df(f,sum)
map_df(f,~.x+1)
modify(f,~.x+1)

#Practise KBO
kbo%>%
  select(타율, 출루율,장타력,OPS)%>%
  map(~lm(kbo$타석당득점~.x))%>%
  map(summary)%>%
  map_df('r.squared')

kbo%>%
  select(타율, 출루율,장타력,OPS)%>%
  map_df(~lm(kbo$타석당득점~.x)%>%
           summary()%>%
           .$r.squared)

# 와이드폼 -> 롱폼
kbo %>%
  pivot_longer(cols = 타율:OPS,names_to='기록',values_to='값')

kbo %>%
  pivot_longer(cols = 타율:OPS,names_to='기록',values_to='값')%>%
  group_by(기록)%>%
  nest()-> kbo2

kbo2$data %>%
  set_names(.,kbo2$기록)%>%
  map_df(~lm(타석당득점~값,data=.)%>%
           summary()%>%
           .$r.squared)

#데이터 묶고 풀기
kbo %>%
  pivot_longer(cols = 타율:OPS, names_to = '기록', values_to='값')%>%
  group_by(X10년대,기록)%>%
  nest()%>%
  mutate(model=map(data,~lm(타석당득점~값,data=.)%>%
                     summary()%>%
                     .$r.squared))%>%
  unnest(model)%>%
  select(-data)%>%
  ggplot(aes(X10년대,model,fill=기록))+
  geom_bar(stat = 'identity', position = position_dodge2(reverse = T))+
  scale_fill_viridis_d()
