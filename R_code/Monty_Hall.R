##몬티홀 시뮬레이션
install.packages("tidyverse")
library(tidyverse)

sample(1:10,1) 
c(1:10)[-c(1,3,5)]
sample((1:10)[-c(1,3,5)],2) 
car<-sample(1:3,1) 
initial_pick<-sample(1:3,1) 
monty_hall<-sample(c(1:3)[-c(initial_pick,car)],1) 
the_other<-c(1:3)[-c(initial_pick,monty_hall)] 
result<-ifelse(the_other==car,'swich','stay')
result

#function_example
add<-function(x){
  tibble(x=x,y=x+1)
  }
# function_monty hall
swich_door<-function(try){
  car<-sample(1:3,1)
  initial_pick<-sample(1:3,1)
  monty_hall<-c(1:3)[-c(car,initial_pick)]
  monty_hall<-ifelse(length(monty_hall)==1,monty_hall,sample(c(monty_hall),1))
  the_other<-c(1:3)[-c(initial_pick,monty_hall)]
  tibble(try=try,
         car=car,
         initial_pick=initial_pick,
         monty_hall=monty_hall,
         the_other=the_other,
         result=ifelse(the_other==car,1,0))
}

#map_df example
map_df(1:5,add)

#swich_door repeat
map_df(1:100,swich_door) %>%
  filter(monty_hall!=car&monty_hall!=initial_pick)

map_df(1:100,swich_door) %>%
  summarise(win_pct=mean(result))
