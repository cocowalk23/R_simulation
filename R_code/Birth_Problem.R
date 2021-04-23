#birth problem

library(tidyverse)

#replacement sampling
sample(10,5,replace = T)
# n=25
sample(365,25,replace = T)
sample(365,25,replace = T) %>%
  duplicated()%>%
  any()

#simulation
map_dbl(1:100000,~sample(365,25,replace = T) %>%
          duplicated()%>%
          any())%>%
  sum()

# How many people do we have to get together for the same birthday?
crossing(a=1:5,b=letters[1:5])
crossing(human=2:68, try=1:10000)%>%
  mutate(birthday=map(human,~ sample(365,.x,replace=T)),
         duplicated=map_lgl(birthday,~.x%>%
                              duplicated()%>%
                              any()))%>%
  group_by(human)%>%
  summarise(probability=mean(duplicated))%>%
  ggplot(aes(x=human, y=probability))+
  geom_line(color = '#00bfa5', lwd=1.25)

            