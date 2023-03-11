#23-0310 fri 10:50
#y축이 지수로 표현될 때

#
library(tidyverse)

#
ggplot(gapminder_NSKorea, aes(year, pop))+ bbc_style() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) 

gapminder_NSKorea
ggplot(gapminder_NSKorea, aes(year, gdpPercap))+
  geom_point(aes(color = country)) #gdp 데이터

ggplot(gapminder_NSKorea, aes(year, gdpPercap,color = country))+
  geom_point()+
  geom_smooth()  #2

range(gapminder_NSKorea$gdpPercap)  
#gdpPercap의 최솟값,최댓값 알아보기
#최솟값: 1030.592/최댓값: 23348.140

ggplot(gapminder_NSKorea, aes(year, gdpPercap,color = country))+
  geom_point()+
  geom_smooth() +
  coord_cartesian(ylim = c(1000, 27000))

