#23-0309 thu 10:42

#
library(tidyverse)

#wordcloud
#install.packages("wordcloud") 인스톨 필요 #주석 제거 후 실행

library(wordcloud)
library("RColorBrewer")    #컬러 예쁘게

library(showtext) #한글일 경우 실행 필요
showtext_auto()


#
read_csv("~/Downloads/fruit_vegetable.csv") -> word_cloud
word_cloud |> head()
word_cloud$title[1:20]

example("wordcloud")
color <- brewer.pal(11,"Spectral")
brewer.pal.info


args(wordcloud)
wordcloud(words = word_cloud$title,
          freq = word_cloud$count,
          min.freq = 2,
          max.words = 105,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = color,
          use.r.layout = T)

#unlist
starwars -> starwars2
colnames(starwars)
unlist(starwars$films) -> starwars2$films

table(starwars$sex) |> sort()
args(sort)
example(sort)

#
word_cloud
mpg
flights |> group_by(dest) |> 
  summarise(n = n()) -> word_cloud2

wordcloud(words = word_cloud2$dest,
          freq = word_cloud2$n,
          min.freq = 10,
          max.words = 105,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.3),
          #colors = color,
          use.r.layout = T)
