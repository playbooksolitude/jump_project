#23-0309 thu 10:42

#
library(tidyverse)
install.packages("wordcloud")
library(wordcloud)
library("RColorBrewer")
library(showtext)
showtext_auto()
getwd()

#
read_csv("~/Downloads/fruit_vegetable.csv") -> word_cloud
word_cloud |> head()
word_cloud$title[1:20]

example("wordcloud")
color <- brewer.pal(8,"Dark2")

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

