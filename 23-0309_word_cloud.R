#23-0309 thu

#기본 라이브러리
library(tidyverse)

#워드 크라우드 라이브러리
#install.packages("wordcloud") 인스톨 #주석 제거 후 실행
library("RColorBrewer")    #컬러 적용
library(wordcloud)         #word cloud 패키지

#한글일 경우 실행 필요
library(showtext) 
showtext_auto()

#사용할 데이터셋
library(nycflights13)

#dataset 만들기---------------
#nycflight 출발공항 dest 활용

flights |> group_by(dest) |> 
  summarise(n = n()) -> word_cloud2

#예쁜 컬러 선택
brewer.pal.info  #저는 "Spectral", 11가지 색
color <- brewer.pal(9,"Set1")

#워드 클라우드 만들기
wordcloud(words = word_cloud2$dest,
          freq = word_cloud2$n,
          min.freq = 10,
          max.words = 105,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = color,
          use.r.layout = T)

#billboard
(billboard |> pivot_longer(cols = starts_with("wk"),
                          values_to = "rank",
                          names_to = "weeks") -> billboard_2_pivot)

#group_by datasets
billboard_2_pivot |> group_by(artist) |> summarise(n = n()) -> billboard_3

#wordcloud
wordcloud(words = billboard_3$artist,
          freq = billboard_3$n,
          min.freq = 10,
          max.words = 300,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.3),
          use.r.layout = T,
          colors = color)

color <- brewer.pal(8,"Dark2")
brewer.pal.info

#1등 많이 한 사람
(billboard_2_pivot |> filter(rank == 1) |> group_by(artist) |> 
  summarise(n = n()) |> arrange(desc(n)) -> billboard_3_no1)

wordcloud(words = billboard_3_no1$artist,
          freq = billboard_3_no1$n,
          min.freq = 1,
          max.words = 100,
          random.order = F,
          rot.per = .0,
          scale = c(4,1),
          use.r.layout = F,
          colors = color)

#top10
(billboard_2_pivot |> filter(rank %in% c(1:10)) |> group_by(artist) |> 
    summarise(n = n()) |> arrange(desc(n)) -> billboard_3_top10)

wordcloud(words = billboard_3_top10$artist,
          freq = billboard_3_top10$n,
          min.freq = 1,
          max.words = 100,
          random.order = F,
          rot.per = .0,
          scale = c(3,.5),
          use.r.layout = F,
          colors = color)

billboard
(billboard_2_pivot |> filter(rank %in% c(1:10)) |> group_by(track) |> 
    summarise(n = n()) |> arrange(desc(n)) -> billboard_3_track)


































