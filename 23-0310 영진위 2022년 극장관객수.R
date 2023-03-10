#23-0310 fri 10:50
#영화진흥위원회

#import
library(tidyverse)
read_csv("./files/KOBIS_2022_edit.csv", skip = 4) -> kobis
colnames(kobis)
kobis |> head()

#전처리
(kobis |> 
  select(1:2,12:15,국적,17) -> kobis2)

  #컬럼명 변경
(kobis2 |> 
  rename("전국매출액" = 3,
         "전국관객수" = 4,
         "서울매출액" = 5,
         "서울관객수" = 6) -> kobis3)

  #자잘한 성인 영화 잘라내기
ggplot(kobis3, aes(영화명 |> fct_reorder(desc(전국관객수)), 
                   전국관객수)) + 
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = length(kobis3$영화명)/2, y = 10000000, 
           label = length(kobis3$영화명))

  #10000만명 이하 잘라내기
(kobis3 |> filter(전국관객수 > 10000) -> kobis4)

ggplot(kobis4, aes(영화명 |> fct_reorder(desc(전국관객수)), 
                   전국관객수)) + 
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = length(kobis4$영화명)/2, y = 10000000, 
           label = length(kobis4$영화명))



#dataset 전체 인기순위와 등급별 인기 순위
(kobis4 |> 
  mutate(
    all_rank = min_rank(desc(전국관객수))
    ) |> group_by(등급) |> 
  mutate(
    grade_rank = min_rank(desc(전국관객수))
  ) -> kobis5_rank)
kobis5_rank |> view()

  #등급별 top 10
(kobis5_rank |> 
  filter(grade_rank %in% c(1:10)) |> arrange(desc(등급)) -> kobis6)

  #15세 관람가 1편 제외 #큐어
kobis6 |> filter(등급 == "15세관람가")
kobis6 |> filter(등급 == "15세이상관람가")
kobis6 |> filter(등급 != "15세관람가") -> kobis7

#ggplot --------------------------------------------------------- 전체

  #색깔 지정 노랑 ~ 빨강
grade_color2 = c("전체관람가" = "yellow",
                 "12세이상관람가" = "#a9d98f",
                 "15세관람가" = "#ff9500",
                 "15세이상관람가" = "#ff9500",
                 "청소년관람불가" = "red")

ggplot(data = kobis7, aes(x = 영화명 |> fct_reorder(전국관객수), 
                          y = 전국관객수)) + 
  geom_bar(aes(fill = 등급), stat = "identity") + 
  coord_flip() +
  labs(x = "영화명", y = "극장관객수") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = grade_color2) + 
  theme(legend.position = "top")
  #scale_color_manual(values = grade_color2)

# 면분할 ---------------------------------------------
ggplot(kobis7, aes(영화명 |> fct_reorder(전국관객수),
                   전국관객수)) + 
  geom_bar(data = select(kobis7, -등급),  fill = "grey", stat = "identity") +
  facet_grid(.~등급) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) + #y축 지수표현 해결
  labs(x = "영화명")

#top20 국가별 극장관객수 --------------------------------
kobis5_rank |> filter(all_rank %in% c(1:20)) |> 
ggplot(aes(영화명 |> fct_reorder(전국관객수),
                   전국관객수)) + 
  geom_bar(fill = "grey", stat = "identity") +
  facet_grid(.~국적) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "영화명")+
  geom_label(aes(label = round(전국관객수/10000,0)))

#영진위 word cloud
(kobis5_rank |> filter(all_rank %in% c(1:20)) -> kobis8)
(kobis8 |> 
  separate(영화명, into = c("영화명", "부제"), sep = ":", convert = T) |> 
  separate(영화명, into = c("영화명", "부제"), sep = "The ", convert = T) -> kobis9)

library(wordcloud)
wordcloud(words = kobis9$영화명,
          freq = kobis9$전국관객수,
          scale = c(4, 0.5),
          min.freq = 100000,
          max.words = Inf,
          random.order = F,
          random.color = T,
          rot.per = .1,
          colors = brewer.pal(8, "Dark2"))

#부제 제거 후 상위 20개 면분할
ggplot(kobis9, aes(영화명 |> fct_reorder(전국관객수),
                   전국관객수)) + 
  geom_bar(fill = "grey", stat = "identity") +
  facet_grid(.~등급) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) + #y축 지수표현 해결
  labs(x = "영화명")
