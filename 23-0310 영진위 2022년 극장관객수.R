#23-0310 fri 10:50
#영화진흥위원회
#

#
library(tidyverse)
read_csv("./files/KOBIS_2022_edit.csv", skip = 4) -> kobis
colnames(kobis)
kobis |> head()

#전처리
kobis |> 
  select(1:2,12:15,국적,17) -> kobis2

kobis2
kobis2 |> 
  rename("전국매출액" = 3,
         "전국관객수" = 4,
         "서울매출액" = 5,
         "서울관객수" = 6) -> kobis3
(kobis3 |> filter(전국관객수 > 10000) -> kobis4)

#dataset
kobis4 |> arrange(desc(전국관객수))

kobis4 |> 
  mutate(
    all_rank = min_rank(desc(전국관객수))
    ) |> group_by(등급) |> 
  mutate(
    grade_rank = min_rank(desc(전국관객수))
  ) -> kobis5_rank

kobis5_rank |> 
  filter(grade_rank %in% c(1:10)) |> arrange(desc(등급)) |> view()

kobis5_rank |> 
  filter(grade_rank %in% c(1:10)) |> arrange(desc(등급)) -> kobis6

#ggplot
ggplot(data = kobis7, aes(x = 영화명 |> fct_reorder(전국관객수), 
                          y = 전국관객수)) + 
  geom_bar(aes(fill = 등급), stat = "identity") + 
  coord_flip() +
  labs(x = "영화명", y = "극장관객수") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = grade_color2)
  #scale_color_manual(values = grade_color2)

grade_color2 = c("전체관람가" = "yellow",
                "12세이상관람가" = "#a9d98f",
                "15세관람가" = "#ff9500",
                "15세이상관람가" = "#ff9500",
                "청소년관람불가" = "red")
                          
kobis6 |> filter(등급 == "15세관람가")
kobis6 |> filter(등급 == "15세이상관람가")
kobis6 |> filter(등급 != "15세관람가") -> kobis7

ggplot(data = kobis7, aes(x = 영화명 |> fct_reorder(전국관객수), 
                          y = 전국관객수,
                          fill = 등급)) + 
  geom_bar(data = select(kobis7, -등급), fill = "grey", stat = "identity") +
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "영화명", y = "극장관객수") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = grade_color2) +
  facet_grid(.~등급)

#
ggplot(kobis7, aes(영화명 |> fct_reorder(전국관객수),
                   전국관객수)) + 
  geom_bar(data = select(kobis7, -등급),  fill = "grey", stat = "identity") +
  facet_grid(.~등급) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)
  
#geom_point() +

ggplot(mpg, aes(displ, hwy)) +
  geom_point(data = select(mpg, -cyl), colour = "grey") +
  geom_point() + 
  facet_wrap(.~cyl)

kobis7
mpg
  
#coord_flip() +
  labs(x = "영화명", y = "극장관객수") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = grade_color2) +
  
