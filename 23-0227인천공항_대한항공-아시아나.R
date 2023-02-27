#23-0227 mon office 12:22
#인천공항 12월

#
library(tidyverse)
library(readxl)
#install.packages("showtext")
library(showtext)
showtext_auto()


getwd()
read_excel("/Users/yohan_15/Documents/jump/jump_project/files/arr_22_12.xlsx") -> icn_arr
read_excel("/Users/yohan_15/Documents/jump/jump_project/files/dep_22_12.xlsx") -> icn_dep

#
icn_arr;icn_dep

icn_arr |> colnames()
icn_arr |> head() |> view()

#
(icn_arr |> select(-10) |> 
    separate(날짜, into = c("year", "month"), sep = 4, convert = T) |> 
    separate(month, into = c("month", "day"), sep = 2, convert = T) |> 
  separate(계획시간, into = c("p_hour", "p_min"), sep = ":", convert = T) |> 
  separate(도착시간, into = c("hour", "min"), sep = ":", convert = T) -> icn_arr_2_sep)

icn_arr_2_sep |> group_by("출발공항명") |> 
  mutate(delay = (p_min - min), .before = 1)

icn_arr |> select(-10) |> 
  separate(날짜, into = c("year", "month"), sep = 4, convert = T) |> 
  separate(month, into = c("month", "day"), sep = 2, convert = T) |> 
  separate(계획시간, into = c("p_hour", "p_min"), sep = ":", convert = T) |> 
  unite(p_hour, p_min) |> head() |> view()

icn_arr |> select(-10) |> 
    ymd(icn_arr$날짜)

icn_arr

# -------------------------------------- 인천공항 최다 노선 (-> 인천)
icn_arr |> group_by(출발공항명) |> 
  summarise(n = n()) |> arrange(desc(n))

icn_arr |> group_by(출발공항명, 항공사) |> 
  summarise(n = n()) |> arrange(desc(n))

icn_arr |> group_by(출발공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:5)) |> arrange(rank) -> icn_arr_2_top5

ggplot(data = icn_arr_2_top5, 
       aes(x = 출발공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + #coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "2022년 12월 인천공항 도착",
       subtitle = "최다 노선 top 5")
ggsave("인천공항도착_22년12월top5.png")



# ------------------------------------------------ top 15
icn_arr |> group_by(출발공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:5)) |> arrange(rank) -> icn_arr_2_top5

colnames(icn_dep)

  #대한항공
icn_dep |> filter(항공사 == "대한항공") |> 
  group_by(도착공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:5)) |> arrange(rank) -> icn_dep_2_kortop5

ggplot(data = icn_dep_2_kortop5, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + #coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "대한항공 최다 노선 top 5",
       subtitle = "2022년 12월 인천출발")

  #아시아나
icn_dep |> filter(항공사 == "아시아나항공")
(icn_dep |> filter(항공사 == "아시아나항공") |> 
  group_by(도착공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:5)) |> arrange(rank) -> icn_dep_2_asiantop5)

ggplot(data = icn_dep_2_asiantop5, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + #coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "아시아나항공 최다 노선 top 5",
       subtitle = "2022년 12월 인천출발")

# ------------------------------------------------ top 10
#대한항공
icn_dep |> filter(항공사 == "대한항공") |> 
  group_by(도착공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:10)) |> arrange(rank) -> icn_dep_2_kortop10

(ggplot(data = icn_dep_2_kortop10, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "2022년 12월 최다 노선 top 10",
       subtitle = "대한항공 인천출발") -> kor_top10)


#아시아나
icn_dep |> filter(항공사 == "아시아나항공")
(icn_dep |> filter(항공사 == "아시아나항공") |> 
    group_by(도착공항명) |> 
    summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
    filter(rank %in% c(1:10)) |> arrange(rank) -> icn_dep_2_asiantop10)

(ggplot(data = icn_dep_2_asiantop10, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "2022년 12월 최다 노선 top 10",
       subtitle = "아시아나항공 인천출발") -> asiana_top10)

kor_top10 / asiana_top10
