#23-0227 mon office 12:22
#인천공항 12월

#
library(tidyverse)
library(readxl)

#install.packages('devtools')
#devtools::install_github('bbc/bbplot')
library(bbplot)

#install.packages("showtext")
library(showtext)
showtext_auto()


getwd()
# read_excel("/Users/yohan_15/Documents/jump/jump_project/files/arr_22_12.xlsx") -> icn_arr
# read_excel("/Users/yohan_15/Documents/jump/jump_project/files/dep_22_12.xlsx") -> icn_dep
read_excel("/Users/yohanchoi/Documents/jump/jump_project/files/dep_22_12.xlsx") -> icn_dep
read_excel("/Users/yohanchoi/Documents/jump/jump_project/files/arr_22_12.xlsx") -> icn_arr
icn_dep;icn_arr

#
icn_arr
icn_dep

icn_arr |> colnames()
icn_arr |> head() |> view()

icn_arr |> select(-10) 

#
(icn_arr |> select(-10) |> 
    separate(날짜, into = c("year", "month"), sep = 4, convert = T) |> 
    separate(month, into = c("month", "day"), sep = 2, convert = T) |> 
  separate(계획시간, into = c("p_hour", "p_min"), sep = ":", convert = T) |> 
  separate(도착시간, into = c("hour", "min"), sep = ":", convert = T) -> icn_arr_2_sep)

# icn_arr |> select(-10) |> 
#   separate(날짜, into = c("year", "month"), sep = 4, convert = T) |> 
#   separate(month, into = c("month", "day"), sep = 2, convert = T) |> 
#   separate(계획시간, into = c("p_hour", "p_min"), sep = ":", convert = T) |> 
#   unite(p_hour, p_min) |> head() |> view()

# -------------------------------------- 인천공항 최다 노선 (-> 인천)
icn_arr |> group_by(출발공항명) |> 
  summarise(n = n()) |> arrange(desc(n))

icn_arr |> group_by(출발공항명, 항공사) |> 
  summarise(n = n()) |> arrange(desc(n))

(icn_arr |> group_by(출발공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:5)) |> arrange(rank) -> icn_arr_2_top5)

ggplot(data = icn_arr_2_top5, 
       aes(x = 출발공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + #coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "2022년 12월 인천공항 도착",
       subtitle = "최다 노선 top 5")

# ------------------------------------------------ top 5
(icn_arr |> group_by(출발공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:5)) |> arrange(rank) -> icn_arr_2_top5)

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
  filter(rank %in% c(1:20)) |> arrange(rank) -> icn_dep_2_kortop20

ggplot(data = icn_dep_2_kortop20, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + coord_flip() +
  geom_label(aes(label = n)) +
    labs(title = "대한항공 최다 노선 top 20",
         subtitle = "2022년 12월 인천출발")

#아시아나
(icn_dep |> filter(항공사 == "아시아나항공") |> 
    group_by(도착공항명) |> 
    summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
    filter(rank %in% c(1:20)) |> arrange(rank) -> icn_dep_2_asiantop20)

ggplot(data = icn_dep_2_asiantop20, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity") +
  bbc_style() + coord_flip() +
  geom_label(aes(label = n)) +
    labs(title = "아시아나항공 최다 노선 top 20",
         subtitle = "2022년 12월 인천출발")

# ------------------------------------------------- top 20 #color 
#대한항공
(icn_dep |> filter(항공사 == "대한항공") |> 
  group_by(도착공항명) |> 
  summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:20)) |> arrange(rank) -> icn_dep_2_kortop20)

#변수 설정
(icn_dep_2_kortop20$도착공항명 %in% c("로스앤젤레스", 
        "뉴욕", "프랑크푸르트", "시카고","런던히드로", "시애틀") -> kor_usa)

ggplot(data = icn_dep_2_kortop20, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity",
           fill = ifelse(kor_usa == TRUE, "#1380A1", "#dddddd")) +
  bbc_style() + coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "대한항공 최다 노선 top 20",
       subtitle = "2022년 12월 인천출발")

#아시아나
(icn_dep |> filter(항공사 == "아시아나항공") |> 
    group_by(도착공항명) |> 
    summarise(n = n()) |> mutate(rank = min_rank(desc(n))) |> 
    filter(rank %in% c(1:20)) |> arrange(rank) -> icn_dep_2_asiantop20)

ggplot(data = icn_dep_2_asiantop20, 
       aes(x = 도착공항명 |> fct_reorder(n), y = n)) + 
  geom_bar(stat = "identity",
           fill = ifelse(icn_dep_2_asiantop20$도착공항명 %in% c("로스앤젤레스", 
            "프랑크푸르트", "샌프란시스코", "시애틀","뉴욕"),
           "#1380A1", "#dddddd")) +
  bbc_style() + coord_flip() +
  geom_label(aes(label = n)) +
  labs(title = "아시아나항공 최다 노선 top 20",
       subtitle = "2022년 12월 인천출발")

# ifelse(icn_dep_2_kortop20$도착공항명 %in% c("로스앤젤레스", 
#   "뉴욕", "프랑크푸르트", "시카고", "런던히드로", "시애틀")
       


