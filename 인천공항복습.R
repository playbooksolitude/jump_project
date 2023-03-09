#23-0304 sat 00:48

#
library(tidyverse)
library(showtext)
library(lubridate)
showtext.auto()
library(bbplot)
library(patchwork)

#
icn_arr
icn_dep
icn_arr |> rename(구분 = "출/도착구분",
                  종류 = "구분",
                  실제시간 = "도착시간") -> icn_arr2
icn_dep |> rename(구분 = "출/도착구분",
                  종류 = "구분",
                  실제시간 = "출발시간") -> icn_dep2
icn_arr2;icn_dep2

full_join(icn_arr2,icn_dep2) -> icn_full

(icn_full -> icn_full2) #---------------------------------- dataset
ymd(icn_full$날짜) -> icn_full2$날짜

icn_full2 |> 
  separate(날짜, into = c("year", "month", "day"), sep = "-", convert = T) |> 
  select(1:4, 종류, 현황, 계획시간, 실제시간,
         contains("공항"), everything()) |> 
  select(-예상시간) -> icn_full3 # ----예상시간 제외

#시간 변경
(icn_full3 |> separate(계획시간, into = c("plan_hour", "plan_min"), sep = ":", convert = T) |> 
  separate(실제시간, into = c("con_hour", "con_min"), sep = ":", convert = T) -> icn_full4)
icn_full3

#지연 많은 날짜
icn_full4 |> filter(현황 == "지연", 종류 == "여객") |> #3,881
  group_by(year, month, day) |> 
  summarise(n = n()) -> icn_full5_delay1
ggplot(icn_full5_delay1, aes(day, n)) + geom_bar(stat = "identity")

icn_full4 |> filter(현황 == "지연", 종류 == "화물") |> #3,881
  group_by(year, month, day) |> 
  summarise(n = n()) -> icn_full5_delay2
ggplot(icn_full5_delay2, aes(day, n)) + geom_bar(stat = "identity")

icn_full4 |> filter(현황 == "지연") |> #3,881
  group_by(year, month, day) |> 
  summarise(n = n()) -> icn_full5_delay

icn_full4 -> icn_full6
rename(icn_full6, status = "현황", 
       type = 종류) -> icn_full7

#면분할
icn_full7 |> 
  ggplot(aes(x = day, y = after_stat(count))) + 
#  geom_point(data = select(icn_full7, -status), stat = "count", color = "grey") +
  geom_bar(stat = "count", aes(color = status)) + 
  facet_wrap(.~status) 

(icn_full7 |> 
  group_by(도착공항명, type) |> summarise(n = n()) |> arrange(desc(n)) -> icn_full8)

  #여객,화물,기타 면분할
(icn_full7 |> group_by(type, status) |> summarise(n = n()) -> icn_8_1type)
(icn_fu8_type |> filter(type %in% c("여객", "화물")) -> icn_8_2filter)

#여객,화물
icn_8_2filter|> ggplot(aes(status,n)) + 
  geom_bar(stat = "identity") +
  facet_wrap(.~type) +
  geom_label(aes(label = n))

#ggplot
icn_full7 |> filter(status == "지연", type == "여객") |> 
  group_by(day) |> summarise(n = n()) |> 
  ggplot(aes(day |> as.factor(),n)) + geom_bar(stat = "identity") +
  geom_label(aes(label = n)) +
  bbc_style() +
  labs(title = "항공 노선 지연", subtitle = "2022년 12월 인천공항")

    #지연 #여객 #출발-도착 노선수
(icn_full7 |> filter(status == "지연", type == "여객", day %in% c(17,21)) |> 
  group_by(구분) |> summarise(n =n()) -> icn_8_3_day17_21)

    #지연출발 #17
icn_full7 |> filter(status == "지연", type == "여객", 
                    day %in% c(17), 구분 == "출발")      #출발 #145
  
icn_full7 |> filter(status == "지연", type == "여객", 
                    day %in% c(17), 구분 == "출발") |> 
  group_by(항공사, 도착공항명) |> summarise(n = n()) |> arrange(desc(n))

  #지연출발 #21
icn_full7 |> filter(status == "지연", type == "여객", 
                    day %in% c(21), 구분 == "출발")      #출발 #169

icn_full7 |> filter(status == "지연", type == "여객", 
                    day %in% c(21), 구분 == "출발") |> 
  group_by(항공사, 도착공항명) |> summarise(n = n()) |> arrange(desc(n))


icn_full7 |> filter(status == "지연", type == "여객", 
                    day %in% c(17), 구분 == "도착")      #출발 #145

# 지연 top 5 항공사
icn_full7 |> filter(status == "지연") |> 
  group_by(구분, 항공사) |> 
  summarise(n = n()) |> arrange(desc(n)) |> 
  mutate(rank = row_number(desc(n))) |> 
  filter(rank %in% c(1:10)) -> icn_8_4late

  #지연 출발
(icn_8_4late |> filter(구분 == "출발") |> 
  ggplot(aes(x = 항공사 |> fct_reorder(n), y = n)) + geom_bar(stat = "identity") +
  facet_wrap(.~구분, nrow = 2) + bbc_style() +
  geom_label(aes(label = n)) + coord_cartesian(ylim = c(0,700)) +
  coord_flip()-> a1)

  #지연 도착
(icn_8_4late |> filter(구분 == "도착") |> 
  ggplot(aes(x = 항공사 |> fct_reorder(n), y = n)) + geom_bar(stat = "identity") +
  facet_wrap(.~구분, nrow = 2) + bbc_style() +
  geom_label(aes(label = n), size = 7) + 
    coord_flip() -> a2)

icn_full7

a1 | a2

#인천공항 자주 이용하는 항공사
icn_full7 |> 
  group_by(구분, 항공사) |> 
  summarise(n = n()) |> arrange(desc(n)) |> 
  mutate(rank = min_rank(desc(n))) |> 
  filter(rank %in% c(1:10)) |> 
  ggplot(aes(x = 항공사 |> fct_reorder(desc(n)), y = n)) + geom_bar(stat = "identity") +
  facet_wrap(.~구분, nrow = 2) + bbc_style() +
  geom_label(aes(label = n), size = 7)


#가장 지연이 길었던 경우 #출발
(icn_full7 |> mutate(
  delay = con_hour - plan_hour, .before = 1) |> 
  filter(type == "여객", delay >= 2, 구분 == "출발") |> 
  arrange(desc(delay)) -> delay_dep)

ggplot(delay_dep, aes(x = day |> as.factor(), y = delay)) + 
  geom_jitter(stat = "identity") + bbc_style()# +
  geom_label(data = filter(delay_dep, delay > 9), aes(label = 도착공항명))
  
#가장 지연이 길었던 경우 #도착
icn_full7 |> mutate(
  delay = con_hour - plan_hour, .before = 1) |> 
  filter(type == "여객", delay >= 1, 구분 == "도착") |> 
  arrange(desc(delay)) -> delay_arr

ggplot(delay_arr, aes(x = day |> as.factor(), y = delay)) + 
  geom_jitter(stat = "identity") + bbc_style()# +
geom_label(data = filter(delay_dep, delay > 9), aes(label = 출발공항명))


#
icn_full7 |> mutate(
  delay = con_hour - plan_hour, .before = 1) |> filter(type == "여객", delay > 3) |> 
  arrange(desc(delay)) |> group_by(항공사) |> summarise(n = n()) |> 
  arrange(desc(n))

#대기시간이 길었던 여객 항공



#주사위 확률
sample(1:2, 10, replace = T)
sample(1:2,6, replace = T)
dice <- function() {
  die <- sample(1:6,6, replace = T)
  table(die)
}

dice()
(1/6)*(1/5)*(1/4)*(1/3)*(1/2)*(1/1)
anscombe

tibble(
  x = anscombe$x1,
  y = anscombe$y1) |> ggplot(aes(x,y)) + geom_bar(size = 5, stat = "identity") + 
  bbc_style() + coord_cartesian(xlim = c(0,15), ylim = c(0,15)) -> a
  
tibble(
  x = anscombe$x1,
  y = anscombe$y1)

tibble(
  x = anscombe$x2,
  y = anscombe$y2) |> ggplot(aes(x,y)) + geom_bar(size = 5, stat = "identity") + 
  bbc_style() + coord_cartesian(xlim = c(0,15), ylim = c(0,15)) -> b

tibble(
  x = anscombe$x3,
  y = anscombe$y3) |> ggplot(aes(x,y)) + geom_bar(size = 5, stat = "identity") + 
  bbc_style() + coord_cartesian(xlim = c(0,15), ylim = c(0,15)) -> c

tibble(
  x = anscombe$x4,
  y = anscombe$y4) |> ggplot(aes(x,y)) + geom_bar(size = 5, stat = "identity") + 
  bbc_style() + coord_cartesian(xlim = c(0,15), ylim = c(0,15)) -> d
                                

a + geom_smooth(se = F, method = "lm") | b + geom_smooth(se = F, method = "lm") | c + geom_smooth(se = F, method = "lm")
c + geom_smooth(se = F) | d + geom_smooth(se = F)

a | b | c


df <- data.frame(player=c('Brad', 'Ty', 'Spencer', 'Luke', 'Max'),
                 points=c(17, 5, 12, 20, 22),
                 assists=c(4, 3, 7, 7, 5))
ggplot(df) +
  geom_point(aes(x=points, y=assists)) + 
  #geom_text(aes(x=12, y=7, label="대한항공 vs 아시아나항공"), size = 10) + 
  geom_label(aes(x=15, y=8, label="대한항공 vs 아시아나항공"), size = 10) + 
  ylim(3, 8)





