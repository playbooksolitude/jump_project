#23-0331 fri 16:49

#
library(tidyverse)
library(lubridate)
library(bbplot)
library(showtext)
showtext_auto()
library(clipr)


#불러오기
getwd()
read_tsv("~/Documents/jump_project/jump_project/files/all-weeks-countries_230331.tsv") -> netflix1_0331
read_csv("./files/netflix_rank1_nametable.csv",
         skip = 1) |> select(2,3) -> netflix_nametable
netflix_nametable

#
netflix_0331
table(netflix_0331$country_name)  #South Korea
table(netflix_0331$week)          #2021-07-04 ~ #2023-03-26

### netflix2_0331_kor ###
(netflix_0331 |> 
  filter(country_name == "South Korea") -> netflix2_0331_kor) #1820

table(netflix1_kor$category)
netflix1_kor_tv$week |> group_by(week) |> summarise(n = n())

table(netflix1_kor_tv$week) #2023-03-05까지만 있음
year(netflix1_kor_tv$week)


  ##0326 누락                                 #data set 만들면 꼭 체크할 것
# (netflix1_kor_tv |> 
#   mutate(
#   year = year(netflix1_kor_tv$week), 
#   month = month(netflix1_kor_tv$week), 
#   day = day(netflix1_kor_tv$week)
# ) |> select(country_name, year, month, day, 
#             category, weekly_rank, show_title, season_title, 
#             cumulative_weeks_in_top_10) -> netflix_2_0331) 

netflix_0331 |> filter(week == c("2023-03-26")) |> 
  filter(country_name == "South Korea") |> print(n = Inf)

table(netflix2_0331_kor$week)

##### 2023년 주단위 1위
  ### netflix3_Films ###
table(netflix2_0331_kor$category) #Films 910 #TV 910
netflix2_0331_kor |> filter(category == "Films") -> netflix3_Films

  ### netflix3_TV ###
(netflix2_0331_kor |> filter(category == "TV") -> netflix3_TV)

  ### netflix4_TV_rank1  #year #month #day 분리
(netflix3_TV |> mutate(
  year = year(week),
  month = month(week),
  day = day(week)) |> filter(weekly_rank == 1) |> 
  arrange(year, month) -> netflix4_TV_rank1)

  ### netflix_nametable3 ### 한국 이름 
colnames(netflix_nametable) <- c("kor_name", "eng_name")
netflix_nametable

    #tidyr separate
(netflix_nametable |> 
  separate(eng_name, 
           into = c("eng_name"), sep = "\n\n") -> netflix_nametable2)

    #tidyr separate
      # 환혼 :part1 #공동경제구역 :: 두번 입력
(netflix_nametable2 |> separate(eng_name, 
                               into = c("eng_name"), 
                               sep = ": Se") -> netflix_nametable3)

#
(left_join(netflix4_TV_rank1, 
          netflix_nametable3, 
          by = c("show_title" = "eng_name")) -> netflix_5_name)

    ##### ggplot ##### --------------------------------------------------------- 한국 1위
netflix4_TV_rank1 |> 
  ggplot(aes(week, 
             y = show_title |> fct_reorder(week))) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity") +
  labs(title = "NETFLIX 한국 1위 TV 부문",
       subtitle = "21년 07 ~ 23년 3월",
       y = "show_title") + bbc_style()

    ##### ggplot ##### --------------------------------------------------------- 한국 1위
netflix_5_name |> 
  ggplot(aes(week, 
             y = kor_name |> fct_reorder(week))) + #날짜가 x축일 때 y 축 정렬
  geom_point(stat = "identity") +
  labs(title = "NETFLIX 한국 1위 TV 부문",
       subtitle = "21년 07 ~ 23년 3월",
       y = "show_title") + bbc_style()


