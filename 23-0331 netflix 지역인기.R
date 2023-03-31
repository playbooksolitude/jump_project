#23-0331 fri 16:49

#
library(tidyverse)
library(lubridate)

getwd()
read_tsv("~/Documents/jump_project/jump_project/files/all-weeks-countries_230331.tsv") -> netflix_0331

#
netflix_0331
table(netflix_0331$country_name)

netflix_0331 |> filter(country_name == "South Korea") -> netflix_0331_kor

table(netflix1_kor$category)
netflix1_kor |> tail() |> view()
netflix1_kor$category

#
netflix1_kor |> filter(category == "TV",
                       week > "2023-01-01") -> netflix1_kor_tv

netflix1_kor_tv$week |> group_by(week) |> summarise(n = n())
lubridate::make_datetime()
?ymd


netflix1_kor_tv
year(netflix1_kor_tv$week)

netflix1_kor_tv |> 
  mutate(
  year = year(netflix1_kor_tv$week), 
  month = month(netflix1_kor_tv$week), 
  day = day(netflix1_kor_tv$week)
) |> select(country_name, year, month, day, 
            category, weekly_rank, show_title, season_title, 
            cumulative_weeks_in_top_10) -> netflix_2_0331
