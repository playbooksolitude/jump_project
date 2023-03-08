#23-0308 wedn 11:31

#
library(tidyverse)
getwd()
dir()
read_tsv("./files/all-weeks-countries.tsv") -> netflix1
netflix1
table(netflix1$country_name)
netflix1 |> filter(country_name == "South Korea") -> netflix1_kor

#
table(netflix1_kor$category)
netflix1_kor |> filter(category == "Films") -> netflix2_films
netflix1_kor |> filter(category == "TV") -> netflix2_tv

#
table(netflix2_films$season_title)
table(netflix2_films$week)


netflix2_films |> filter(week == "2023-03-05")  #영화 랭킹
netflix2_tv |> filter(week == "#2023-03-05")     #TV 랭킹

#ditto
netflix2_films |> filter(show_title == "Ditto")
netflix2_tv |> filter(show_title == "Summer Strike")
netflix2_tv

#
netflix1
str_replace(netflix1$week, "-", "")
str_replace_all(netflix1$week, "-", "")
netflix1[,3:5]

netflix1 |> slice(dim(netflix1)[1])
netflix1
last(netflix1$weekly_rank);first(netflix1$weekly_rank)
max(netflix1$weekly_rank);min(netflix1$weekly_rank)
range(netflix1$weekly_rank)

args(last)
example(last)

#rfm
install.packages("rfm")
library(rfm)
rfm_data_orders

#
args(rfm_bar_chart)
example("rfm_bar_chart")
