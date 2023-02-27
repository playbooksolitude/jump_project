#23-0224 fri 22:04

#
library(tidyverse)
# install.packages("devtools")
#devtools::install_github('bbc/bbplot')
library(bbplot)

# read_tsv
getwd()
read_tsv("/Users/yohanchoi/Documents/jump/jump_project/files/all-weeks-global.tsv") -> netflix
read_tsv("/Users/yohanchoi/Downloads/all-weeks-global.tsv") -> netflix

netflix

#separate

#top 10
colnames(netflix)

netflix |> view()
netflix |> head()

netflix
netflix |> 
  group_by(show_title, season_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000) |> 
  arrange(desc(M_hours))

netflix
#2021년의 가장 인기있는 작품
netflix |> filter(show_title == "Stranger Things") |> 
  group_by(show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000) |> 
  arrange(desc(M_hours))




netflix |> dim();netflix |> slice(1,3440)

netflix |> group_by(show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000,
         n = n()) |> 
  arrange(desc(M_hours)) |> 
  mutate(rank = min_rank(desc(M_hours))) |> 
  filter(rank %in% c(1:5)) -> netflix_4_total

ggplot(data = netflix_4_total, 
       aes(x = show_title |> fct_reorder(M_hours), y = M_hours)) + 
  geom_bar(stat = "identity") +
  bbc_style() +
  geom_label(aes(label = round(M_hours,0)), size = 10) +
  labs(title = "Netflix global top viewd (million hours)",
       subtitle = "2021-07-04 ~ 2023-02-19")

#2021 #2022 #2023 top10

(netflix |> 
  separate(week, into = c("year", "month", "day"), sep = "-") -> netflix_2_year)



  #2021 -----------------
(netflix_2_year |> filter(year == "2021") |> 
  group_by(show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000,
            n = n()) |> 
  arrange(desc(M_hours)) |> 
  mutate(rank = min_rank(desc(M_hours))) |> 
    filter(rank %in% c(1:5)) -> netflix_4_2021)

ggplot(data = netflix_4_2021, 
       aes(x = show_title |> fct_reorder(M_hours), y = M_hours)) + 
  geom_bar(stat = "identity") +
  bbc_style() +
  geom_label(aes(label = round(M_hours,0)), size = 10) +
  labs(title = "Netflix global top viewed (million hours)",
       subtitle = "2021-07 ~ 2021-12")

  #2022 -----------------
(netflix_2_year |> filter(year == "2022") |> 
  group_by(show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000,
            n = n()) |> 
  arrange(desc(M_hours)) |> 
  mutate(rank = min_rank(desc(M_hours))) |> 
    filter(rank %in% c(1:5))-> netflix_4_2022)

ggplot(data = netflix_4_2022, 
       aes(x = show_title |> fct_reorder(M_hours), y = M_hours)) + 
  geom_bar(stat = "identity") +
  bbc_style() +
  geom_label(aes(label = round(M_hours,0)), size = 10) +
  labs(title = "Netflix global top viewed (million hours)",
       subtitle = "2021-01 ~ 2022-12")


  #2023 -----------------
(netflix_2_year |> filter(year == "2023") |> 
  group_by(show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000,
            n = n()) |> 
  arrange(desc(M_hours)) |> 
  mutate(rank = min_rank(desc(M_hours))) |> 
    filter(rank %in% c(1:5)) -> netflix_4_2023)

ggplot(data = netflix_4_2023, 
       aes(x = show_title |> fct_reorder(M_hours), y = M_hours)) + 
  geom_bar(stat = "identity") +
  bbc_style() +
  geom_label(aes(label = round(M_hours,0)), size = 10) +
  labs(title = "Netflix global top viewed (million hours)",
       subtitle = "2023-01 ~ 2023-02")


#2022 monthly top 10

(netflix_2_year |> filter(year == "2022") -> netflix_3_2022)

netflix_3_2022 |> 
  group_by(year, month, show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed/1000000)) |> 
  mutate(rank = min_rank(desc(M_hours))) |> 
  filter(rank %in% c(1:10)) |> 
  arrange(month, rank) |> print(n = Inf)

  #montly top 1
netflix_3_2022 |> 
  group_by(year, month, show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed/1000000)) |> 
  mutate(rank = min_rank(desc(M_hours))) |> 
  filter(rank %in% c(1)) |> 
  arrange(month, rank) |> print(n = Inf)

  #2022 top10 by season
netflix_3_2022 |> group_by(show_title, season_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed/1000000)) |> 
  arrange(desc(M_hours))

netflix_3_2022 |> group_by(show_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed/1000000)) |> 
  arrange(desc(M_hours))

  #check
netflix_3_2022 |> filter(show_title == "Stranger Things") |> 
  count(wt = weekly_hours_viewed/1000000)

  #Stranger Things season by season
netflix_3_2022 |> filter(show_title == "Stranger Things") |> 
  group_by(show_title, season_title) |> 
  summarise(M_hours = sum(weekly_hours_viewed/1000000)) |> 
  arrange(desc(M_hours))

  #2022 time spent monthly ggplot
netflix_3_2022 |> group_by(year, month) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000) |> 
  ggplot(aes(x = month, y = M_hours)) + 
  geom_bar(stat = "identity") +
  geom_line(group = 1, color = "#1380A1") +
  geom_label(aes(label = round(M_hours,0))) +
  geom_hline(yintercept = 0, size = 1, color = "#333333")+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) + bbc_style() +
  labs(title = "Netflix time spent", 
       subtitle = "2022 by month")

netflix_3_2022 |> group_by(year, month) |> 
  summarise(M_hours = sum(weekly_hours_viewed)/1000000) |> 
  ggplot(aes(x = month, y = M_hours)) +
  geom_bar(stat = "identity") + 
  geom_line(group = 1, color = "#1380A1") +
  geom_label(aes(label = round(M_hours,0))) +
  geom_hline(yintercept = 0, size = 3, color = "#333333") +
  labs(title = "Netflix time spent", 
       subtitle = "2022 by monthly") +
  bbc_style()


