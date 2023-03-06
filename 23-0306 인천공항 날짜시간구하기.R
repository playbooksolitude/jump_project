#23-0306 mon 20:27
#날짜 시간 구하기

#
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

#-----------------지연 시간 구하기
hms(icn_full4$plan_hour, icn_full4$plan_min, icn_full4$plan_sec)

icn_full4 |> mutate(
  plan_sec = lubridate::seconds(0), .after = plan_min
) 
icn_full4 |> mutate(
  plan = make_datetime(year, month, day, plan_hour, plan_min), .keep = "unused"
)

icn_arr |> mutate(
  data = ymd(날짜), .keep = "unused"
)


icn_arr |> 
  mutate(
    date = ymd(날짜), .keep = "unused", .before = 2,
    sec = seconds(0)
  ) -> icn_arr2_time

icn_arr2_time
