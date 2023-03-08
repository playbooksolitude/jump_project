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

icn_arr2
icn_arr_rename
icn_arr_rename |> select(1:2, 9:13, everything()) -> icn_arr_rename
icn_arr_rename

colnames(icn_arr_rename)
icn_arr_rename |> location()

icn_arr_rename |> mutate(
  sec = seconds(0), .before = 6
) -> icn_arr2_time

icn_arr2_time |> 
  separate(계획시간, into = c("p_time", "p_min"), sep = ":") |> 
  separate(예상시간, into = c("e_time", "e_min"), sep = ":") |> 
  separate(실제시간, into = c("c_time", "c_min"), sep = ":") -> icn_arr3

icn_arr3 |> 
  mutate(
    date = make_date(날짜), .keep = "unused", .before = 2
  )

#
icn_arr3 |> 
  mutate(
    date = ymd(날짜), .keep = "unused", .before = 2,
#    hms(p_time, p_min, sec)
#    make_datetime(date, p_time, p_min, sec)
  ) -> icn_arr4

icn_arr4 -> icn_arr5
icn_arr4$p_time  |> as.integer() -> icn_arr5$p_time
icn_arr4$p_min  |> as.integer() -> icn_arr5$p_min

icn_arr4$e_time  |> as.integer() -> icn_arr5$e_time
icn_arr4$e_min  |> as.integer() -> icn_arr5$e_min
icn_arr5

icn_arr4$c_time  |> as.integer() -> icn_arr5$c_time
icn_arr4$c_min  |> as.integer() -> icn_arr5$c_min

icn_arr4;icn_arr5
icn_arr4;icn_arr5

icn_arr4 |> slice(200)
icn_arr5 |> slice(200)

icn_arr5 |> 
  separate(date, into = c("year", "month", "day"), 
           sep = "-", convert = T) -> icn_arr_6

icn_arr_6 |> 
  mutate(
    date_time = make_datetime(year,month,day,p_time,p_min,sec), 
    .keep = "unused", .before = 2
  ) -> icn_arr_7

3
icn_arr_6 |> 
  mutate(
    p_time = make_datetime(year,month,day,p_time,p_min,sec), 
    e_time = make_datetime(year,month,day,e_time,e_min,sec), 
    c_time = make_datetime(year,month,day,c_time,c_min,sec), 
    .keep = "unused", .before = 2
  ) -> icn_arr_10

icn_arr_10 |> mutate(
  delay_time = c_time - p_time, .keep = "unused"
)

icn_arr_7
(icn_arr_7 |> separate(date_time, into = c("date", "time"), 
                      sep = " ", convert = T) -> icn_arr_8)

icn_arr_8 |> 
  separate(date, into = c("year", "month", "day"), 
           sep = "-", convert = T) |> 
  separate(time, into = c("hour", "min", "sec"),
           sep = ":", convert = T) -> icn_arr_9

icn_arr_9 |> 
























