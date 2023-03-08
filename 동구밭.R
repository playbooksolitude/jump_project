#23-0308 wedn 13:59 #동구밭

#
library(tidyverse)
library(rfm)
library(bbplot)
library(showtext)
showtext_auto()
#

#par(ask = FALSE)


#
dgb <- c("#334557", "#588195", "#CEAE88", "#EFF0F2", "#E3CFC6", "#E5B299")
#
rfm_data_customer
rfm_data_orders

#user
rfm_data_customer

rfm_data_customer |> count(cut_width(number_of_orders, 10, lables = F))
rfm_data_customer |> range(number_of_orders)

#구매분포
n_distinct(rfm_data_customer$number_of_orders)
ggplot(data = rfm_data_customer, aes(x = as.factor(number_of_orders))) + 
  geom_bar() + 
  geom_label(aes(label = after_stat(count)), stat = "count") +
  bbc_style() +
  labs(title = "RFM 분석 예시: 구매건수별 고객수", 
       subtitle = "X축 구매건수, Y축 고객수")

rfm_data_customer
table(rfm_data_customer$most_recent_visit) |> length()

ggplot(data = rfm_data_customer, 
       aes(x = most_recent_visit)) +
  geom_bar() + bbc_style()

table(rfm_cus$year)
rfm_data_customer |> 
  str_replace(most_recent_visit, "2006", "2022")

rfm_data_customer |> separate(most_recent_visit,
                              into = c("year", "month", "day"),
                              sep = "-", convert = T) -> rfm_cus

year2 = as.integer(2022)
rfm_cus |> mutate(year2, .before = 3) |> select(-year) |> 
  rename(year = year2) -> rfm_cus2

rfm_cus2 |> filter(year == 2022) |> ggplot(aes(x = month)) + geom_bar()
rfm_cus2 |> filter(year == 2022) -> rfm_cus2022
colnames(rfm_cus2022)
  #revenue #recency_days #number_of_orders

#x축 기간 #y축 구매건수
rfm_cus2022 |> ggplot(aes(recency_days, 
                          number_of_orders)) + 
  geom_point(stat = "identity",
             color = "#588195") +
  bbc_style() +
  labs(title = "RFM 분석 예시: 가입기간 * 구매건수 추세", 
       subtitle = " ")

#누적 고객수
rfm_cus2022 |> count(cut_width(recency_days,30, boundary = 0, labels = F)) |> 
  rename("기간" = 1, "users" = 2) |> 
  mutate(a = 30,
         days = 기간*a) |> select(days, users) |> 
  mutate(cum_users = cumsum(users),
         pop = cum_users/last(cum_users)*100) |> 
  rename("기간"= 1, "고객수" = 2, "누적고객수" = 3, "비율" = 4) -> rfm_cus_cum

#
ggplot(data = rfm_cus_cum, 
       aes(x = as.factor(기간), y = 고객수)) + 
  geom_bar(stat = "identity") + geom_label(aes(label = 고객수)) +
  bbc_style() +
  labs(title = "RFM 분석 예시: 가입기간별 고객수 분포", 
       subtitle = "X축 : 회원 가입일, Y축 고객수 ")

rfm_cus_cum
rfm_cus2022
#
ggplot(rfm_cus2022, aes(x = month |> as.factor(), 
                        after_stat(count))) +
  geom_bar(stat = "count") + 
  geom_label(aes(label = ..count..), stat = "count") +
  bbc_style() +
  labs(title = "RFM 분석 예시: 월별 구매건수", 
       subtitle = "X축 : 월, Y축 구매건수 ")

#면분할
rfm_cus2022 |> ggplot(aes(x = revenue, y = number_of_orders)) + 
  geom_point(stat = "identity", ) + 
  facet_wrap(.~month)

?rfm_heatmap
rfm_order
rfm_data_orders
### ** Examples

# using transaction data
analysis_date <- lubridate::as_date('2006-12-31')
rfm_order <- rfm_table_order(rfm_data_orders, customer_id, order_date,
                             revenue, analysis_date)

# heat map
rfm_heatmap(rfm_order)
rfm_heatmap(rfm_order) 



























