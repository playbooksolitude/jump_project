#23-0311 sat 22:44

#
library(tidyverse)
library(rfm)

#
rfm_data_orders |> tail()
rfm_data_orders |> distinct(customer_id)
rfm_data_orders |> group_by(customer_id) |> summarise(n = n()) |> 
  arrange(desc(n))

rfm_data_orders |> filter(customer_id == "Mr. Lary Champlin")
rfm_data_orders |> group_by(order_date) |> summarise(n = n()) |> 
  arrange(desc(n))

rfm_data_orders$revenue |> range()
rfm_data_orders$order_date |> range()
rfm_data_orders |> slice(1:4,4904:4906)
