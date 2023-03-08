#23-0308 wedn 13:59 #동구밭

#
library(tidyverse)
library(rfm)
library(bbplot)

#
dgb <- c("#334557", "#588195", "#CEAE88", "#EFF0F2", "#E3CFC6", "#E5B299")

dgb                  
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
  #geom_label(aes(label = after_stat(count)), stat = "count") +
  bbc_style()

rfm_data_customer
