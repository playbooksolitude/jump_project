#23-0227 mon home 19:42

#
library(tidyverse)
library(nycflights13)
sample(1:6, 6) |> sd()
sample(1:6, 6) |> mean()

mtcars
table(mtcars$cyl) |> barplot()
mpg
table(mpg$manufacturer) |> barplot()
ggplot2::bar

args(barplot)
example("barplot")

mpg
diamonds
diamonds$cut
table(diamonds$cut) |> barplot()
table(diamonds$cut) |> barplot()

#
flights
table(flights$dest) |> barplot()
table(flights$origin) |> barplot()

table(flights$dest) |> qplot()
table(flights$origin) |> qplot()

table(flights$tailnum) |> qplot()
table(flights$tailnum) |> length()
