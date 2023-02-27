#23-0227 mon office 11:50

#
library(tidyverse)
library(bbplot)
library(patchwork)

#anscombe
anscombe

# --------------------------- dataset
tibble(
  x1 = anscombe$x1,
  y1 = anscombe$y1
) -> anscombe_1

tibble(
  x1 = anscombe$x2,
  y1 = anscombe$y2
) -> anscombe_2

tibble(
  x1 = anscombe$x3,
  y1 = anscombe$y3
) -> anscombe_3

tibble(
  x1 = anscombe$x4,
  y1 = anscombe$y4
) -> anscombe_4

anscombe_1;anscombe_1;anscombe_3;anscombe_4

# --------------------------- ggplot
anscombe_1 |> 
  ggplot(aes(x = x1, y = y1)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

anscombe_2 |> 
  ggplot(aes(x = x1, y = y1)) + geom_point() + geom_smooth(method = "lm", se = F)

anscombe_3 |> 
  ggplot(aes(x = x1, y = y1)) + geom_point() + geom_smooth(method = "lm", se = F)

anscombe_4 |> 
  ggplot(aes(x = x1, y = y1)) + geom_point() + geom_smooth(method = "lm", se = F)



