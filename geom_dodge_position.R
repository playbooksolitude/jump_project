#23-0307 tue 21:29

#
library(tidyverse)

starwars
ggplot(data = starwars, aes(x = gender, 
                            fill = sex)) + 
  geom_bar(stat = "count", 
           position = "dodge") +
  geom_label(aes(label = stat(count), group = sex), 
            stat = "count", 
            position = position_dodge(width = .9))
