library(tidyverse)

mpg

dec25 <- filter(flights, month==12, day==25)
nov_dec <- filter (flights, month %in% c(11, 12))
