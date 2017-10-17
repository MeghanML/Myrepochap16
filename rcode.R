library(tidyverse)
library(nycflights13)
?flights


dec25 <- filter(flights, month==12, day==25)
nov_dec <- filter (flights, month %in% c(11, 12))
janv1 <- filter(flights, month ==1, day ==1)
delay_more_120 <- filter(flights, dep_delay > 120 | arr_delay > 120 )
