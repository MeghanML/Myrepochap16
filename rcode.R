library(tidyverse)
library(nycflights13)
?flights


dec25 <- filter(flights, month==12, day==25)
nov_dec <- filter (flights, month %in% c(11, 12))
janv1 <- filter(flights, month ==1, day ==1)
delay_more_120 <- filter(flights, dep_delay > 120 | arr_delay > 120 )

#Exercices 
#2 hours or more arrival delay 
TWo_hours_or_more_ardelay <- filter(flights, arr_delay > 120)

#Felw to Houston 
flew_to_houston <- filter (flights, dest %in% c('IAH', 'HOU'))

#operated by United, Americ, Delta ----
airlines
United_car <- filter (airlines, name == 'United Air Lines Inc.')
Delta_car <- filter (airlines, name == 'Delta Air Lines Inc.')
AA_car <- filter (airlines, name == 'American Airlines Inc.')

op_by_UAD <- filter(flights, carrier %in%  c(United_car, Delta_car, AA_car))
#----

#dep in summer 
sum_dep <- filter(flights, month %in% c(7,8,9))

#arrived late depart on time 
arr_late_dep_on_time <- filter (flights, arr_delay > 120 & dep_delay==0)
