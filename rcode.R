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

#dep between midnight and 6 included
dep_night <- filter(flights, sched_dep_time > 0 & sched_dep_time < 601)

dep_night2 <- filter(flights, between(sched_dep_time, 0, 600))

#missing dep time
miss_dep_time <- filter(flights, is.na(dep_time))

#arrange 
arrange(flighs, year, month, day)
flights

arrange(flights, desc(arr_delay))


select (flights, -(year:day))
select(flights, year:dep_time)


select(flights, dep_time, arr_time, everything())
select(flights, contains("arr"))

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)

mutate(flights_sml, gain = arr_delay-dep_delay , speed = distance / air_time *60)
mutate(flights_sml, gain = arr_delay-dep_delay , hours= air_time/60, gain_per_hour=gain/hours)

transmute(flights, dep_time, hour=dep_time %/%100, min=dep_time%%100)

#mean delay by month 
by_month <- group_by(flights, year, month)
summarize(by_month, delay=mean(dep_delay, na.rm=TRUE))

#mean delay by day 
by_day <- group_by(flights, day)
summarize(by_day, delay=mean(dep_delay, na.rm=TRUE))

#delay group by desination 
by_dest <- group_by(flights, dest)
delay_by_dest <- summarize(by_dest, delay=mean(dep_delay, na.rm=TRUE))
arrange(delay_by_dest, desc(delay), dest)

delay <- summarize(by_dest, 
                   count=n(), 
                   dist= mean(distance, na.rm=TRUE), 
                   delay=mean(dep_delay, na.rm=TRUE)
                   )

delay <- filter(delay, count > 20, dest != 'HNL')

library(tidyverse)
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)


delays3 <- flights %>%
  group_by(dest) %>%
  summarize( 
    count=n(),
    dist = mean(distance, na.rm=TRUE),
    delay = mean(arr_delay, na.rm=TRUE)
  )  %>% 
  filter(count > 20, dest != "HNL")

ggplot(data=delays3, mapping=aes(x=dist, y=delay))+
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se = FALSE)


flights %>%
  filter( !is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(year,month, day)%>%
  summarize(mean=mean(dep_delay))


delays_by_tailnum <- flights %>%
  filter (!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarize(delay=mean(arr_delay, na.rm=TRUE), 
            n=n())

ggplot(data=delays_by_tailnum, mapping = aes(x =n, y= delay)) + 
  geom_point(alpha =1/10)

#last flight of the day
last_flight <- not_cancelled %>%
  group_by(year, month, day)%>%
  summarise(
    last=max(dep_time)
  )

#first flight of the day
frist_flight <- not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
  first=min(dep_time)
)

#New chapter on strings 