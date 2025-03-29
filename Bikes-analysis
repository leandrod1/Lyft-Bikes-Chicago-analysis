library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(readxl)
trip_10_2023 <- read_xlsx("202310-divvy-tripdata.xlsx")
trip_11_2023 <- read_xlsx("202311-divvy-tripdata.xlsx")
trip_12_2023 <- read_xlsx("202312-divvy-tripdata.xlsx")
trip_1_2024 <- read_xlsx("202401-divvy-tripdata.xlsx")
trip_2_2024 <- read_xlsx("202402-divvy-tripdata.xlsx")
trip_3_2024 <- read_xlsx("202403-divvy-tripdata.xlsx")
trip_4_2024 <- read_xlsx("202404-divvy-tripdata.xlsx")
trip_5_2024 <- read_xlsx("202405-divvy-tripdata.xlsx")
trip_6_2024 <- read_xlsx("202406-divvy-tripdata.xlsx")
trip_7_2024 <- read_xlsx("202407-divvy-tripdata.xlsx")
trip_8_2024 <- read_xlsx("202408-divvy-tripdata.xlsx")
trip_9_2024 <- read_xlsx("202409-divvy-tripdata.xlsx")
trip_10_2024 <- read_xlsx("202410-divvy-tripdata.xlsx")
trip_6_2024 <- mutate(trip_6_2024, started_at = as.POSIXct(started_at),
                      ended_at = as.POSIXct(ended_at))
trip_7_2024 <- mutate(trip_7_2024, started_at = as.POSIXct(started_at),
                      ended_at = as.POSIXct(ended_at))
trip_8_2024 <- mutate(trip_8_2024, started_at = as.POSIXct(started_at),
                      ended_at = as.POSIXct(ended_at))
trip_9_2024 <- mutate(trip_9_2024, started_at = as.POSIXct(started_at),
                      ended_at = as.POSIXct(ended_at))
trip_10_2024 <- mutate(trip_10_2024, started_at = as.POSIXct(started_at),
                       ended_at = as.POSIXct(ended_at))
trip_6_2024 <- mutate(trip_6_2024, end_station_id = as.character(end_station_id))
all_trips <- bind_rows(trip_10_2023, trip_11_2023, trip_12_2023, trip_1_2024,
                       trip_2_2024, trip_3_2024, trip_4_2024, trip_5_2024,
                       trip_6_2024, trip_7_2024, trip_8_2024, trip_9_2024,
                       trip_10_2024)
save.image("C:/Users/leand/OneDrive/Escritorio/Proyectos/Proyecto_Google_1/Google_proyecti.RData")
str(all_trips)
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
summary(all_trips)
all_trips$date <- as.Date(all_trips$started_at)
View(all_trips)
all_trips$month <- format(as.Date(all_trips$date), "%m")
View(all_trips)
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$days_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_lengths <- difftime(all_trips$ended_at,all_trips$started_at)
is.factor(all_trips$ride_lengths)
all_trips$ride_lengths <- as.numeric(as.character(all_trips$ride_lengths))
is.numeric(all_trips$ride_lengths)
View(all_trips)
mean(all_trips$ride_lengths)
summary(all_trips)
median(all_trips$ride_lengths)
max(all_trips$ride_lengths)
min(all_trips$ride_lengths)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = min)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual + all_trips$days_of_week, FUN = mean)
all_trips$days_of_week <- ordered(all_trips$days_of_week, levels=c("domingo",
                                                                   "lunes",
                                                                   "martes",
                                                                   "miércoles",
                                                                   "jueves", "viernes", "sábado"))
aggregate(all_trips$ride_lengths ~ all_trips$member_casual + all_trips$days_of_week, FUN = mean)
view(all_trips$member_casual)
all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_lengths)) %>%
  arrange(member_casual, weekday)
all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_lengths)) %>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_lengths)) %>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
