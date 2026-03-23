# To wrangle data
library(tidyverse)

# To manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Use the readxl package to read xlsx files
library(readxl)

# Upload datasets (xlsx files)
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

# Inspect the dataframes and look for incongruencies
str(trip_10_2023)
str(trip_11_2023)
str(trip_12_2023)
str(trip_1_2024)
str(trip_2_2024)
str(trip_3_2024)
str(trip_4_2024)
str(trip_5_2024)
str(trip_6_2024)
str(trip_7_2024)
str(trip_8_2024)
str(trip_9_2024)
str(trip_10_2024)

# Convert started_at and ended_at to date-time so that they can stack correctly
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

# Convert end_station_id to character so that they can stack correctly
trip_6_2024 <- mutate(trip_6_2024, end_station_id = as.character(end_station_id))

# Stack individual year's data frames into one big data frame
all_trips <- bind_rows(trip_10_2023, trip_11_2023, trip_12_2023, trip_1_2024,
                       trip_2_2024, trip_3_2024, trip_4_2024, trip_5_2024,
                       trip_6_2024, trip_7_2024, trip_8_2024, trip_9_2024,
                       trip_10_2024)

# Remove lat, long
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the new table that has been created
str(all_trips)
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
summary(all_trips)

# Add columns that list the date, month, day, and year of each ride
# This will allow to aggregate ride data for each month, day, or year
# Before completing these operations I could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) #the default format is yyyy-mm-dd
View(all_trips)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$days_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_lengths <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert “ride_length” from factor to numeric so that calculations can be run on the data
is.factor(all_trips$ride_lengths)
all_trips$ride_lengths <- as.numeric(as.character(all_trips$ride_lengths))
is.numeric(all_trips$ride_lengths)
View(all_trips)

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips$ride_lengths) #straight average (total ride length / rides)
median(all_trips$ride_lengths) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_lengths) #longest ride
min(all_trips$ride_lengths) #shortest ride

# Condense the four lines above to one line using summary() on the specific attribute
summary(all_trips$ride_lengths)

# Compare members and casual users
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_lengths ~ all_trips$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_lengths ~ all_trips$member_casual + all_trips$days_of_week, FUN = mean)

# The days of the week are out of order. Let's fix that
all_trips$days_of_week <- ordered(all_trips$days_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                   "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips$ride_lengths ~ all_trips$member_casual + all_trips$days_of_week, FUN = mean)
view(all_trips$member_casual)

# Analyze ridership data by type and weekday
all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>%            #creates weekday field using
  group_by(member_casual, weekday) %>%                                        #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_lengths)) %>%  #calculates the number of rides and average duration
  arrange(member_casual, weekday)                                             #sorts

# Let's visualize the number of rides by rider type
all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_lengths)) %>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_lengths)) %>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
