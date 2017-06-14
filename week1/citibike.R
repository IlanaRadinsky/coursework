library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
# trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
nrow(trips)

# find the earliest and latest birth years (see help for max and min to deal with NAs)
trips <- mutate(trips, birth_year = as.numeric(birth_year))
trips %>% summarise(earliest=min(birth_year, na.rm = TRUE), latest=max(birth_year, na.rm = TRUE)) %>% View

# use filter and grepl to find all trips that either start or end on broadway
trips %>% filter(grepl("Broadway", start_station_name) | grepl("Broadway", end_station_name)) %>% View

# do the same, but find all trips that both start and end on broadway
trips %>% filter(grepl("Broadway", start_station_name), grepl("Broadway", end_station_name)) %>% View

# find all unique station names
bind_rows(select(trips, start_station_name), select(trips, end_station_name) %>% transmute(start_station_name = end_station_name)) %>% unique() %>% View
unique(combine(trips["start_station_name"], trips["end_station_id"]))

# count the number of trips by gender
trips %>% group_by(gender) %>% count()
trips %>% group_by(gender) %>% summarise(count=n())

# compute the average trip time by gender
trips %>% group_by(gender) %>% summarise(avg_mins = mean(tripduration)/60) %>% View

# comment on whether there's a (statistically) significant difference
trips %>% group_by(gender) %>% summarise(avg_mins = mean(tripduration)/60, sd_mins = sd(tripduration)/60) %>% View

# find the 10 most frequent station-to-station trips
trips %>% group_by(start_station_name, end_station_name) %>% count() %>% arrange(desc(n)) %>% head(10) %>% View

# find the top 3 end stations for trips starting from each start station
trips %>% 
  group_by(start_station_name, end_station_name) %>% 
  count() %>% 
  #ungroup() %>% 
  group_by(start_station_name) %>% 
  top_n(3, n) %>% 
  View

trips %>% 
  group_by(start_station_name, end_station_name) %>% 
  count() %>% 
  #ungroup() %>% 
  group_by(start_station_name) %>% 
  arrange(start_station_name, desc(n)) %>%
  mutate(row = row_number()) %>%
  filter(row<=3) %>%
  View

trips %>% 
  group_by(start_station_name, end_station_name) %>% 
  count() %>% 
  #ungroup() %>% 
  group_by(start_station_name) %>% 
  arrange(start_station_name, desc(n)) %>%
  filter(row_number()<=3) %>%
  View

# find the top 3 most common station-to-station trips by gender
trips %>% 
  group_by(gender, start_station_name, end_station_name) %>% 
  count() %>% 
  #ungroup() %>% 
  group_by(gender) %>% 
  top_n(3, n) %>% 
  View

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)
trips <- mutate(trips, day=as.Date(starttime))
trips <- select(trips, tripduration, day, everything())
trips %>% 
  group_by(day) %>% 
  count() %>% 
  #ungroup() %>% 
  filter(n == max(n)) %>% 
  View

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?
trips <- mutate(trips, time=format(starttime, "%H:%M:%S"))
trips <- mutate(trips, hour=format(starttime, "%H"))
trips <- select(trips, tripduration, day, time, hour, everything())
trips %>% 
  group_by(hour, day) %>% 
  count() %>% 
  #ungroup() %>% 
  group_by(hour) %>% 
  summarise(avg_num_trips = mean(n)) %>%
  View