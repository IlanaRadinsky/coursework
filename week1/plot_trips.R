<<<<<<< HEAD
########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
trips %>%
  summarise(quant=quantile(tripduration, prob=0.99)/60) %>%
  View

ggplot(trips, aes(x=tripduration/60)) +
  geom_density() +
  xlim(c(0, 59.3)) +
  xlab("Trip Duration (min)") +
  ylab("Density")
# OR - this way doesn't involve cutting out data
ggplot(trips, aes(x=tripduration/60)) +
  geom_density() +
  scale_x_log10() +
  xlab("Trip Duration (min)") +
  ylab("Density")

# plot the distribution of trip times by rider type
ggplot(trips, aes(x=tripduration/60, color=usertype, fill=usertype)) +
  geom_density(alpha=0.2) +
  xlim(c(0, 59.3)) +
  xlab("Trip Duration (min)") +
  ylab("Density")

# plot the number of trips over each day
trips %>%
  group_by(ymd) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=ymd, y=count)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(label=comma) +
  xlab("Date (YMD)") +
  ylab("Number of Trips")

# plot the number of trips by gender and age
trips %>%
  filter(gender!="Unknown") %>%
  group_by(birth_year, gender) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=2013-birth_year, y=count, color=gender)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(label=comma) +
  xlab("Age (yrs)") +
  ylab("Number of Trips")

# plot the ratio of male to female trips by age
# hint: use the spread() function to reshape things to make it easier to compute this ratio
trips %>%
  mutate(age=2013-birth_year) %>%
  filter(gender!="Unknown", age<=70, age>=5) %>%
  group_by(gender, age) %>%
  summarise(count=n()) %>%
  spread(gender, count) %>%
  mutate(ratio=Male/Female) %>%
  ggplot(aes(x=age, y=ratio, size=log10(Male+Female))) +
  geom_point() +
  ylim(0, 7.5) +
  xlab("Age (yrs)") +
  ylab("Ratio of Male to Female Bikers")

########################################
# plot weather data
########################################
# plot the minimum temperature over each day
ggplot(weather, aes(x=ymd, y=tmin)) +
  geom_smooth() +
  geom_point(color = "blue") +
  xlab("Date (ymd)") +
  ylab("Minimum Temperature")

# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting
weather %>%
  select(ymd, tmax, tmin) %>%
  gather("min_max", "temp", 2:3) %>%
  ggplot(aes(x=ymd, y=temp, color=min_max)) +
  geom_point() +
  geom_smooth() +
  xlab("Date (ymd)") +
  ylab("Temperature") +
  scale_color_hue(labels = c("Max", "Min")) +
  labs(color="")

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>%
  group_by(tmin, ymd) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=tmin, y=count)) +
  geom_point() +
  geom_smooth() +
  labs(x="Minimum Temperature", y="Number of Trips")

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>%
  mutate(substantial_prcp=prcp>=quantile(prcp, 0.9)) %>%
  group_by(tmin, ymd, substantial_prcp) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=tmin, y=count, color=substantial_prcp)) +
  geom_point() +
  labs(x="Minimum Temperature", y="Number of Trips", color="Substantial Precipitation")

# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>%
  mutate(substantial_prcp=quantile(prcp, 0.9)) %>%
  group_by(tmin, ymd, substantial_prcp) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=tmin, y=count, color=substantial_prcp)) +
  geom_point() +
  geom_smooth() +
  labs(x="Minimum Temperature", y="Number of Trips", color="Substantial Precipitation")

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
library(lubridate)
avg_sd <- trips %>%
  mutate(hour=hour(starttime)) %>%
  group_by(hour, ymd) %>%
  summarise(count=n()) %>%
  group_by(hour) %>%
  summarise(avg=mean(count), sd=sd(count)) #%>%
  #View

# plot the above
avg_sd %>%
  gather("measure", "val", 2:3) %>%
  ggplot(aes(x=hour, y=val, color=measure)) +
  geom_point() +
  geom_smooth() +
  labs(x="Hour", y="Trip Duration (mins)", color="Type")

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
avg_sd_by_week <- trips %>%
  mutate(hour=hour(starttime), dow=as.factor(wday(starttime))) %>%
  group_by(hour, ymd, dow) %>%
  summarise(count=n()) %>%
  group_by(hour, dow) %>%
  summarise(avg=mean(count), sd=sd(count)) #%>%
  #View

levels(avg_sd_by_week$dow) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

avg_sd_by_week %>%
  gather("measure", "val", 3:4) %>%
  ggplot(aes(x=hour, y=val, color=measure)) +
  geom_point() +
  geom_smooth() +
  labs(x="Hour", y="Trip Duration (mins)", color="Type") +
  facet_wrap(~ dow)