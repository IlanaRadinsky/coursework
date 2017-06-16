########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)
library(maps)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')

#######################################
# experiment with mapping
#######################################
states <- map_data("state")
ny <- subset(states, region == "new york")
ny %>% count(subregion) %>% View
nyc <- subset(ny, subregion == "manhattan")

ggplot(nyc, aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3) +
  geom_polygon(color="black", fill="gray") +
  geom_path(trips, aes(x=start_station_longitude, y=end_station_longitude), size=3, lineend = "round")
