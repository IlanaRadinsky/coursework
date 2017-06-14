#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
sed 1d 201402-citibike-tripdata.csv | cut -d, -f5,9 | tr , '\n' | sort | uniq -c | wc -l

# count the number of unique bikes
 sed 1d 201402-citibike-tripdata.csv | cut -d, -f12 | sort | uniq -c | wc -l

# count the number of trips per day
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c 2-11 | sort | uniq -c

# find the day with the most rides
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c 2-11 | sort | uniq -c | sort -rn | head -n1

# find the day with the fewest rides
sed 1d 201402-citibike-tripdata.csv | cut -d, -f2 | cut -c 2-11 | sort | uniq -c | sort -n | head -n1

# find the id of the bike with the most rides
sed 1d 201402-citibike-tripdata.csv | cut -d, -f12 | sort | uniq -c | sort -rn | head -n1

# count the number of rides by gender and birth year
sed 1d 201402-citibike-tripdata.csv | cut -d, -f14,15 | sort | uniq -c

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
sed 1d 201402-citibike-tripdata.csv | cut -d, -f5 | grep '[0-9].*&.*[0-9]' | wc -l

# compute the average trip duration
sed 1d 201402-citibike-tripdata.csv | cut -d, -f1 | tr -d '"' | awk -F, '{ total += $1 } END { if (NR > 0) print total/NR }'
