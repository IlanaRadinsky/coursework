library(scales)
library(readr)
library(tidyverse)

# set plot theme
theme_set(theme_bw())

# read ratings from csv file
#ratings <- read_csv('ratings.csv')

# for reference: same thing, using base R functions and explicitly setting column information
ratings <- read.delim('ratings.csv',
                      sep=',',
                      header=F,
                      col.names=c('user_id','movie_id','rating','timestamp'),
                      colClasses=c('integer','integer','numeric','integer'))

print(object.size(ratings), units="Mb")

####################
# brief look at data
####################

head(ratings)
nrow(ratings)
str(ratings)
summary(ratings)

####################
# aggregate stats
####################

# plot distribution of rating values (slide 21)
ratings %>% 
  ggplot(aes(x=rating)) +
  geom_bar(width=0.4, position = position_dodge(width=0.4)) + 
  labs(x="Rating", y="Number of ratings") +
  scale_y_continuous(labels=comma)

####################
# per-movie stats
####################

# aggregate ratings by movie, computing mean and number of ratings
# hint: use the n() function for easy counting within a group
ratings_by_movie <-
  ratings %>%
  group_by(movie_id) %>%
  summarise(avg=mean(rating), count=n())

# plot distribution of movie popularity (= number of ratings the movie received)
# hint: try scale_x_log10() for a logarithmic x axis
ratings_by_movie %>%
  ggplot(aes(x=movie_id, y=count)) +
  geom_smooth() +
  scale_x_log10()

# plot distribution of mean ratings by movie (slide 23)
# hint: try geom_histogram and geom_density
ratings_by_movie %>%
  ggplot(aes(x=avg)) +
  geom_density(fill="grey", alpha=0.2) +
  labs(x="Avg Rating By Movie", y="Density")

# rank movies by popularity and compute the cdf, or fraction of movies covered by the top-k moves (slide 25)
# hint: use dplyr's rank and arrange functions, and the base R sum and cumsum functions
ratings_by_movie_with_cdf <-
  ratings_by_movie %>%
  arrange(desc(count)) %>%
  mutate(rank=rank(desc(count), na.last=TRUE), cumsum=cumsum(count), cdf=cumsum/sum(count))

# plot the CDF of movie popularity
ratings_by_movie_with_cdf %>%
  ggplot(aes(x=rank, y=cdf)) +
  geom_line() +
  labs(x="Movie Rank", y="CDF") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent)

####################
# per-user stats
####################

# aggregate ratings by user, computing mean and number of ratings
ratings_by_user <-
  ratings %>%
  group_by(user_id) %>%
  summarise(avg=mean(rating), count=n()) 

# plot distribution of user activity (= number of ratings the user made)
# hint: try a log scale here
ratings %>%
  ggplot(aes(x=user_id)) +
  geom_density(fill="purple", alpha=0.8) +
  scale_x_log10() +
  scale_y_continuous(labels=comma)
  
####################
# anatomy of the long tail
####################

# generate the equivalent of figure 2 of this paper:
# https://5harad.com/papers/long_tail.pdf

# Specifically, for the subset of users who rated at least 10 movies,
# produce a plot that shows the fraction of users satisfied (vertical
# axis) as a function of inventory size (horizontal axis). We will
# define "satisfied" as follows: an individual user is satisfied p% of
# the time at inventory of size k if at least p% of the movies they
# rated are contained in the top k most popular movies. As in the
# paper, produce one curve for the 100% user satisfaction level and
# another for 90%---do not, however, bother implementing the null
# model (shown in the dashed lines).

movie_ranks <-
  ratings %>%
  group_by(movie_id) %>%
  summarise(count=n()) %>%
  mutate(rank=rank(desc(count), ties.method="first")) %>%
  arrange(rank) %>%
  select(movie_id, rank)

ratings_with_ranks <-
  ratings %>%
  select(user_id, movie_id) %>%
  left_join(movie_ranks)

# users_with_perc_satisfaction <-
#   ratings_with_ranks %>%
#   group_by(user_id) %>%
#   mutate(num_movies_for_nintey_perc_satisfaction=quantile(rank, 0.9), num_movies_for_onehundred_perc_satisfaction=quantile(rank, 1)) %>%
#   arrange(rank)
# 
# num_unique_users <-
#   ratings %>%
#   select(user_id) %>%
#   unique() %>%
#   summarise(count=n())
# 
# ninety <-
#   users_with_perc_satisfaction %>%
#   select(user_id, rank, num_movies_for_nintey_perc_satisfaction) %>%
#   group_by(rank) %>%
#   filter(num_movies_for_nintey_perc_satisfaction<=rank) %>%
#   summarise(perc_satisfied_ninety=sum(num_movies_for_nintey_perc_satisfaction<=rank)/num_unique_users) %>%
#   View
# 
# hundred <-
#   users_with_perc_satisfaction %>%
#   select(user_id, rank, num_movies_for_onehundred_perc_satisfaction) %>%
#   group_by(rank) %>%
#   filter(num_movies_for_onehundred_perc_satisfaction<=rank) %>%
#   summarise(perc_satisfied_hundred=n()/num_unique_users[1,1])
# 
# inventory_size <-
#   ratings_with_ranks %>%
#   select(rank) %>%
#   unique() %>%
#   arrange(rank)
# 
# final <-
#   left_join(inventory_size, ninety) %>%
#   left_join(hundred) %>%
#   gather("perc_user_satisfaction", "perc_of_users_satisfied", 2:3) 
# 
# final[is.na(final)] <- 0
# 
# ggplot(final, aes(x=rank, y=perc_of_users_satisfied, color=perc_user_satisfaction)) +
#   geom_smooth()

########### Take 2 ##################
num_unique_users <-
  ratings %>%
  select(user_id) %>%
  unique() %>%
  summarise(count=n())

users_with_perc_satisfaction <-
  ratings_with_ranks %>%
  group_by(user_id) %>%
  summarise(num_movies_for_nintey_perc_satisfaction=quantile(rank, 0.9), num_movies_for_onehundred_perc_satisfaction=quantile(rank, 1)) 

inventory_size <-
  ratings_with_ranks %>%
  select(rank) %>%
  unique() %>%
  arrange(rank)

ninety <-
  inventory_size %>%
  group_by(rank) %>%
  summarise(perc_satisfied_ninety=sum(users_with_perc_satisfaction$num_movies_for_nintey_perc_satisfaction<=rank)/num_unique_users[1,1]) 


hundred <-
  inventory_size %>%
  group_by(rank) %>%
  summarise(perc_satisfied_hundred=sum(users_with_perc_satisfaction$num_movies_for_onehundred_perc_satisfaction<=rank)/num_unique_users[1,1])

final <-
  inner_join(ninety, hundred) %>%
  gather("perc_user_satisfaction", "perc_of_users_satisfied", 2:3) 

ggplot(final, aes(x=rank, y=perc_of_users_satisfied, color=perc_user_satisfaction)) +
  geom_line() +
  labs(y="Percent of Users Satisfied", x="Inventory Size", color="Percentage of Individual Satisfaction") +
  scale_color_manual(labels = c("100%", "90%"), values = c("red", "blue")) +
  scale_x_continuous(labels=comma)
