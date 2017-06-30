
## Movie enjoyment dataset


library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(boot)
library(glmnet)
library(scales)

setwd("C:/MSR-DS3/coursework/week3")

movies <- read.csv("5-26-2017_Ratings_of_50_popular_films_on_enjoyment_versus_appreciation_(and_more).csv")
colnames(movies)


# Cleaning data...


movies <-
  movies %>%
  mutate(FilmName=V15) %>%
  mutate(ViewerCount = (ViewerCount_enj + ViewerCount_appr)/2) %>%
  select(-V15, -Check_enj_label, -Check_appr_label, -ViewerCount_appr, -ViewerCount_enj) %>%
  mutate(InflatedGross=as.numeric(gsub('[$,]','',InflatedGross)))


# 1. Which genre is the highest rated?


genres_with_enj_and_appr <-
  movies %>%
  gather("Genre", "isGenre", 19:34) %>%
  group_by(Genre, isGenre) %>%
  filter(isGenre==1) %>%
  summarise(avg_enjoyment = mean(Enjoyment), avg_appreciation = mean(Appreciation)) %>%
  gather("Enj_Appr", "Avg_Value", 3:4) 

plot1 <- 
  ggplot(genres_with_enj_and_appr, aes(x=Genre, y=Avg_Value, fill=Enj_Appr)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1

# 2. Which genre is the most popular?

genres_with_ratings <-
  movies %>%
  gather("Genre", "isGenre", 19:34) %>%
  group_by(Genre, isGenre) %>%
  filter(isGenre==1) %>%
  summarise(popularity = mean(ViewerCount)) 

plot2 <-
  ggplot(genres_with_ratings, aes(x=Genre, y=popularity)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2

# 3. How does appr/enjoyment with movie gross income?

appr_enj_by_income <-
  movies %>%
  mutate(InflatedGross = InflatedGross/10^6) %>%
  group_by(InflatedGross) %>%
  summarise(avg_appr=mean(Appreciation), avg_enj=mean(Enjoyment)) %>%
  gather("Enj_Appr", "Avg_Val", 2:3)

plot3 <-
  ggplot(appr_enj_by_income, aes(x=InflatedGross, y=Avg_Val, color=Enj_Appr)) +
  geom_point() +
  geom_smooth() +
  labs(x="Inflated Gross (in Millions)") +
  xlim(0, quantile(appr_enj_by_income$InflatedGross, 0.9))

plot3

# 4. Are older movies more likely to be considered meaningful?


year_release_vs_meaningful <-
  movies %>%
  group_by(Year) %>%
  summarise(avg=mean(Appreciation))

plot4 <-
  year_release_vs_meaningful %>%
  ggplot(aes(x=Year, y=avg)) +
  geom_point() +
  geom_smooth()

plot4


movies_with_genre <-
  movies %>%
  gather("Genre", "isGenre", 19:34) %>%
  filter(isGenre==1)

X <- model.matrix(Appreciation ~ Genre, data=movies_with_genre)
Y <- as.matrix(movies_with_genre$Appreciation)

reg <- cv.glmnet(X, Y, alpha=1)
summary(reg)

reg2 <- glm(Appreciation ~ Classic, data=movies_with_genre)
summary(reg2)
plot(reg2)

preds <- movies_with_genre %>%
  distinct(Classic)

preds$predicted <- predict(reg2, preds)
head(preds)

plot5 <-
  ggplot(aes(x=Classic, y=Appreciation), data=movies_with_genre) +
  geom_point() +
  geom_line(aes(x=Classic, y=predicted), data=preds)

plot5

save(plot1, plot2, plot3, plot4, plot5, file="outputs.RData")






































