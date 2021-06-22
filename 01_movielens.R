#### INTRODUCTION ---------------------------------------------------------

# This code is part of the Capstone Project of the Course "Introduction to Data Science" of Harvardx by Rafael Irizarry.
# There are many comments within this code so anyone interested could follow it without the need of additional documents (i.e. Markdown files)
# Note that this code has sections, so the reader can jump to different parts of the code by clicking:
# Shift+Alt+J (windows) or Cmd+Shift+Option+J (mac) and selecting a section with up/down arrow + ENTER
# which i strongly recommend.

# The main goal of this project is to build a recommendation system for movies.



####. . . i. Explanation by the EDX Team ---------------------------------------------------------


# You will use the following code to generate your datasets. Develop your algorithm using the edx set. For a final test of your final algorithm,
# predict movie ratings in the validation set (the final hold-out test set) as if they were unknown. RMSE will be used to evaluate how close your
# predictions are to the true values in the validation set (the final hold-out test set).
# 
# Important: The validation data (the final hold-out test set) should NOT be used for training, developing, or selecting your algorithm and it
# should ONLY be used for evaluating the RMSE of your final algorithm. The final hold-out test set should only be used at the end of your project
# with your final model. It may not be used to test the RMSE of multiple models during model development. You should split the edx data into
# separate training and test sets to design and test your algorithm.



####. . . ii. Code provided ---------------------------------------------------------

# Create edx set, validation set (final hold-out test set)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Save the objects in a folder "data" of your working directory: 
dir.create("data")
saveRDS(edx,"data/edx.rda")
saveRDS(validation,"data/validation.rda")

# Clear temporal files
rm(dl, ratings, movies, test_index, temp, movielens, removed)



#### I. LOAD LIBRARIES ---------------------------------------------------------

if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")

library(kableExtra)
library(gridExtra)
library(stringr)
library(lubridate)
library(Matrix)



#### II. DATA EXPLORATION (DESCRIPTIVE STATISTICS) ---------------------------------------------------------


# Our train set (edx) and our test set (validation) are composed by 6 columns or variables:

head(edx)

# Title and Genres are variables of class character, the rest of the variables are numeric.
# Note that the title contains the release year in parentheses, a movie can have several genres separated by a pipe operator ("|")
# and the timestamp variable represents seconds since January 1, 1970 (so technically it is a date and we should treat it like so).
# The total of observations are 9,000,055 and each one represents a rating of a movie given by a user.

str(edx)

# The dataset has a total of:

length(edx$userId) # total observations

ds_movies <- nrow(edx %>% distinct(movieId))
ds_movies # movies

ds_users <- nrow(edx %>% distinct(userId))
ds_users # users

ds_genres <- nrow(edx %>% distinct(genres))
ds_genres # group of genres

year(as_datetime(min(edx$timestamp))) # year of first rating
year(as_datetime(max(edx$timestamp))) # year of last ratinge

# Frequence of ratings (4 stars is the most frequent rating):
fig <- edx %>%
  ggplot(aes(rating)) +
  geom_bar(alpha=0.5,fill="#0072B2",colour="#0072B2") +
  xlab("Rating") +
  scale_y_continuous("Cases", labels = scales::comma) +
  theme_bw()
fig

# Create figure:
png(filename="figs/ratings_frequence.png", width=1000, height=538)
plot(fig)
dev.off()


# We can see that the title of each movie contains the release year. We would add a column to both sets with this value:

edx <- edx %>%
  mutate(year = as.numeric(str_extract(str_sub(title,start=-6),"(\\d{4})")))

validation <- validation %>%
  mutate(year = as.numeric(str_extract(str_sub(title,start=-6),"(\\d{4})")))

# To analyse the timestamp, which is the date when the movie was rated by a user, we transform the class from numeric to date
# And then we express that date as the first day of the week (so we can broup by just a few distinct dates):

edx <- edx %>%
  mutate(date = as_datetime(timestamp))

edx <- edx %>%
  mutate(week=week(date), rating_date = round_date(date,unit="week"))

edx <- edx %>%
  select(-date,-week)

validation <- validation %>%
  mutate(date = as_datetime(timestamp))

validation <- validation %>%
  mutate(week=week(date), rating_date = round_date(date,unit="week"))

validation <- validation %>%
  select(-date,-week)

# Now our data set looks like this:
edx



#### III. RELATIONSHIP BETWEEN VARIABLES AND RATINGS ---------------------------------------------------------


####. . . III.I By User ---------------------------------------------------------

users <- edx %>%
  group_by(userId) %>%
  summarize(cases=n(),avg_rating = mean(rating)) %>%
  arrange(desc(cases))

sd <- sd(users$avg_rating)

mean <- mean(users$avg_rating)

p1 <- users %>%
  ggplot(aes(avg_rating,cases)) +
  geom_point(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle("Average Ratings by User") +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

p2 <- users %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle(str_c("mean = ",round(mean,2),"  sd = ",round(sd,3),sep="")) +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

# Here is an histogram and a scatterplot of the average ratings by user:
fig_users <- grid.arrange(p1,p2, ncol = 2)



####. . . III.II By Movie ---------------------------------------------------------


movies <- edx %>%
  group_by(movieId) %>%
  summarize(cases=n(),avg_rating = mean(rating)) %>%
  arrange(desc(cases))

sd <- sd(movies$avg_rating)

mean <- mean(movies$avg_rating)

p1 <- movies %>%
  ggplot(aes(avg_rating,cases)) +
  geom_point(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle("Average Ratings by Movie") +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

p2 <- movies %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle(str_c("mean = ",round(mean,2),"  sd = ",round(sd,3),sep="")) +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

# Here is an histogram and a scatterplot of the average ratings by movie:
fig_movies <- grid.arrange(p1,p2, ncol = 2)



####. . . III.III By Genre ---------------------------------------------------------


genres <- edx %>%
  group_by(genres) %>%
  summarize(cases=n(),avg_rating = mean(rating)) %>%
  arrange(desc(cases))

sd <- sd(genres$avg_rating)

mean <- mean(genres$avg_rating)

p1 <- genres %>%
  ggplot(aes(avg_rating,cases)) +
  geom_point(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle("Average Ratings by Group of Genres") +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

p2 <- genres %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle(str_c("mean = ",round(mean,2),"  sd = ",round(sd,3),sep="")) +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

# Here is an histogram and a scatterplot of the average ratings by genres:
fig_genres <- grid.arrange(p1,p2, ncol = 2)



####. . . III.IV By Premiere Year ---------------------------------------------------------


years <- edx %>%
  group_by(year) %>%
  summarize(cases=n(),avg_rating = mean(rating)) %>%
  arrange(desc(cases))

sd <- sd(years$avg_rating)

mean <- mean(years$avg_rating)

p1 <- years %>%
  ggplot(aes(avg_rating,cases)) +
  geom_point(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle("Average Ratings by Release Year") +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

p2 <- years %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  #scale_y_log10() +
  ggtitle(str_c("mean = ",round(mean,2),"  sd = ",round(sd,3),sep="")) +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

# Here is an histogram and a scatterplot of the average ratings by release year (premiere year):
fig_years <- grid.arrange(p1,p2, ncol = 2)



####. . . III.V By Date of Rating ---------------------------------------------------------


rating_date <- edx %>%
  group_by(rating_date) %>%
  summarize(cases=n(),avg_rating = mean(rating)) %>%
  arrange(desc(cases))

sd <- sd(rating_date$avg_rating)

mean <- mean(rating_date$avg_rating)

p1 <- rating_date %>%
  ggplot(aes(avg_rating,cases)) +
  geom_point(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle("Average Ratings by Date of Rating") +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

p2 <- rating_date %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(alpha=0.2,fill="#0072B2",colour="#0072B2") +
  ggtitle(str_c("mean = ",round(mean,2),"  sd = ",round(sd,3),sep="")) +
  xlab("Average Rating") +
  ylab("Cases") +
  geom_vline(xintercept = mean,linetype = "dashed", size = 1) +
  theme_bw()

# Here is an histogram and a scatterplot of the average ratings by date of rating:
fig_rating_date <- grid.arrange(p1,p2, ncol = 2)

# A larger standard deviation means greater variability in the data.
# So ratings vary substantially by movie, genre and user; And in a lesser way by release year and date of rating.

# Here we can see all the plots together:
fig_all <- grid.arrange(fig_movies,fig_genres,fig_users,fig_years,fig_rating_date, ncol = 1)

# Create figure:
png(filename="figs/average_ratings_by_variable.png", width=1145, height=1000)
plot(fig_all)
dev.off()

# Remove objects:
rm(fig, fig_all,fig_genres,fig_movies,fig_rating_date,fig_users,fig_years,p1,p2,rating_date,users,genres,movies,years)
gc()
memory.size (max=FALSE)


#### IV. RMSE FUNCTION ---------------------------------------------------------


# We define the loss function to measure the accuracy of our predictions

RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

# We will keep track of the predictions in our validation set, beginning with the true rating. We also will record the times a movie has been rated
# in the edx set and the times a user has rated a movie in the edx set, as a reference for better understanding the results of our predictions.

# Add movie times + user times:

movie_cases_train <- edx %>%
  group_by(movieId) %>%
  summarize(movie_cases_train_train = n())

user_cases_train <- edx %>%
  group_by(userId) %>%
  summarize(user_cases_train_train = n())

prediction_results <- validation %>%
  left_join(movie_cases_train, by = "movieId") %>%
  left_join(user_cases_train, by = "userId")

# Add true ratings:

prediction_results <- prediction_results %>%
  mutate(true_rating = validation$rating)

head(prediction_results)



#### V. NORMALIZATION EFFECTS MODELS ---------------------------------------------------------

####. . . V.I Model 1 Average ---------------------------------------------------------


method_number <- 1
method <- "average"
method_abv <- "avg"
method_desc <- "rating average of all the observations in the dataset"

# As our first approach, we will simply predict the mean rating of all movies

mu <- mean(edx$rating)

prediction <- rep(mu,nrow(validation)) %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- data.frame(ID = method_number,Method = method,RMSE = rmse_prediction, Description = method_desc)

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction) #Prediction are the same since we used the average no matter the user nor the movie



####. . . V.II Model 2 Average + Movie Effect ---------------------------------------------------------


method_number <- 2
method <- "average + movie effect"
method_abv <- "avg_movie_effect"
method_desc <- "rating average of all observations plus bias due to movie effect"

# Now we will add each of the variables effect to the mean rating of all movies. Let's start with the movie effect

mu <- mean(edx$rating)

b_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_m, by='movieId') %>%
  .$b_m %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction)



####. . . V.III Model 3 Average + Genre Effect ---------------------------------------------------------


method_number <- 3
method <- "average + genre effect"
method_abv <- "avg_genre_effect"
method_desc <- "rating average of all observations plus bias due to genre effect"

# This prediction considers just the genre effect over the mean rating of all movies

mu <- mean(edx$rating)

b_g <- edx %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_g, by='genres') %>%
  .$b_g %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction)

# At this point we see that the average + the user effect produces a better prediction than the average + the genre effect
# Let's see how the user effect behaves.



####. . . V.IV Model 4 Average + User Effect ----


method_number <- 4
method <- "average + user effect"
method_abv <- "avg_user_effect"
method_desc <- "rating average of all observations plus bias due to user effect"

# This prediction considers just the user effect over the mean rating of all movies

mu <- mean(edx$rating)

b_u <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_u, by='userId') %>%
  .$b_u %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:
rmse_results
range(prediction)

# Now we see that the prediction using the average + the user effect performs better than the average + the genre effect but worst than
# the average + the movie effect. This makes much sense since movies tend to be considered in general as good or bad. Let's consider for example
# the academy awards, many people agree in which movies are better, despite often disagree in which movie is the best. And that is explained by
# the user effect: a user tend to rate a movie using the same criteria based on his own taste.
# On the other hand, genre is less accurate since a user could like and dislike movies of the same genre. This variable is less particular
# than the other two. Nontheless, it's a better start to predict movies than just considering the average alone.



####. . . V.V Model 5 Average + Year Effect ---------------------------------------------------


method_number <- 5
method <- "average + year effect"
method_abv <- "avg_year_effect"
method_desc <- "rating average of all observations plus bias due to year effect"

# This prediction considers just the release year effect over the mean rating of all movies
# This variable is less particular so we can imagine that its performance will be worst than the movie and user effect.

mu <- mean(edx$rating)

b_y <- edx %>% 
  group_by(year) %>% 
  summarize(b_y = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_y, by='year') %>%
  .$b_y %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results # Our assumption stands corrected
range(prediction)



####. . . V.VI Model 6 Average + Rating Date Effect ---------------------------------------------------


method_number <- 6
method <- "average + rating date effect"
method_abv <- "avg_date_effect"
method_desc <- "rating average of all observations plus bias due to rating date effect"

# This prediction considers just the rating date effect over the mean rating of all movies
# As our previous method, this seems to be a pretty generic effect.

mu <- mean(edx$rating)

b_d <- edx %>% 
  group_by(rating_date) %>% 
  summarize(b_d = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_d, by='rating_date') %>%
  .$b_d %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction)



####. . . V.VII Model 7 Average + Movie Effect + User Effect ------------------------------------


method_number <- 7
method <- "average + movie + user effect"
method_abv <- "avg_movie_user_effect"
method_desc <- "rating average of all observations plus bias due to movie and user effect"

# Now let's try the strongest effects together. Those are the movie and the user effect.

mu <- mean(edx$rating)

b_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

b_u <- edx %>%
  left_join(b_m,by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_m))

prediction <- validation %>% 
  left_join(b_m, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + b_m + b_u) %>%
  .$pred %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction)



####. . . V.VIII Model 8 Average + Movie Effect + User Effect (OOR Correction) --------------------------------------------------


method_number <- 8
method <- "average + movie + user effect (OOR correction)"
method_abv <- "avg_movie_user_effect_corrected"
method_desc <- "rating average of all observations plus bias due to movie and user effect with out of range correction"

# When mixing both effects now we see that our lowest prediction is less than 0 and our higher prediction is greater than 5 which in fact are out
# of the real rating parameters:

prediction <- validation %>% 
  left_join(b_m, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = ifelse(mu + b_m + b_u < 0,0,ifelse(mu + b_m + b_u >5,5,mu + b_m + b_u))) %>%
  .$pred %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results      # Turns out that the difference is marginal
range(prediction) # No predictions are out of range 0-5



####. . . V.IX Model 9 Average + All Effects --------------------------------------------------


method_number <- 9
method <- "average + all effects"
method_abv <- "avg_all_effects"
method_desc <- "rating average of all observations plus bias due to movie, user, genre, year and date of rating effect"

# Our next model will consider all of the effects together. This time we will correct from the beginning all the predictions that are out of range.

mu <- mean(edx$rating)

b_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

b_u <- edx %>%
  left_join(b_m,by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_m))

b_g <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_m - b_u))

b_d <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_g,by="genres") %>%
  group_by(rating_date) %>% 
  summarize(b_d = mean(rating - mu - b_m - b_u - b_g))

b_y <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_g,by="genres") %>%
  left_join(b_d,by="rating_date") %>%
  group_by(year) %>% 
  summarize(b_y = mean(rating - mu - b_m - b_u - b_g - b_d))

prediction <- validation %>% 
  left_join(b_m, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_d,by="rating_date") %>%
  left_join(b_y,by="year") %>%
  mutate(pred = ifelse(mu + b_m + b_u + b_g + b_d + b_y < 0,0,ifelse(mu + b_m + b_u + b_g + b_d + b_y >5,5,mu + b_m + b_u + b_g + b_d + b_y))) %>%
  .$pred %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction) # No predictions are out of range 0-5



####. . . V.X Model 10 Regularization ------------------------------------


method_number <- 10
method <- "average + all effects with regularization"
method_abv <- "avg_all_effects_regularization"
method_desc <- "rating average of all observations plus bias due to all effect with regularization"

####. . . . . . V.X.I Approach ------------------------------------


# One problem with our predictions in the last model (which seems to be the best in terms of the RMSE) is that, the estimates of the biases
# (b_m, b_u, b_g, etc.) are prone to be larger (positive or negative) when there is just a few samples when estimated. For example, here are the
# movies which were rated 20 or fewer times, what will cause b_m to be larger and hence increase our RMSE:

movies_20_less <- nrow(edx %>%
                         group_by(movieId) %>%
                         summarize(cases = n()) %>%
                         ungroup() %>%
                         filter(cases<=20))
movies_20_less

movies_20_less / ds_movies # these movies represent 18.10% of all the movies in the train set

# And here are the users that rated 20 or fewer movies, what will cause b_u to be larger and hence increase our RMSE:

users_20_less <- nrow(edx %>%
                        group_by(userId) %>%
                        summarize(cases = n()) %>%
                        ungroup() %>%
                        filter(cases<=20))
users_20_less

users_20_less / ds_users # these users represent 7.1% of all the users in the train set

# So we will use regularization to penalize the large estimates that are produced when using small samples.
# Note that lambda is a parameter that can be tuned, thus we can seek the best value of lambda which minimizes the RMSE. This means that we need
# to evaluate different values of lambda to see which one leads us to the best prediction. But we can't use our test set to seek for the best value
# of lambda, so we will use cross-validation (CV) to accomplish our goal using 10 folds (k) of the 90% of the data in our edx set to train our
# model and then use the remaining 10% to validate the RMSE in our edx set and find the best lambda
# (note that we use the train set all the time, we are not using the validation set in the tuning process)



####. . . . . . V.X.II Tuning for Lambda ------------------------------------

# We set seed to 1, for reproducibility:

set.seed(1, sample.kind="Rounding")

k_folds_index <- createFolds(edx$movieId,k=10,list=TRUE,returnTrain = TRUE) #We create 10 folds to perform CV.

mu <- mean(edx$rating)

lambdas <- seq(3.5,5.5,.15) # Range adjusted from 3.5 to 5.5 after analysis, but it can be any range.

# Partitioning train set in two: train_fold = 90% of the k_th fold and validation_fold = 10% of the remaining data.

k <- 1                                          # Here is an example of the 1st fold
train_fold <- edx[k_folds_index[[k]],]          # This is the train set used for cross validation where k indicates the number of the fold.
validation_fold <- edx[-k_folds_index[[k]],]    # This is the validation set used for the cross validation

# Now we can tune for lambda with the following function.
# We are going to use the first fold to verify the code. In the next section i will provide the results of the ten folds.
# So let's test our function (this could take several minutes):

rmses <- sapply(lambdas,function(lambda){
  b_m <- train_fold %>% 
    group_by(movieId) %>% 
    summarize(b_m = sum(rating - mu)/(n()+lambda))
  
  b_u <- train_fold %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_m)/(n()+lambda))
  
  b_g <- train_fold %>%
    left_join(b_m,by="movieId") %>%
    left_join(b_u,by="userId") %>%
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_m - b_u)/(n()+lambda))
  
  b_d <- train_fold %>%
    left_join(b_m,by="movieId") %>%
    left_join(b_u,by="userId") %>%
    left_join(b_g,by="genres") %>%
    group_by(rating_date) %>% 
    summarize(b_d = sum(rating - mu - b_m - b_u - b_g)/n()+lambda)

  b_y <- train_fold %>%
    left_join(b_m,by="movieId") %>%
    left_join(b_u,by="userId") %>%
    left_join(b_g,by="genres") %>%
    left_join(b_d,by="rating_date") %>%
    group_by(year) %>% 
    summarize(b_y = sum(rating - mu - b_m - b_u - b_g - b_d)/(n()+lambda))
  
  prediction <- validation_fold %>% 
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    left_join(b_d, by="rating_date") %>%
    left_join(b_y, by="year") %>%
    mutate(pred = ifelse(mu + b_m + b_u + b_g + b_d + b_y < 0,
                         0,
                         ifelse(mu + b_m + b_u + b_g + b_d + b_y >5,
                                5,
                                mu + b_m + b_u + b_g + b_d + b_y)))

  # Since we splitted the data 90%-10% it is possible that some observations result in NA when left-joining.
  # So we identify those observations and substract them from both sets and calculate RMSE only with the remaining observations:
  
  index_preserve <- which(!is.na(prediction$pred))
  p <- prediction$pred[index_preserve]
  v <- validation_fold$rating[index_preserve]
  
  return(RMSE(p,v))
})

# The rmse for each lambda in our first fold (k=1) is:

cbind(k,lambdas,rmses)


####. . . . . . V.X.III Load Entire Results from GitHub ------------------------------------


# We could repeat the previous code for all folds. For practicality, i provide a URL to download the RDA file with the full results.
# Remember that this process needs to be manually:
# https://github.com/rafaelyanlab/movielens/raw/master/rda/tuning_results_lambda.rda
# And save the object on the folder "rda/".

# Load object:
tuning_results_lambda <- readRDS("rda/tuning_results_lambda.rda")

# The RMSES for each lambda and each fold are:

tuning_results_lambda

# With these results we calculate the mean rmses for each lambda:

tuning_results_lambda <- tuning_results_lambda %>%
  mutate(mean_rmse = rowMeans(select(., rmses1:rmses10))) 

tuning_results_lambda

# the lambda with the lowest mean rmse is the best lambda (our tuned penalty parameter):

lambdas[which.min(tuning_results_lambda$mean_rmse)]

# Let's plot the 10 results:

tidy_lambdas <- tuning_results_lambda %>%
  gather("k_fold","rmses",rmses1:rmses10)

tidy_lambdas <- tidy_lambdas %>%
  mutate(k_fold = str_replace(k_fold,"rmses",""))

plot_lambda <- function(k){
  df <- tidy_lambdas %>%
    filter(k_fold == k)
  
  min_lambda <- df$lambdas[which.min(df$rmses)]
  min_rmse <- min(df$rmses)
  
  p <- df %>%
    ggplot(aes(lambdas,rmses)) +
    geom_point(colour="#0072B2") +
    geom_vline(xintercept = min_lambda,linetype = "dashed", colour = "red", alpha=0.4, size = .6) +
    geom_hline(yintercept = min_rmse,linetype = "dashed", colour = "red", alpha=0.4, size = .6) +
    ggtitle(str_c("k fold = ", k ,"  best lambda= ",min_lambda,"  min rmse= ",round(min_rmse,4),sep="")) +
    xlab("lambda") +
    ylab("RMSE") +
    theme_bw() +
    theme(plot.title = element_text(size = 8, face = "bold"))
  
  return(list(p))
  
  }

k <- c(1:10)

p_all <- sapply(k,plot_lambda)

p_all <- do.call("grid.arrange", c(p_all, ncol=3))

# Create figure:
png(filename="figs/tuning_for_lambda.png", width=1000, height=998)
plot(p_all)
dev.off()


# We observe that the best lambda for each fold ranges between 4.55 and 5.15. But the one with the minimun mean RMSE is:
lambdas[which.min(tuning_results_lambda$mean_rmse)]



####. . . . . . V.X.IV Predict with Best Lambda ------------------------------------


mu <- mean(edx$rating)

# We define best lambda as:

best_lambda <- 4.7

b_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = sum(rating - mu)/(n()+best_lambda))

b_u <- edx %>% 
  left_join(b_m, by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_m)/(n()+best_lambda))

b_g <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_m - b_u)/(n()+best_lambda))

b_d <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_g,by="genres") %>%
  group_by(rating_date) %>% 
  summarize(b_d = sum(rating - mu - b_m - b_u - b_g)/n()+best_lambda)

b_y <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_g,by="genres") %>%
  left_join(b_d,by="rating_date") %>%
  group_by(year) %>% 
  summarize(b_y = sum(rating - mu - b_m - b_u - b_g - b_d)/(n()+best_lambda))

prediction <- validation %>% 
  left_join(b_m, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_d, by="rating_date") %>%
  left_join(b_y, by="year") %>%
  mutate(pred = ifelse(mu + b_m + b_u + b_g + b_d + b_y < 0,
                       0,
                       ifelse(mu + b_m + b_u + b_g + b_d + b_y >5,
                              5,
                              mu + b_m + b_u + b_g + b_d + b_y))) %>%
  .$pred %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))


prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results
range(prediction)



####. . . V.XI Model 11 Average + Regularization Best Effects ------------------------------------


method_number <- 11
method <- "average + best effects regularization"
method_abv <- "avg_best_effects_regularization"
method_desc <- "rating average of all observations plus bias due to all effect with regularization on movie, user and genre effects"

# Despite so far this prediction is the best, look how large are b_d and b_y after penalization:

range(b_m$b_m)
range(b_u$b_u)
range(b_g$b_g)
range(b_d$b_d)
range(b_y$b_y)

# So what if we use regularization just on the strongest effects. Now, we change our estimates of b_d and b_y and look for the best lambda.
# This time we can use +-values respect our previous best lambda (4.7) and find a inflection point (or use the code we used for tuning for lambda).
# With just few iterations (i tried 4.65, 4.75, 4.8, 5 and 5.15) we arrive to our best new lambda:

mu <- mean(edx$rating)

best_lambda <- 5

b_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = sum(rating - mu)/(n()+best_lambda))

b_u <- edx %>% 
  left_join(b_m, by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_m)/(n()+best_lambda))

b_g <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_m - b_u)/(n()+best_lambda))

b_d <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_g,by="genres") %>%
  group_by(rating_date) %>% 
  summarize(b_d = mean(rating - mu - b_m - b_u - b_g))

b_y <- edx %>%
  left_join(b_m,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_g,by="genres") %>%
  left_join(b_d,by="rating_date") %>%
  group_by(year) %>% 
  summarize(b_y = mean(rating - mu - b_m - b_u - b_g - b_d))

# Before computing our prediction we will save an object with all movies, users, ratings and predicted ratings with this last model:
# (we will use this dataset later in the next section: Matrix Completion)

edx_model_11 <- edx %>% 
  left_join(b_m, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_d, by="rating_date") %>%
  left_join(b_y, by="year") %>%
  mutate(rating_hat = ifelse(mu + b_m + b_u + b_g + b_d + b_y < 0,
                             0,
                             ifelse(mu + b_m + b_u + b_g + b_d + b_y >5,
                                    5,
                                    mu + b_m + b_u + b_g + b_d + b_y))) %>%
  select(movieId,userId,rating,rating_hat) %>%
  as.data.frame()

saveRDS(edx_model_11,"rda/edx_model_11.rda")

prediction <- validation %>% 
  left_join(b_m, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_d, by="rating_date") %>%
  left_join(b_y, by="year") %>%
  mutate(pred = ifelse(mu + b_m + b_u + b_g + b_d + b_y < 0,
                       0,
                       ifelse(mu + b_m + b_u + b_g + b_d + b_y >5,
                              5,
                              mu + b_m + b_u + b_g + b_d + b_y))) %>%
  .$pred %>%
  as.data.frame()

colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

prediction_results <- cbind(prediction_results,prediction)

# Result of Prediction:

rmse_results      # Well... the improvement was just marginal
range(prediction)



#### VI. MATRIX FACTORIZATION -----------------------------------------------------------


# So far, the algorithms implemented have considered just global and isolated factors (i.e. movie effect, user effect, etc.), those are the bias
# in ratings considering a specific effect. But there are in fact other latent factors that can explain most of the variation in the ratings we
# observed.

# Our models, as explained by Rafael Irizarry in his book Introduction to Data Science: "[...] leaves out an important source of variation
# related to the fact that groups of movies have similar rating patterns and groups of users have similar patterns" (Irizarry, 2020, p.626).

# For example, if a user likes Harry Potter 3 there's a good chance that he would enjoy the rest of the saga, moreover there is a chance
# that he would enjoy other films like Lord of the Rings, Star Wars, Indiana Jones, Pan's Labyrinth, Jurassic Park and Noah. Here are some
# assumptions why this may happen:

# a). The fan factor: The user is very into the Harry Potter world, he knows the story, the characters, the places, etc. so he would enjoy all the
# saga despite of the quality, the cast or the production of the film.
# b). The fantasy factor: Letting aside that he is a fan, in general he likes fantasy so he also will enjoy Lord of the Rings or Star Wars.
# c). The direction and production factor: Not only he likes fantasy but he likes very much Cuaron's films so maybe he will enjoy Pan's Labyrinth.
# d). The music factor: The user is also very auditory, so maybe John Williams' music causes him goosebumps, so maybe Jurassic Park has the right
# mix of fantasy and music he expects in a movie, and so has Star Wars.
# e). The cast factor: Besides, the user likes very much Emma Watson, so Noah maybe has the right mix of fantasy and Watson he likes.

# Now if we consider that there are many patterns like the ones described above, theoretically, if we could realize of the existance of this
# patterns, we could decompose our prediction in factors making it more accurate. So how can we find these patterns?

# One way is to use Principal Component Analysis (PCA) or Singular Value Decomposition (SVD) which allow us to find the correlations (patterns)
# or the lack of correlation among the variables in a matrix, i.e. a user-movie ratings matrix where movies are columns, users are rows and the
# numbers inside the matrix are the ratings for each combination of column and row (user-movie). These methods will decompose our matrix in
# Principal Components that can be thought as the patterns we are looking for.  

# This decomposition is called matrix factorization and the mathematical principles applied in these methods permits us to find the similarities
# among our movies and our users, no matter if the "latent factors" that give us these similarities are interpretable or not. In other words, we
# don't have to be aware if a fan, music, fantasy, direction or cast factor exists, the method will just find those patterns.

# PCA and SVD are in fact known as dimensionality reduction methods that we will use for matrix factorization purposes. Here are two links that
# clearly explain the basics of dimension reduction: 
# 1). Josh Starmer's "StatQuest: PCA main ideas in only 5 minutes!!!": https://www.youtube.com/watch?v=HMOI_lkzW08
# 2). Luis Serrano's "Principal Component Analysis (PCA)": https://www.youtube.com/watch?v=g-Hb26agBFg 

# PCA and SVD have the benefit of reducing the dimensions of a data set. As explained by Zakaria Jaadi (2021) in his article "A STEP-BY-STEP
# EXPLANATION OF PRINCIPAL COMPONENT ANALYSIS (PCA)" (https://builtin.com/data-science/step-step-explanation-principal-component-analysis):

# "Reducing the number of variables of a data set naturally comes at the expense of accuracy, but the trick in dimensionality reduction
# is to trade a little accuracy for simplicity. Because smaller data sets are easier to explore and visualize and make analyzing data much
# easier and faster for machine learning algorithms without extraneous variables to process".

# But also PCA has the benefit of capturing in the process those "latent factors" or "patterns" we previously described. So we will use PCA and
# SVD in our next models.



####. . . VI.I Matrix Completion -----------------------------------------------------------

# Until now we have gone from our first model (average, which means you are practically guessing) with a RMSE of 1.0612018 to our best model
# with a RMSE of 0.863954. That is an improvement of 18.58%.
1 - 0.8639540 / 1.0612018

# We achieve this by simply normalizing the global effects (user, movie, genre, etc.). Redarding this approach Yehuda Koren, one of the winner of
# the Netflix challenge, wrote in his article "The BellKor Solution to the Netflix Grand Prize" (2009):

# "Out of the numerous new algorithmic contributions, I would like to highlight one – those humble baseline predictors (or biases), which capture
# main effects in the data. While the literature mostly concentrates on the more sophisticated algorithmic aspects, we have learned that an
# accurate treatment of main effects is probably at least as significant as coming up with modeling breakthroughs".

# Remember we want to predict the ratings that all users will give to all movies.
# So if we create a matrix with users in rows and movies in columns, we will find out that our matrix is pretty sparse.
# This makes sense, since normally the repertoire is huge but our capacity to see/rate all movies is limited.
# Think for instance of Amazon's inventory and compare it to the products that you have bought, looked at or searched for up until now.
# So, let's calculate the sparsity of a matrix with only the most rated movies and the most assiduous users, say at least 100 cases:

best_movies <- edx %>%
  group_by(movieId) %>%
  summarize(cases = n()) %>%
  filter(cases >= 100) %>%
  .$movieId

length(best_movies) #5,711 movies

best_users <- edx %>%
  group_by(userId) %>%
  summarize(cases = n()) %>%
  filter(cases >= 100) %>%
  .$userId

length(best_users) #24,115 users

edx_best_observations <- edx %>%
  filter(movieId %in% best_movies, userId %in% best_users)

# Our matrix still has more than 6.7 million observations:

str(edx_best_observations)

# We transform our dense matrix into a sparse matrix with:

edx_sparse_matrix <- sparseMatrix(i = edx_best_observations$userId,
                                  j = edx_best_observations$movieId,
                                  x = edx_best_observations$rating)

# Why our dimensions are 71565 x 63113 ? It should be a 24,115 x 5,711 matrix. This definitely is not what we want

dim(edx_sparse_matrix)

# This is because i and j are taken as sequential (1 up to maximum ID) even if they don't exist. So to correct that we change our code to:

edx_sparse_matrix <- sparseMatrix(
  as.integer(factor(edx_best_observations$userId)),
  as.integer(factor(edx_best_observations$movieId)),
  x=edx_best_observations$rating)

# Now we got what we want:

dim(edx_sparse_matrix)

class(edx_sparse_matrix)

# The sparsity of our matrix (the proportion of missing values) is defined as:

sparsity <- 1 - ( length(edx_sparse_matrix@x) / edx_sparse_matrix@Dim[1] / edx_sparse_matrix@Dim[2])

# This means 95% is the proportion of ratings we must predict in order to fill the matrix

sparsity



####. . . VI.II Choosing a Completion Technique -----------------------------------------------------------


# All the models we have created until now, are completing a sparse user-movie matrix:
# a). Model 1 is replacing all the missing values by the average rating of all movies.
# b). Model 11 is replacing all the missing values by the average plus a penalized bias caused by a specific effect (movie, user, genre, year and
# rating date).

# Now we will try two additional models:

# 1). Model 12 - Replace missing values with zeros: As explained by Rafael Irizarry in his book Introduction to Data Science: "In general, the
# choice of how to fill in missing data, or if one should do it at all, should be made with care" (Irizarry, 2020, p. 642). If we have the true
# ratings in our user-movie matrix and we substract the prediction we obtained with our last linear model, then we obtain the rating residuals for
# each known user-movie combination in our matrix. Then we will replace the missing ratings by zeros. Note that because the residuals are centered
# at zero, imputing zeros to the missing values will not affect our matrix. Finally, we will apply PCA to the residuals matrix to detect the
# patterns between groups of movies and groups of users and then we will sum this residuals to our last linear model. By doing this we train a
# model that considers the regularized global effects (of the linear models) and the "latent patterns" (obtained with matrix factorization).

# 2). Model 13 - Replace missing values using Expectation-Maximization (EM): As explained by Jason Brownlee (2019) in his article "A Gentle
# Introduction to Expectation-Maximization (EM Algorithm)" (https://machinelearningmastery.com/expectation-maximization-em-algorithm/): "The EM
# algorithm is an iterative approach that cycles between two modes. The first mode attempts to estimate the missing or latent variables, called
# the estimation-step or E-step. The second mode attempts to optimize the parameters of the model to best explain the data, called the
# maximization-step or M-step". We will use the "eimpute" function in R which iterates using EM and then approximates the completed matrix via
# truncated SVD.

# Other techniques that can be used for matrix completion are: Convex Relaxation, Stochastic Gradient Descent (SGD) and Alternating Least Squares
# Minimization (ALS).

# REFERENCES:
# Rank (linear algebra) From Wikipedia https://en.wikipedia.org/wiki/Rank_(linear_algebra)
# Matrix completion From Wikipedia https://en.wikipedia.org/wiki/Matrix_completion#:~:text=Various%20matrix%20completion%20algorithms%20have,and%20alternating%20minimization%2Dbased%20algorithm.
# Expectation–maximization algorithm From Wikipedia https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm
# Low-Rank Matrix Completion Yuejie Chi [2018] http://users.ece.cmu.edu/~yuejiec/papers/LRMC_spm.pdf



####. . . VI.III Model 12 Average + Regularization Best Effects + PCA -----------------------------


# To start with this model we will complete a matrix with the residual of all the true ratings minus the predicted ratings of our last model:

edx_model_11 <- edx_model_11 %>% mutate(residual = rating - rating_hat)
head(edx_model_11)

# In order to run further expensive computations, we will keep only the best movies and users
# (if your computer has enough memory, you can keep all the movies and users)

nrow(semi_join(edx_model_11,edx_best_observations))

str(edx_model_11)

# Now we build our sparse matrix:

edx_model_11_sparse <- sparseMatrix(
  as.integer(factor(edx_model_11$userId)),
  as.integer(factor(edx_model_11$movieId)),
  x=edx_model_11$residual)


####. . . . . . VI.III.I Clear R Environment ---------------------------------------------------------------------

# At this point, it is important to save the objects we have created and that we will use later, because the next are very expensive computations,
# so we will clear the R environment to use the most memory we can.

# This will save the objects to our working directory:
saveRDS(edx_model_11,"rda/edx_model_11.rda")
saveRDS(edx_best_observations,"rda/edx_best_observations.rda")
saveRDS(prediction_results,"rda/prediction_results.rda")
saveRDS(rmse_results,"rda/rmse_results.rda")

# Now we can clear all the objects of the R environment (keep only the ones we will need) and use the garbage collection gc() to automatically
# release memory from the objects we just cleared:

rm(list=setdiff(ls(), c("edx_model_11_sparse","RMSE")))
gc()
memory.size (max=FALSE)

# Now we transform our sparse matrix into a dense matrix with all the missing values equal to Zero. Remember that the reason why we choose to
# make all missing values equal to zero is because the residuals are centered at zero, therefore imputing zeros to the missing values will not
# affect our matrix.

edx_model_11_dense <- as.matrix(edx_model_11_sparse)
rm(edx_model_11_sparse)
gc()
memory.size (max=FALSE)

# We assign the names of the users to the rows and the names of the movies to the column of the matrix.

edx_model_11 <- readRDS("rda/edx_model_11.rda")

colnames(edx_model_11_dense) <- edx_model_11 %>%
  distinct(movieId) %>%
  arrange(movieId) %>%
  as.matrix()

rownames(edx_model_11_dense) <- edx_model_11 %>%
  distinct(userId) %>%
  arrange(userId) %>%
  as.matrix()

saveRDS(edx_model_11_dense,"rda/edx_model_11_dense.rda")

rm(edx_model_11)
gc()
memory.size (max=FALSE)

# Here are the first 10 x 10 results of our matrix (note that the data in the matrix corresponds to the rating residuals):

edx_model_11_dense[1:10,1:10]



####. . . . . . VI.III.II PCA ---------------------------------------------------------------------


# We will perform PCA in the matrix with our residual ratings for the best movies and users. The following code takes several minutes
# (at least 60 minutes):

pca_edx_residuals <- prcomp(edx_model_11_dense)
saveRDS(pca_edx_residuals,"rda/pca_edx_residuals.rda")

# If your computer crashes at some point with the code above, you can download the RDA object from:
# Remember that this process needs to be manually:
# https://github.com/rafaelyanlab/movielens/raw/master/rda/pca_edx_residuals.rda
# And save the object on the folder "rda/".

rm(edx_model_11_dense)
gc()
memory.size (max=FALSE)

pca_edx_residuals <- readRDS("rda/pca_edx_residuals.rda")

# Or you can try the following:
# 1. Restart your R session in RStudio.
# 2. Load libraries from section "ii. Code provided" and "I. LOAD LIBRARIES".
# 3. Run function RMSE form section "IV. RMSE FUNCTION".
# 4. Load "edx_model_11_dense.rda" with this piece of code: edx_model_11_dense <- readRDS("rda/edx_model_11_dense.rda")
# 5. Try again the line above with this code: pca_edx_residuals <- prcomp(edx_model_11_dense)

# Let's explore the object with the results of the PCA:

# x = matrix of the coordinates of users projected into each PC and dimensions.
# This is the "recipe for users": the proportion of each user to prepare each Principal Component.

pca_edx_residuals$x[1:10,1:10]

# rotation = matrix of eigenvectors or loading scores.
# This is the "recipe for movies": the proportion of each movie to prepare each Principal Component:

pca_edx_residuals$rotation[1:10,1:10]

# sdev = standard deviations of each Principal Components:

pca_edx_residuals$sdev[1:10]

# center = the values that center each column of the matrix (centering is a must-to-do step in PCA):

pca_edx_residuals$center[1:10]

# scale = the values that scales each column of the matrix (when needed). In this case, we didn't scale the values because all the columns
# have the same weight (residuals of ratings). But we would definitely need to scale if for example some columns are residuals of ratings and
# others are whole ratings, since residuals are significantly smaller.
# As explained by Sudharsan Asaithambi (2017): "Scaling is critical, while performing Principal Component Analysis (PCA). PCA tries to get the
# features with maximum variance and the variance is high for high magnitude features. This skews the PCA towards high magnitude features".
# ("Why, How and When to Scale your Features" https://medium.com/greyatom/why-how-and-when-to-scale-your-features-4b30ab09db5e )

pca_edx_residuals$scale[1:10]

# We can calculate how much variation (sdev^2) in the original data each principal component accounts for:

# Variance in %.
pca_var_per <- round(pca_edx_residuals$sdev^2/sum(pca_edx_residuals$sdev^2)*100, 2) 
pca_var_per

# A scree plot is a graph were we can see the standard deviation of each principal component:
# The scree plot is used to determine the number of components to retain in PCA.

variance_df <- data.frame(pc = 1:length(pca_var_per), variation_perc = pca_var_per) %>%
  mutate(variation_accum = cumsum(variation_perc))

pareto <- 80
pc_pareto <- max(variance_df$pc[which(variance_df$variation_accum<=pareto)]) + 1
variance_accum_pareto <- variance_df$variation_accum[pc_pareto]

fig <- variance_df %>%
  ggplot(aes(x=pc)) +
  geom_bar(aes(y=variation_perc), stat="identity", alpha=0.5, fill="#0072B2", colour="#0072B2") +
  xlab("Principal Component") +
  ylab("Variance (%)") +
  theme_bw() +
  geom_vline(xintercept = pc_pareto,linetype = "dashed", col="red",size = 1) +
  annotate(geom="text",
           x= pc_pareto + 30,
           y= 1,
           hjust= 0,
           label= str_c(pc_pareto," PCs = ",round(variance_accum_pareto,2)," % of variance",sep=""),
           size= 5)

fig

# Create figure:
png(filename="figs/scree_plot.png", width=1000, height=600)
plot(fig)
dev.off()


# We can see that:
# a). The first 100 principal components account just for the 21.82% of the variability:

sum(pca_var_per[1:100]) 

# b). The first 1400 principal components account for 80.44% of the variability:

sum(pca_var_per[1:1400])

# This means that if we want to explain the original user-movie matrix (which has 5,711 dimensions -movies-), we can reduce to just 1,400
# dimensions and explain almost all the variability in our data. Remember that when reducing dimensions you trade a little accuracy for simplicity.



####. . . . . . VI.III.III Principal Components Interpretation ---------------------------------------------------------------------


# Now we will explore the first 2 PCs of the variables (movies). We will show how movies are projected on PC1 and PC2 axes.
# This is, if we imagine we take a picture of the movies from the perspective of PC1 and do the same with PC2, the picture would look something
# like this:

library(ggrepel)
pcs_movies <- data.frame(pca_edx_residuals$rotation[,1:2]) %>%
  mutate(movieId = rownames(pca_edx_residuals$rotation[,1:2]))

rect <- data.frame(x1=c(0.015,-0.05),
                   x2=c(0.05,-0.02),
                   y1=c(-0.025,0),
                   y2=c(0,0.025),
                   r=c(2,1))

fig <- pcs_movies %>%
  ggplot() +
  geom_point(mapping=aes(PC1, PC2),colour="#0072B2",alpha=0.2) +
  ggtitle("PC1 vs PC2 of Movies")  +
  theme_bw() +
  geom_rect(data=rect, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="#009E73", color="#009E73", alpha=0.2) +
  geom_text(data=rect, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=3) +
  theme(plot.title = element_text(size = 12, face = "bold"))

fig

# Create figure:
png(filename="figs/pcs_movies_pca.png", width=1000, height=873)
plot(fig)
dev.off()


# For illustration purposes, we will focus just on the movies in the first 2 cuadrants:

edx_best_observations <- readRDS("rda/edx_best_observations.rda")

pcs_movies_1 <- pcs_movies %>%
  filter(PC1 >= -0.05 & PC1 <= -0.02 & PC2 >= 0 & PC2 <= 0.025)

pcs_movies_1$movieId <- as.numeric(pcs_movies_1$movieId)

pcs_movies_1 <- left_join(pcs_movies_1,edx_best_observations %>%
                            distinct(movieId,title), by="movieId")

# The picture of the first cuadrant is:

highlight_movie <- c(6874,7438,1261,1215,904,1333,2455,2288,1345)
highlight_df <- pcs_movies_1 %>% filter(movieId %in% highlight_movie)

fig <- pcs_movies_1 %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(colour="#0072B2",alpha=0.2) +
  geom_point(data=highlight_df,aes(x=PC1,y=PC2),colour="#D55E00",alpha=0.8) +
  geom_text_repel(aes(PC1, PC2, label=title), size = 2) + 
  ggtitle("Movies Projected on PC1 and PC2 (1st Cuadrant)")  +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"))

fig

# Create figure:
png(filename="figs/movies_pc1_pc2_1C_model12.png", width=1000, height=873)
plot(fig)
dev.off()



# We can observe that, from the point of view of "patterns" PC1 and PC2, there are similarities among some gruesome movies like Kill Bill 1 and
# Kill Bill 2, Evil Dead II and Army of Darkness (Evil Dead III), and some horror movies like Hitchcock's Rear Window and The Birds and some others
# like The Fly, The Thing and Carrie.

# The picture of the second cuadrant is:

pcs_movies_2 <- pcs_movies %>%
  filter(PC1 >= 0.015 & PC1 <= 0.05 & PC2 >= -0.025 & PC2 <= 0)

pcs_movies_2$movieId <- as.numeric(pcs_movies_2$movieId)

pcs_movies_2 <- left_join(pcs_movies_2,edx_best_observations %>%
                            distinct(movieId,title), by="movieId")

highlight_movie <- c(2421,2422,1918,2002,2338,1644,2411,2412,2642,2643,1378,1379)
highlight_df <- pcs_movies_2 %>% filter(movieId %in% highlight_movie)

fig <- pcs_movies_2 %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(colour="#0072B2",alpha=0.2) +
  geom_point(data=highlight_df,aes(x=PC1,y=PC2),colour="#D55E00") +
  geom_text_repel(aes(PC1, PC2, label=title), size = 2) + 
  ggtitle("Movies Projected on PC1 and PC2 (2nd Cuadrant)")  +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"))

fig

# Create figure:
png(filename="figs/movies_pc1_pc2_2C_model12.png", width=1000, height=1300)
plot(fig)
dev.off()



# Here we observe many movies and their sequels like Karate Kid Part II and Part III, Lethal Weapon 3 and Lethal Weapon 4,
# I Know What You Did Last Summer and I Still Know What You Did Last Summer, Rocky IV and Rocky V, Superman III and Superman IV, and
# Young Guns and Young Guns II.

# To sum up, there are many other patterns in our data, and despite we've just plotted two of them, we can certainly use as many as we need to
# make our prediction more accurate. Note that the number of PCs is a parameter in our model so it can be tuned.



####. . . . . . VI.III.IV Final Prediction -----------------------------------------------------------------------


# First we release some memory space:

rm(list=setdiff(ls(), c("pca_edx_residuals","RMSE")))
gc()
memory.size (max=FALSE)

method_number <- 12
method <- "average + best effects regularization + pca"
method_abv <- "avg_best_effects_regularization_pca"
method_desc <- "regularization model plus PCA"

# Then we read the prediction_results object (this object contains the validation set with the results of the prediction of the different
# models we have used):

prediction_results <- readRDS("rda/prediction_results.rda")
head(prediction_results)

# We obtain the movies and the users that exist in the prediction results set:

movies <- prediction_results %>% distinct(movieId) %>% arrange(movieId)
movies$movieId <- as.character(movies$movieId)
str(movies)

users <- prediction_results %>% distinct(userId) %>% arrange(userId)
users$userId <- as.character(users$userId)
str(users)

# We obtain the movies and the users that exist in the pca_edx_residuals:

movies_pca <- rownames(pca_edx_residuals$rotation) %>% as.data.frame()
names(movies_pca) <- "movieId" 
str(movies_pca)

users_pca <- rownames(pca_edx_residuals$x) %>% as.data.frame()
names(users_pca) <- "userId" 
str(users_pca)

# We obtain the movies and users that matches both the prediction_results set and the pca_edx_residuals set:

matching_movies <- semi_join(movies,movies_pca, by= "movieId")
matching_movies <- matching_movies$movieId %>% as.character()
str(matching_movies)

matching_users <- semi_join(users,users_pca, by= "userId")
matching_users <- matching_users$userId %>% as.character()
str(matching_users)

# Now we calculate the residual ratings with the PCs of our choice:

pcs <- 40 # We can use Cross-Validation to tune for the number of pcs, but i'm not doing this since these calculations are very expensive.

# Normally the way to obtain this result would be to multiply the matrix "x" by the traspose of matrix "rotation" like this:
# (r stands for residuals)

residual_prediction <- pca_edx_residuals$x[matching_users,1:pcs] %*% t(pca_edx_residuals$rotation[matching_movies, 1:pcs])

# But since we substracted the center of each column with the prcomp() function, we need to add it back.
# To do so, we traspose our previous matrix, add the center of each column (by default this operation is performed row-wise that's why we traspose
# the matrix) and we traspose back our matrix to our original form (users in rows and movies in columns):

residual_prediction <- t(t(residual_prediction)+pca_edx_residuals$center)
residual_prediction[1:10,1:10]

# Now our final prediction will be:

# a). The prediction of our last model (model 11) + the residual prediction: For each user-movie combination from the validation set that exists
# in the residual prediction matrix.

# b). The prediction of our last model (model 11) alone: For the non-matching combinations (remember we performed PCA only with the best movies
# and users because of the memory space).

# Note that we are not using the validation set to make calculations! We are using it exclusively to extract the combinations of movie and user
# we need to predict.

# So we get our matching movies and users:

matching_pred_movies <- prediction_results %>%
  filter(movieId %in% matching_movies & userId %in% matching_users) %>%
  select(movieId)

matching_pred_movies$movieId <- as.character(matching_pred_movies$movieId) 
matching_pred_movies <- matching_pred_movies$movieId %>% as.vector()
str(matching_pred_movies)

matching_pred_users <- prediction_results %>%
  filter(movieId %in% matching_movies & userId %in% matching_users) %>%
  select(userId)

matching_pred_users$userId <- as.character(matching_pred_users$userId) 
matching_pred_users <- matching_pred_users$userId %>% as.vector()
str(matching_pred_users)

# Then we extract for each combination its corresponding residual prediction:

residuals <- residual_prediction[cbind(matching_pred_users,matching_pred_movies)]
head(residuals)

residuals <- cbind(matching_pred_users,matching_pred_movies,residuals) %>% as.data.frame()

residuals <- residuals %>% mutate_all(~(type.convert(as.numeric(.))))

residuals <- residuals %>% rename(userId = matching_pred_users, movieId = matching_pred_movies)

str(residuals)

# And now we join our residuals to the prediction result set:

prediction_results <- left_join(prediction_results,residuals, by=c("userId","movieId"))

prediction_results <- prediction_results %>%
  mutate(avg_best_effects_regularization_pca = ifelse(is.na(residuals),
                                                      avg_best_effects_regularization,
                                                      avg_best_effects_regularization + residuals))

# Note that some values are out of range so we will replace them for zero or five:

range(prediction_results$avg_best_effects_regularization_pca)

prediction_results <- prediction_results %>%
  mutate(avg_best_effects_regularization_pca = ifelse(avg_best_effects_regularization_pca < 0, 0,
                                  ifelse(avg_best_effects_regularization_pca > 5 , 5,
                                         avg_best_effects_regularization_pca)))

# We calculate the rmse:

rmse_prediction <- RMSE(prediction_results$true_rating,prediction_results$avg_best_effects_regularization_pca)

# We read the rmse_results object we previously saved and add our new result:

rmse_results <- readRDS("rda/rmse_results.rda")
rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

# Result of Prediction:

rmse_results      # We can see that our prediction has an improvement
range(prediction_results$avg_best_effects_regularization_pca)



####. . . VI.IV Model 13 EM + Truncated SVD -----------------------------


# Now we will use a function in R called "eimpute" (from the package "eimpute"), which allow us to "Fit a low-rank matrix approximation to
# a matrix with missing values. The algorithm iterates like EM (Expectation-Maximization): filling the missing values with the current guess,
# and then approximating the complete matrix via truncated SVD (Singular Value Decomposition)".

# REFERENCE
# eimpute: Efficiently impute missing values for a large scale matrix From RDocumentation
# https://www.rdocumentation.org/packages/eimpute/versions/0.1.1/topics/eimpute



####. . . . . . VI.IV.I Crear R Environment ---------------------------------------------


# We clear the R environment:

saveRDS(prediction_results,"rda/prediction_results.rda")
saveRDS(rmse_results,"rda/rmse_results.rda")

rm(list=setdiff(ls(), c("RMSE")))
gc()
memory.size (max=FALSE)

# Now we create our sparse matrix with the best movies and users: 

edx_best_observations <- readRDS("rda/edx_best_observations.rda")

edx_best_observation_sparse <- sparseMatrix(
  as.integer(factor(edx_best_observations$userId)),
  as.integer(factor(edx_best_observations$movieId)),
  x=edx_best_observations$rating)

# We convert the sparse matrix to a dense matrix:

edx_best_observation_dense <- as.matrix(edx_best_observation_sparse)

# Finally, we assign the names of the users to the rows and the names of the movies to the column of the matrix:

colnames(edx_best_observation_dense) <- edx_best_observations %>%
  distinct(movieId) %>%
  arrange(movieId) %>%
  as.matrix()

rownames(edx_best_observation_dense) <- edx_best_observations %>%
  distinct(userId) %>%
  arrange(userId) %>%
  as.matrix()

# This time we replace all zeros with NA:

edx_best_observation_dense[edx_best_observation_dense == 0] <- NA

# Here are the first 10 x 10 results of our matrix:

edx_best_observation_dense[1:10,1:10]



####. . . . . . VI.IV.II eimpute Package -----------------------------------------------------------


# We install the "eimpute" package:

if(!require(eimpute)) install.packages("eimpute", repos = "http://cran.us.r-project.org")
library(eimpute)

edx_best_observation_dense <- readRDS("rda/edx_best_observation_dense.rda")

rm(list=setdiff(ls(), c("edx_best_observation_dense","RMSE")))
gc()
memory.size (max=FALSE)

# We define rank= 40 for approximating the low-rank matrix:

rank <- 40
impute_matrix <- eimpute(edx_best_observation_dense, rank) # this might take a couple of minutes (10-15)
edx_em_svd_matrix <- impute_matrix[["x.imp"]]

# We assign the user and the movie ids to the name of the rows and columns, respectively:

colnames(edx_em_svd_matrix) <- colnames(edx_best_observation_dense)
rownames(edx_em_svd_matrix) <- rownames(edx_best_observation_dense)

# Here are the first 10 x 10 results:

edx_em_svd_matrix[1:10,1:10]



### . . . . . . VI.IV.III Final Prediction -----------------------------------------------------


# First we release some memory space:

rm(list=setdiff(ls(), c("edx_em_svd_matrix","RMSE")))
gc()
memory.size (max=FALSE)

method_number <- 13
method <- "regularization + em + truncated svd"
method_abv <- "regularization_em_svd"
method_desc <- "regularization model plus Expectation-Matimization and truncated SVD"

# Then we read the prediction_results object (this object contains the validation set with the results of the prediction of the different
# models we have used):

prediction_results <- readRDS("rda/prediction_results.rda")
head(prediction_results)

# Now our final prediction will be:

# a). The rating imputeted with the low-rank approximation: For each user-movie combination from the validation set that exists in the low rank
# matrix.

# b). The prediction of (model 11) alone: For the non-matching combinations (remember we performed eimpute only with the best movies
# and users because of the memory space).

# Note that we are not using the validation set to make calculations! We are using it exclusively to extract the combinations of movie and user
# we need to predict.

# We create a data frame with the combinations of movieId and userId in the prediction results set:

movieId <- prediction_results$movieId %>% as.character()
userId <- prediction_results$userId %>% as.character()
combination <- cbind(userId,movieId) %>% as.data.frame()
str(combination)

# We filter only the user-movie combinations that match our low rank matrix:

movies_em_svd_matrix <- colnames(edx_em_svd_matrix) %>% as.data.frame()
users_em_svd_matrix <- rownames(edx_em_svd_matrix) %>% as.data.frame()
combination <- combination %>% filter(movieId %in% movies_em_svd_matrix$. & userId %in% users_em_svd_matrix$.)
str(combination)

# We extract the ratings of the low rank matrix by the resulting combinations:

em_svd_predictions <- edx_em_svd_matrix[combination %>% as.matrix()]
em_svd_predictions <- cbind(combination,em_svd_predictions)
em_svd_predictions <- em_svd_predictions %>%
  mutate(movieId = as.numeric(movieId), userId = as.numeric(userId))
head(em_svd_predictions)

# And now we join our residuals to the prediction result set:

prediction_results <- left_join(prediction_results,em_svd_predictions, by=c("userId","movieId"))

prediction_results <- prediction_results %>%
  mutate(regularization_em_svd = ifelse(is.na(em_svd_predictions), avg_best_effects_regularization,em_svd_predictions))

head(prediction_results)

# Note that some values are out of range so we will replace them for zero or five:

range(prediction_results$regularization_em_svd)

prediction_results <- prediction_results %>%
  mutate(regularization_em_svd = ifelse(regularization_em_svd < 0, 0,
                                        ifelse(regularization_em_svd > 5 , 5,
                                               regularization_em_svd)))

# We calculate the rmse:

rmse_prediction <- RMSE(prediction_results$true_rating,prediction_results$regularization_em_svd)

# We read the rmse_results object we previously saved and add our new result:

rmse_results <- readRDS("rda/rmse_results.rda")
rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

# Result of Prediction:

rmse_results
range(prediction_results$regularization_em_svd)



####. . . VI.V Model 14 Ensemble -----------------------------

method_number <- 14
method <- "ensemble"
method_abv <- "ensemble"
method_desc <- "ensemble of regularization + pca and regularization + em + truncated svd"

prediction_results <- prediction_results %>% mutate(ensemble = (avg_best_effects_regularization_pca + regularization_em_svd) / 2)

# We calculate the rmse:

rmse_prediction <- RMSE(prediction_results$true_rating,prediction_results$ensemble)

# We write the result in rmse_results:

rmse_results <- rbind(rmse_results, data.frame(ID = method_number, Method = method, RMSE = rmse_prediction, Description = method_desc))

# Result of Prediction:

rmse_results
range(prediction_results$ensemble)



#### VII. Conclusions -----------------------------


saveRDS(rmse_results,"rda/rmse_results.rda")
saveRDS(prediction_results,"rda/prediction_results.rda")

# To sum up, we have trained a total of 14 models:

# Model 1 uses a baseline predictor: the average of all ratings. Thus, every prediction is the same no matter the movie or the user.
# This represents a naive approach.

# Models 2 to 6 predicts based on global effects separately. These effects or biases are: movie, user, genre, premiere year and date of
# rating. These models demonstrate to have little improvement over our first model, and this makes sense since we are not taking into account
# the interaction among all these effects together.

# Model 7 to 9 predicts based on a combination of global effects. This resulted in much more accurate models since we take into account the
# interaction among all the global effects.

# Model 10 and 11 uses regularization in global effects to penalyze biases that were estimated with small sample sizes. But still, these
# models leave out an important variation: the similarities between groups of movies and groups of users. Here we used Cross-Validation (CV) to
# find the best penalty term of $\lambda$.

# Model 12 and 13 uses different techniques of matrix factorization that helped us to find "latent patterns" between different groups of
# movies and users, which in the end led us to an important improvement in our prediction. To accomplish this we used Principal Component Analysis
# (PCA) and Expectation-Maximization (EM) with truncated Singular Value Decomposition (SVD). This last method was way faster and has a better
# result. Despite we did not tune the parameters *number of PCs* or *rank* we could have used Cross-Validation to find the best values of both
# parameters.

# Model 14 ensembles the predictions from models 12 and 13 by estimating the average of both models. This final algorithm proved to be the
# best model in terms of RMSE.

#### VIII. Prepare Objects for Rdm File -----------------------------

# Here we will create some objects we will need in our rmd file:

# 01 Extract Prediction Results for Examples

prediction_results_extract <- prediction_results[c(285,1934,933,6164,6903,6737,5569,6203,990,3493),]

saveRDS(prediction_results_extract,"rda/prediction_results_extract.rda")

# 02 Example of Movie bias

edx <- readRDS("data/edx.rda")

mu <- mean(edx$rating)

b_m <- edx %>% 
  group_by(movieId,title) %>% 
  summarize(mean_rating = mean(rating), mu = mu ,b_m = mean(rating - mu)) %>%
  ungroup() %>%
  slice_head(n = 10)

saveRDS(b_m,"rda/b_m.rda")

# 03 Mean RMSE of Lambda

tuning_results_lambda <- readRDS("rda/tuning_results_lambda.rda")

tuning_results_lambda <- tuning_results_lambda %>%
  mutate(mean_rmse = rowMeans(select(., rmses1:rmses10))) 

mean_rmse_lambdas <- tuning_results_lambda %>%
  select(lambdas,mean_rmse)

saveRDS(mean_rmse_lambdas,"rda/mean_rmse_lambdas.rda")

# 04 dimensions of pca_edx_residuals

pca_edx_residuals <- readRDS("rda/pca_edx_residuals.rda")

dim_pca_x <- str_c(dim(pca_edx_residuals$x)[1]," x ",dim(pca_edx_residuals$x)[2],sep="")
dim_pca_rotation <- str_c(dim(pca_edx_residuals$rotation)[1]," x ",dim(pca_edx_residuals$rotation)[2],sep="")

saveRDS(dim_pca_x,"rda/dim_pca_x.rda")
saveRDS(dim_pca_rotation,"rda/dim_pca_rotation.rda")

# 05 eimpute extract

edx_em_svd_matrix <- readRDS("rda/edx_em_svd_matrix.rda")

edx_em_svd_matrix_extract <- edx_em_svd_matrix[1:10,1:10]

saveRDS(edx_em_svd_matrix_extract,"rda/edx_em_svd_matrix_extract.rda")
