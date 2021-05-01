# Introduction ------------------------------------------------------------

# You will use the following code to generate your datasets. Develop your algorithm using the edx set. For a final test of your final algorithm,
# predict movie ratings in the validation set (the final hold-out test set) as if they were unknown. RMSE will be used to evaluate how close your
# predictions are to the true values in the validation set (the final hold-out test set).
# 
# Important: The validation data (the final hold-out test set) should NOT be used for training, developing, or selecting your algorithm and it
# should ONLY be used for evaluating the RMSE of your final algorithm. The final hold-out test set should only be used at the end of your project
# with your final model. It may not be used to test the RMSE of multiple models during model development. You should split the edx data into separate
# training and test sets to design and test your algorithm.
# 
# Also remember that by accessing this site, you are agreeing to the terms of the edX Honor Code. This means you are expected to submit your own
# work and can be removed from the course for substituting another student's work as your own.

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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Quiz: MovieLens Dataset -------------------------------------------------

# All of the questions in this quiz involve working with the datasets you've created using the code we provided.

# Q1
# How many rows and columns are there in the edx dataset?

str(edx)
 
# Number of rows:
# 9000055

# Number of columns:
# 6

# Q2
# How many zeros were given as ratings in the edx dataset?
edx %>% group_by(rating) %>% summarize(n())
# 0

# How many threes were given as ratings in the edx dataset?
# 2121240

# Q3
# How many different movies are in the edx dataset?
edx %>% distinct(movieId) %>% summarize(n())
# 10677

# Q4
# How many different users are in the edx dataset?
edx %>% distinct(userId) %>% summarize(n())
# 69878

# Q5
# How many movie ratings are in each of the following genres in the edx dataset?

# Drama:
edx %>% filter(genres %like% "Drama") %>% summarize(n())
# 3910127

# Comedy:
edx %>% filter(genres %like% "Comedy") %>% summarize(n())
# 3540930

# Thriller:
edx %>% filter(genres %like% "Thriller") %>% summarize(n())
# 2325899

# Romance:
edx %>% filter(genres %like% "Romance") %>% summarize(n())
# 1712100

# Q6
# Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% summarize(ratings = n()) %>% arrange(desc(ratings))

# Forrest Gump
# Jurassic Park
# Pulp Fiction <--- 31362
# The Shawshank Redemption
# Speed 2: Cruise Control

# Q7
# What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(ratings = n()) %>% arrange(desc(ratings))

# 4, 3, 5, 3.5, 2 <--- TRUE
# 3.5, 4.5, 1, 3, 5
# 0.5, 5, 1, 4.5, 1.5
# 5, 3, 1, 4.5, 3.5
# 2, 3.5, 5, 3, 4

# Q8
# True or False: In general, half star ratings are less common than whole star ratings
# (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).

# True <--- TRUE
# False

saveRDS(edx,"rda/edx.rda")
saveRDS(validation,"rda/validation.rda")



