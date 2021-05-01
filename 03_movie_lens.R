library(gridExtra)
library(tidyverse)

# Data Exploration --------------------------------------------------------
edx <- read_rds("rda/edx.rda")
validation <- read_rds("rda/validation.rda")

memory.size (max=TRUE)
memory.size (max=FALSE)
gc()
memory.size (max=FALSE)

users <- edx %>% group_by(userId) %>% summarize(cases=n(),avg_rating = mean(rating)) %>% arrange(desc(cases))
users %>% ggplot(aes(cases,avg_rating)) + geom_point()
users %>% ggplot(aes(avg_rating)) + geom_histogram()

# RMSE Function -----------------------------------------------------------
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

prediction_results <- validation %>% mutate(true = validation$rating)

# Average -----------------------------------------------------------------
method_number <- 1
method <- "average"
method_abv <- "avg"

mu <- mean(edx$rating)
prediction <- rep(mu,nrow(validation)) %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- data.frame(num = method_number,method = method,RMSE = rmse_prediction)
prediction_results <- cbind(prediction_results,prediction)

# Average + Movie Effect --------------------------------------------------
method_number <- 2
method <- "average + movie effect"
method_abv <- "avg_movie_effect"

mu <- mean(edx$rating)
b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_i, by='movieId') %>%
  .$b_i %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + User Effect ---------------------------------------------------
method_number <- 3
method <- "average + user effect"
method_abv <- "avg_user_effect"

mu <- mean(edx$rating)

b_u <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_u, by='userId') %>%
  .$b_u %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + Genre ---------------------------------------------------------
method_number <- 4
method <- "average + genre effect"
method_abv <- "avg_genre_effect"

mu <- mean(edx$rating)

b_g <- edx %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu))

prediction <- mu + validation %>% 
  left_join(b_g, by='genres') %>%
  .$b_g %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + Movie Effect + User Effect ------------------------------------
method_number <- 5
method <- "average + movie + user effect"
method_abv <- "avg_movie_user_effect"

mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

b_u <- edx %>%
  left_join(b_i,by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

prediction <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + Movie Effect + User Effect + Genre effect --------------------------------------------------
method_number <- 6
method <- "average + movie + user + genre effect"
method_abv <- "avg_movie_user_genre_effect"

mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

b_u <- edx %>%
  left_join(b_i,by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

b_g <- edx %>%
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))

prediction <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + Regularized Movie Effect ------------------------------------
method_number <- 7
method <- "average + reg movie effect"
method_abv <- "avg_reg_movie_effect"

mu <- mean(edx$rating)

# 1. Tuning lambda for movie effect (may take a few minutes)

k_folds_index <- readRDS("rda/k_folds_index.rda") #dataframe with indexes used for 10 folds
# k_folds_index <- createFolds(edx$movieId,k=10,list=TRUE,returnTrain = TRUE) #Code used for creatin 10 folds to perform cv

lambdas <- seq(0,10,.25)

# Algorithm used for tuning:
n <- 1 #Here is an example of the 1st fold (k_1)
train_fold <- edx[k_folds_index[[n]],] #This is the train set used for cross validation where n indicates the number of the fold.
validation_fold <- edx[-k_folds_index[[n]],] #This is the validation set used for the cross validation

rmses <- sapply(lambdas,function(lambda){
  b_i <- train_fold %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  prediction <- validation_fold %>%
    left_join(b_i, by="movieId") %>%
    mutate(pred = mu + b_i)
  
  index_preserve <- which(!is.na(prediction$pred))
  p <- prediction$pred[index_preserve]
  v <- validation_fold$rating[index_preserve]
  
  return(RMSE(p,v))
})

# Tuning Results:
tuning_lambda_u_model <- read_rds("rda/tuning_lambda_u_model.rda") #Here are the results of the tuning
lambdas[which.min(tuning_lambda_u_model$`Mean RMSE`)] #lambda value that minimizes the Mean of the ten RMSE's

#2. Predicition with Best Lambda
best_lambda <- 2.25

b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+best_lambda))

prediction <- validation %>%
  left_join(b_i, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + Regularized Movie & User Effect ------------------------------------
method_number <- 8
method <- "average + reg movie and user effect"
method_abv <- "avg_reg_movie_user_effect"

mu <- mean(edx$rating)

# 1. Tuning lambda for movie effect (may take a few minutes)

k_folds_index <- readRDS("rda/k_folds_index.rda") #dataframe with indexes used for 10 folds
# k_folds_index <- createFolds(edx$movieId,k=10,list=TRUE,returnTrain = TRUE) #Code used for creatin 10 folds to perform cv

lambdas <- seq(0,10,.25)

# Algorithm used for tuning:
n <- 1 #Here is an example of the 1st fold (k_1)
train_fold <- edx[k_folds_index[[n]],] #This is the train set used for cross validation where n indicates the number of the fold.
validation_fold <- edx[-k_folds_index[[n]],] #This is the validation set used for the cross validation

rmses <- sapply(lambdas,function(lambda){
  b_i <- train_fold %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train_fold %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))
  
  prediction <- validation_fold %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    mutate(pred = mu + b_i + b_u)
  
  index_preserve <- which(!is.na(prediction$pred))
  p <- prediction$pred[index_preserve]
  v <- validation_fold$rating[index_preserve]
  
  return(RMSE(p,v))
})

# Tuning Results:
tuning_lambda_ui_model <- read_rds("rda/tuning_lambda_ui_model.rda") #Here are the results of the tuning
lambdas[which.min(tuning_lambda_ui_model$`Mean RMSE`)] #lambda value that minimizes the Mean of the ten RMSE's

#2. Predicition with Best Lambda
best_lambda <- 5

b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+best_lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i)/(n()+best_lambda))

prediction <- validation %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# Average + Regularized Movie, User & Genre Effect ------------------------------------
method_number <- 9
method <- "average + reg movie, user & genre effect"
method_abv <- "avg_reg_movie_user_genre_effect"

mu <- mean(edx$rating)

# 1. Tuning lambda for movie effect (may take a few minutes)

k_folds_index <- readRDS("rda/k_folds_index.rda") #dataframe with indexes used for 10 folds
# k_folds_index <- createFolds(edx$movieId,k=10,list=TRUE,returnTrain = TRUE) #Code used for creatin 10 folds to perform cv

lambdas <- seq(0,10,.25)

# Algorithm used for tuning:
n <- 10 #Here is an example of the 1st fold (k_1)
train_fold <- edx[k_folds_index[[n]],] #This is the train set used for cross validation where n indicates the number of the fold.
validation_fold <- edx[-k_folds_index[[n]],] #This is the validation set used for the cross validation

rmses <- sapply(lambdas,function(lambda){
  b_i <- train_fold %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train_fold %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))
  
  b_g <- train_fold %>%
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId") %>%
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda))
  
  prediction <- validation_fold %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    mutate(pred = mu + b_i + b_u + b_g)
  
  index_preserve <- which(!is.na(prediction$pred))
  p <- prediction$pred[index_preserve]
  v <- validation_fold$rating[index_preserve]
  
  return(RMSE(p,v))
})

# Tuning Results:
tuning_lambda_uig_model <- read_rds("rda/tuning_lambda_uig_model.rda") #Here are the results of the tuning
lambdas[which.min(tuning_lambda_uig_model$`Mean RMSE`)] #lambda value that minimizes the Mean of the ten RMSE's

#2. Predicition with Best Lambda
best_lambda <- 4.75

b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+best_lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i)/(n()+best_lambda))

b_g <- edx %>%
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+best_lambda))

prediction <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred %>% as.data.frame()
colnames(prediction) <- method_abv

rmse_prediction <- RMSE(validation$rating,prediction[[1]])
rmse_results <- rbind(rmse_results, data.frame(num = method_number,method = method,RMSE = rmse_prediction))
prediction_results <- cbind(prediction_results,prediction)

# List of Genres ----------------------------------------------------------
genres <- edx %>% distinct(genres)
count_genres <- max(genres %>% mutate(pipes = str_count(genres,pattern="\\|")) %>% .$pipes)+1
col_genres <- c(1:count_genres) %>% as.character()
genres <- genres %>% separate(col=genres,into=col_genres,sep="\\|")
genres_list <- gather(genres,key="key",value="value",c(1:max(col_genres))) %>% distinct(value) %>% drop_na()

edx_movie_genres <- edx %>% group_by(userId,genres,rating) %>% summarize(n())
#adecuar código, porque se agregaron columnas:
edx_movie_genres <- edx_movie_genres %>% separate(col=genres,into=col_genres,sep="\\|")
edx_movie_genres_2 <- gather(edx_movie_genres,key="key",value="genre",c(2:(max(col_genres %>% as.numeric())+1))) %>% distinct(movieId,genre) %>% drop_na()

# Ejemplos Exploración ----------------------------------------------------
movie_effect <- edx %>% 
  group_by(movieId) %>% 
  summarize(avg_movie = mean(rating), mu=mu, b_i = mean(rating - mu))

writexl::write_xlsx(movie_effect,"exports/movie_effect.xlsx")

user_effect <- edx %>%
  left_join(movie_effect,by="movieId") %>%
  group_by(userId) %>% 
  summarize(avg_user = mean(rating), mu = mean(mu), b_i = mean(b_i), b_u = mean(rating - mu - b_i))

genre_effect <- edx %>%
  left_join(movie_effect,by="movieId") %>%
  left_join(user_effect,by="userId") %>%
  group_by(genres) %>% 
  summarize(avg_genre = mean(rating), mu = mean(mu), b_i = mean(b_i), b_u = mean(b_u), b_g = mean(rating - mu - b_i - b_u))



writexl::write_xlsx(movie_user_effect,"exports/movie_user_effect.xlsx")

writexl::write_xlsx(edx %>% filter(userId <= 10),"exports/movies_10_users.xlsx")

movies_muestra <- c(122,	185,	292,	316,	329,	355,	356,	362,	364,	370,	377,	420,	466,	520,	539,	588,	589,	594,	616)

writexl::write_xlsx(edx %>% filter(movieId %in% movies_muestra),"exports/movies_user_1.xlsx")

usuarios_muestra <- edx %>% group_by(userId) %>% summarize(cases=n()) %>% arrange(desc(cases)) %>% slice(1:2,100:101,200:201)

writexl::write_xlsx(edx %>% filter(userId %in% usuarios_muestra$userId),"exports/movies_6_user_genre_anylisis.xlsx")
writexl::write_xlsx(validation %>% filter(userId %in% usuarios_muestra$userId),"exports/movies_6_user_genre_test_set.xlsx")
writexl::write_xlsx(prediction_results %>% filter(userId %in% usuarios_muestra$userId),"exports/movies_6_user_genre_prediction_results.xlsx")
writexl::write_xlsx(prediction_movie_user_effect %>% filter (userId %in% usuarios_muestra$userId),"exports/prediction_movie_user_effect.xlsx")
writexl::write_xlsx(prediction_movie_user_effect %>% filter (userId <= 10),"exports/prediction_movie_user_effect_10_users.xlsx")
writexl::write_xlsx(genres_list,"exports/genres_list.xlsx")

prediction_results %>% filter(movieId == 5502)
View(movie_effect %>% filter(movieId == 5502))
