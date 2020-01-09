

###########################################################################
# WARNING: If you want to run everything it might take up to half an hour #
###########################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
library(tinytex)


# This is for anyone who cannot knit the PDF

# install.packages('tinytex')
# tinytex::install_tinytex()
# options(digits = 5)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

# Add the variable timestamp in month

edx <- edx %>% mutate( date_m = round_date(as_datetime(timestamp),"month"))
validation <- validation %>% mutate(date_m = round_date(as_datetime(timestamp),"month"))

######################
## Assignment start ##
######################

## ---Visualisation and data Exploring --- ##

# Explore dimention of in-sample set and out-of-sample set

dim(edx)
dim(validation)

# Histogram of rating

head(edx)
edx %>% ggplot(aes(rating)) + geom_histogram() + ggtitle("Histogram of rating")

# Describtive Statitic

edx %>% summarise( avg = mean(rating), sd = sd(rating))

# how many group in independent variables

data.frame( variable_name = c('movieId','title','userId','genres','timestamp_month'),
            n_catagory = c(nlevels(as.factor(edx$movieId)),
                           nlevels(as.factor(edx$title)),
                           nlevels(as.factor(edx$userId)),
                           nlevels(as.factor(edx$genres)),
                           nlevels(as.factor(edx$date_m))))

# See the number in the factors in tilte variable

head(edx %>% group_by(title) %>% summarise(n = n()) %>% arrange(n))

# Histogram of mean rating of each movie

edx %>% group_by(movieId) %>% 
  summarise( avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + geom_histogram() + ggtitle("Histogram of mean rating of each movie")

# Histogram of mean rating of each user

edx %>% group_by(userId) %>% 
  summarise( avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + geom_histogram() + ggtitle("Histogram of mean rating of each user")

# Histogram of mean rating of each genres

edx %>% group_by(genres) %>% 
  summarise( avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + geom_histogram()+ ggtitle("Histogram of mean rating of each genres")

# Average Rating vs. Time

edx %>% mutate( date_m = round_date(as_datetime(timestamp),"month")) %>%
  group_by(date_m) %>% summarise( month_avg = mean(rating))%>%
  ggplot(aes(date_m,month_avg)) + geom_line() + ggtitle(" Average Rating vs. Time")

# See least sample in group for userId

head(edx %>% group_by(userId) %>% 
       summarise( n = n()) %>% arrange(n))

# See least sample in group for movie

head(edx %>% group_by(title) %>% 
       summarise( n = n()) %>% arrange(n))

# See least sample in group for genres

head(edx %>% group_by(genres) %>% 
       summarise( n = n()) %>% arrange(n))

# See least sample in group for months

head(edx %>% group_by(date_m) %>% 
       summarise( n = n()) %>% arrange(n))

## --- Function for Calculation --- ##

# extract the index for k-fold cross-validation

ind_cv <- function(k){
  ind <- round(seq(1,nrow(edx),nrow(edx)/k),digits = 0)
  return(ind)
}

# Calculate mean of rating

mu_cal <- function(rating){
  mean(rating)
}

# average effect on rating of each movie

movie_reg <- function(ts,mu_l,lamb){
  ts %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu_l)/(n()+lamb))
}

# average effect on rating of each user

user_reg <- function(ts,mu_l,mov_avgs,lamb){
  ts %>% 
    left_join(mov_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_l - b_i)/(n()+lamb))
}

# average effect on rating of each genre

genres_reg <- function(ts,mu_l,mov_avgs,use_avgs,lamb){
  ts %>% 
    left_join(mov_avgs, by='movieId') %>%
    left_join(use_avgs, by= 'userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu_l - b_i - b_u)/(n()+lamb))
}

# average effect on rating of each month

month_reg <- function(ts,mu_l,mov_avgs,use_avgs,gen_avgs,lamb){
  ts %>%
    left_join(mov_avgs, by='movieId') %>%
    left_join(use_avgs, by='userId') %>%
    left_join(gen_avgs, by='genres') %>%
    group_by(date_m) %>%
    summarize(b_d  = sum(rating - mu_l - b_i - b_u - b_g)/(n()+lamb))
}

# RMSE calculation

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# The code below is for preparing the dataset for splitting and setting the number of fold
set.seed(2019, sample.kind="Rounding")
random_index <- sample(seq(1:nrow(edx)),nrow(edx), replace = FALSE)
k <- 5
ind <- ind_cv(k)

# The single constant model (model 1)

RMSE_model_1 <- sapply(ind,function(n){
  train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
  test_set <- temp_set %>% # check that train set and test set will have same variable
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId") %>%
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  mu <- mu_cal(train_set$rating) # calculate average rating
  predicted_ratings <- test_set %>%  # generate Y_hat
    mutate(pred = mu) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating) # calculate RMSE
})
mean(RMSE_model_1) # mean from cross-validation
RMSE_result <- data_frame(method = "Plain average", RMSE = mean(RMSE_model_1)) # add to table
RMSE_result
rm(RMSE_model_1) # remove result from model 1

# The movie model (model 2)

RMSE_model_2 <- sapply(ind,function(n){
  train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
  test_set <- temp_set %>% # check that train set and test set will have same variable
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId") %>%
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  mu <- mu_cal(train_set$rating) # calculate average rating
  movie_avgs <- movie_reg(train_set,mu, lamb = 0) # calculate b_i
  predicted_ratings <- test_set %>%   # generate Y_hat
    left_join(movie_avgs, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating) # calculate RMSE
})
mean(RMSE_model_2) # mean from cross-validation
RMSE_result <- bind_rows(RMSE_result,
                         data_frame(method = "Plus Movie Avg", RMSE = mean(RMSE_model_2))) # add to table
RMSE_result
rm(RMSE_model_2) # remove result from model 2

# The movie and user model (model 3)

RMSE_model_3 <- sapply(ind,function(n){
  train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
  test_set <- temp_set %>% # check that train set and test set will have same variable
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId") %>%
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  mu <- mu_cal(train_set$rating) # calculate average rating
  movie_avgs <- movie_reg(train_set,mu, lamb = 0) # calculate b_i
  user_avgs <- user_reg(train_set,mu,movie_avgs, lamb = 0) # calculate b_u
  predicted_ratings <- test_set %>% # generate Y_hat
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating) # calculate RMSE
})
mean(RMSE_model_3) # mean from cross-validation
RMSE_result <- bind_rows(RMSE_result,
                         data_frame(method = "Plus User Avg", RMSE = mean(RMSE_model_3))) # add to table
RMSE_result
rm(RMSE_model_3) # remove result from model 3

# The movie, user and genres model (model 4)

RMSE_model_4 <- sapply(ind,function(n){
  train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
  test_set <- temp_set %>% # check that train set and test set will have same variable
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId") %>%
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  mu <- mu_cal(train_set$rating) # calculate average rating
  movie_avgs <- movie_reg(train_set,mu, lamb = 0) # calculate b_i
  user_avgs <- user_reg(train_set,mu,movie_avgs, lamb = 0) # calculate b_u
  genres_avgs <- genres_reg(train_set,mu,movie_avgs,user_avgs, lamb = 0) # calculate b_g
  predicted_ratings <- test_set %>% # generate Y_hat
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genres_avgs, by='genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating) # calculate RMSE
})
mean(RMSE_model_4) # mean from cross-validation
RMSE_result <- bind_rows(RMSE_result,
                         data_frame(method = "Plus Genres Avg", RMSE = mean(RMSE_model_4))) # add to table
RMSE_result
rm(RMSE_model_4) # remove result from model 4

# The All four variables model (model 5)

RMSE_model_5 <- sapply(ind,function(n){
  train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
  test_set <- temp_set %>% # check that train set and test set will have same variable
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId") %>%
    mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
  mu <- mu_cal(train_set$rating) # calculate average rating
  movie_avgs <- movie_reg(train_set,mu, lamb = 0) # calculate b_i
  user_avgs <- user_reg(train_set,mu,movie_avgs, lamb = 0) # calculate b_u
  genres_avgs <- genres_reg(train_set,mu,movie_avgs,user_avgs, lamb = 0) # calculate b_g
  month_avgs <- month_reg(train_set,mu,movie_avgs,user_avgs,genres_avgs, lamb = 0) # calculate b_d
  predicted_ratings <- test_set %>%  # generat Y_hat
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genres_avgs, by='genres') %>%
    left_join( month_avgs, by = 'date_m') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating) # calculate RMSE
})
mean(RMSE_model_5) # mean from cross-validation
RMSE_result <- bind_rows(RMSE_result,
                         data_frame(method = "Plus Month Avg", RMSE = mean(RMSE_model_5))) # add to table
RMSE_result
rm(RMSE_model_5) # remove result from model 5

lambda_vector <- seq(2,7)
RMSE_lambda <- c()
for (lambda in lambda_vector) {
  RMSE_cv <- c()
  for (n in ind) {
    train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
      mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
    temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
    test_set <- temp_set %>% # check that train set and test set will have same variable
      semi_join(train_set, by = "movieId") %>%
      semi_join(train_set, by = "userId") %>%
      mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
    mu <- mu_cal(train_set$rating) # calculate average rating
    movie_avgs <- movie_reg(train_set,mu,lambda) # calculate b_i with lambda
    user_avgs <- user_reg(train_set,mu,movie_avgs,lambda) # calculate b_u with lambda
    genres_avgs <- genres_reg(train_set,mu,movie_avgs,user_avgs,lambda) # calculate b_g with lambda
    month_avgs <- month_reg(train_set,mu,movie_avgs,user_avgs,genres_avgs, lamb = 0) # calculate b_d \
    predicted_ratings <- test_set %>%  # generat Y_hat
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genres_avgs, by='genres') %>%
      left_join(month_avgs, by='date_m') %>%
      mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
      .$pred
    RMSE_cv <- append(RMSE_cv,RMSE(predicted_ratings, test_set$rating)) # calculate RMSE 
  }
  RMSE_lambda <- append(RMSE_lambda,mean(RMSE_cv))  # mean from cross-validation
  rm(mu,user_avgs,movie_avgs,genres_avgs,month_avgs, # remove object
     train_set,test_set,temp_set,predicted_ratings, RMSE_cv)
}
plot(lambda_vector,RMSE_lambda) # plot to find best lambda
best_lambda <- lambda_vector[which.min(RMSE_lambda)] # store best lambda
min(RMSE_lambda) # print best RMSE
RMSE_result <- bind_rows(RMSE_result,
                         data_frame(method = "Regularization", RMSE = min(RMSE_lambda))) # add to table
message("Best lambda: ", best_lambda)
RMSE_result

# new setting before final test on in-sample set

k <- 10
ind <- ind_cv(k)
lambda_vector <- best_lambda
RMSE_lambda <- c()

# final test for in-sample set

for (lambda in lambda_vector) {
  RMSE_cv <- c()
  for (n in ind) {
    train_set <- edx[-(random_index[n:(n+nrow(edx)/k-2)]),] %>%  # partition training set
      mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
    temp_set <- edx[random_index[(n:(n+nrow(edx)/k-2))],]  # partition temp set
    test_set <- temp_set %>% # check that train set and test set will have same variable
      semi_join(train_set, by = "movieId") %>%
      semi_join(train_set, by = "userId") %>%
      mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
    mu <- mu_cal(train_set$rating) # calculate average rating
    movie_avgs <- movie_reg(train_set,mu,lambda) # calculate b_i with lambda
    user_avgs <- user_reg(train_set,mu,movie_avgs,lambda) # calculate b_u with lambda
    genres_avgs <- genres_reg(train_set,mu,movie_avgs,user_avgs,lambda) # calculate b_g with lambda
    month_avgs <- month_reg(train_set,mu,movie_avgs,user_avgs,genres_avgs, lamb = 0) # calculate b_d \
    predicted_ratings <- test_set %>%  # generat Y_hat
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genres_avgs, by='genres') %>%
      left_join(month_avgs, by='date_m') %>%
      mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
      .$pred
    RMSE_cv <- append(RMSE_cv,RMSE(predicted_ratings, test_set$rating)) # calculate RMSE 
  }
  RMSE_lambda <- append(RMSE_lambda,mean(RMSE_cv))  # mean from cross-validation
  rm(mu,user_avgs,movie_avgs,genres_avgs,month_avgs, # remove object
     train_set,test_set,temp_set,predicted_ratings, RMSE_cv)
}
RMSE_lambda
RMSE_result <- bind_rows(RMSE_result,
                         data_frame(method = "Regularization with best lambda", RMSE = max(RMSE_lambda))) # add to table
RMSE_result

# out-of-sample prediction

train_set <- edx %>%
  mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
test_set <- validation %>%
  mutate( date_m = round_date(as_datetime(timestamp),"month")) # mutate timestamp to months
mu <- mu_cal(train_set$rating) # calculate average rating
movie_avgs <- movie_reg(train_set,mu,lambda) # calculate b_i with lambda
user_avgs <- user_reg(train_set,mu,movie_avgs,lambda) # calculate b_u with lambda
genres_avgs <- genres_reg(train_set,mu,movie_avgs,user_avgs,lambda) # calculate b_g with lambda
month_avgs <- month_reg(train_set,mu,movie_avgs,user_avgs,genres_avgs, lamb = 0) # calculate b_d
predicted_ratings <- test_set %>%   # generat Y_hat
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  left_join(month_avgs, by='date_m') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  .$pred
final_RMSE <- RMSE(predicted_ratings, test_set$rating) # RMSE result
final_RMSE

