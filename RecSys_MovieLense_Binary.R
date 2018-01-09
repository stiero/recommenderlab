rm(list=ls())

library(recommenderlab)
library(ggplot2)

data("MovieLense")

similarity_users <- similarity(MovieLense[1:4,], method="jaccard", which="users")

similarity_items <- similarity(MovieLense[,1:4], method="jaccard", which="items")

#Show possible models
possible_models <- recommenderRegistry$get_entries(dataType="realRatingMatrix")

slotNames(MovieLense)
class(MovieLense@data)

#Contains ratings from MovieLense matrix as a flat vector
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

#table returns counts of unique cases
table_ratings <- table(vector_ratings)
table_ratings

#Select only non-zero ratings
vector_ratings <- vector_ratings[vector_ratings != 0]
qplot(vector_ratings, binwidth=0.5)

#views_per_movie is a named int
views_per_movie <- colCounts(MovieLense)

#This only works because views_per_movie is a named int
table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie)

table_views <- table_views[order(table_views$views, decreasing=TRUE),]

ggplot(table_views[1:6,], aes(x=movie, y=views)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle("Number of views of the top movies")

#Average ratings per movie
average_ratings <- colMeans(MovieLense)
qplot(average_ratings) + stat_bin(binwidth = 0.1)

#Average ratings of those movies watched by more than 100 users
average_ratings_relevant <- average_ratings[views_per_movie > 100]
qplot(average_ratings_relevant, binwidth=0.1)

image(MovieLense)
image(MovieLense[1:10, 1:15])

min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)

image(MovieLense[rowCounts(MovieLense) > min_n_movies,
                 colCounts(MovieLense) > min_n_users])

########### Minimum criteria #################
#Subset MovieLense to only include movies watched by more than 100 users...
# and users who have watched at least 50 movies
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]

ratings_movies

#Determine cutoff for top 2% of movies and users
min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)

image(ratings_movies[rowCounts(ratings_movies) > min_movies,
                     colCounts(ratings_movies) > min_users])

#Calculate average rating a user has given
average_ratings_per_user <- rowMeans(ratings_movies)
qplot(average_ratings_per_user, binwidth = 0.1)

#Normalise rating matrix
ratings_movies_norm <- normalize(ratings_movies)

image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,
                          colCounts(ratings_movies_norm) > min_users])

######### Binarize rating matrix ##################
#Rating intensity information is lost

#If rating is atleast 1, the binary matrix records it as 1, else as 0
ratings_movies_binarized <- binarize(ratings_movies, minRating = 1)

min_movies_binary <- quantile(rowCounts(ratings_movies_binarized), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies_binarized), 0.95)

image(ratings_movies_binarized[rowCounts(ratings_movies) > min_movies_binary,
                               colCounts(ratings_movies) > min_users_binary])


#If the rating is atleast 3, the binary matrix records it as 1, else 0
ratings_movies_good <- binarize(ratings_movies, minRating = 3)

image(ratings_movies_good[rowCounts(ratings_movies) > min_movies_binary,
                          colCounts(ratings_movies) > min_users_binary])

image(ratings_movies_good[,colCounts(ratings_movies) > min_movies_binary])

#Splitting data into train and test sets

#Take a sample of the vector (TRUE, FALSE) with replacement.
#The sample size is equal to the number of rows
#The sample should be 80% TRUE and 20% FALSE
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE,
                      prob = c(0.8,0.2))

recc_data_train <- ratings_movies_binarized[which_train,]
recc_data_test <- ratings_movies_binarized[!which_train,]

#Our model stores 30 most similiar movies for a particular movie
recc_model <- Recommender(data = recc_data_train, method = "UBCF",
                          parameter = list(method="jaccard"))

recc_model

############## Exploring the model #####################
#getModel is used to examine the built model in detail
model_details <- getModel(recc_model)

model_details$description

model_details$k

#Sum of similarities of each movie
row_sums <- rowSums(model_details$sim > 0)

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 0.5)

n_items_top <- 20

image(model_details$sim[1:n_items_top, 1:n_items_top])

############ Applying on test set #################
#No of items to recommend
n_recommended <- 6

recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)

#Get the names of movies recommended to each user in test data
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})

number_of_items <- factor(table(recc_matrix))

qplot(number_of_items)

#Most popular recommended movies
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)

number_of_items_sorted_top <- number_of_items_sorted[1:4]

top_table <- data.frame("movie" = names(number_of_items_sorted_top),
                        "times recommended" = number_of_items_sorted_top)

recc_matrix <- as.data.frame(recc_matrix)


