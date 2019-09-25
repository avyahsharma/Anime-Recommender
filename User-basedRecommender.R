# Packages
library("recommenderlab")
library("data.table")
library("reshape2")
library("taRifx")
library("proxy")

# Set the working directory to properly access file locally
getwd()
setwd("C:/xyz")

# Access files
file_in <- "anime.csv"
rating <- "rating.csv"

# Read csv files
table_in <- read.csv(file_in, header=FALSE)
rating <- read.csv(rating, header=FALSE)

# Create ratings matrix. Rows = userId, Columns = movieId
rating = rating[-1,]
names(rating) <- c("user_id", "anime_id", "rating")
ratingmat <- dcast.data.table(data.table(rating), user_id~anime_id, value.var="rating")
ratingmat <- as.matrix(ratingmat[, -1])

# Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

# Normalize the matrix
ratingmat_norm <- normalize(ratingmat)

# Determine number of recommendations
n <- readline("How many recommendations?")

# Create Recommender Model
recommender_model <- Recommender(ratingmat_norm, method="UBCF", param=list(method="Cosine", nn=30))
recom_mat <- predict(recommender_model, ratingmat[1], n=n)
recom_list <- as(recom_mat, "list")

# List recommendations
recom_result <- matrix(0, n)
for (i in c(1:n)) {
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]), 2]
  
}