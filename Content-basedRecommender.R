# Packages
library("data.table")
library("proxy")
library("reshape2")
library("taRifx")

# Set the working directory 
getwd()
setwd("C:/xyz")

# Access files
file_in <- "anime.csv"
rating <- "rating.csv"

# Read csv files
table_in <- read.csv(file_in, header=FALSE)
rating <- read.csv(rating, header=FALSE)

# Create data table of anime with name, genres, type, episode, rating, and members
table <- data.table(table_in)
setnames(table, c("anime_id", "name", "genres", "type", "episodes", "rating", "members"))
table = table[-1,]

# Establishes genres for genre matrix
genres <- as.data.frame(table$genres, stringsAsFactors=FALSE)
genres <- as.data.frame(tstrsplit(genres[,1], '[,]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres) <- c(1:ncol(genres))

genre_list <- c("Action", "Adventure", "Cars", "Comedy", "Dementia", "Demons", "Drama", "Ecchi", "Fantasy",
                "Game", "Harem", "Historical", "Horror", "Josei", "Kids", "Magic", "Martial Arts", "Mecha",
                "Military", "Music", "Mystery", "Parody", "Police", "Psychological", "Romance", "Samurai",
                "School", "Sci-Fi", "Seinen", "Shoujo", "Shoujo Ai", "Shounen", "Shounen Ai", "Slice of Life",
                "Space", "Sports", "Super Power", "Supernatural", "Thriller", "Vampire", "Yaoi", "Yuri")

genre_matrix <- matrix(0, nrow(genres)+1, length(genre_list))
genre_matrix[1,] <- genre_list
colnames(genre_matrix) <- genre_list

for (r in 1:nrow(genres)) {
  for (c in 1:ncol(genres)) {
    genmat_col = which(genre_matrix[1,] == genres[r, c])
    genre_matrix[r+1, genmat_col] <- 1
    
  }
}

genre_matrix <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE)
for (c in 1:ncol(genre_matrix)) {
  genre_matrix[, c] <- as.integer(genre_matrix[, c])
  
}

# Create a binarized matrix of 1 or 0 to relate the data
binaryratings <- rating
binaryratings = binaryratings[-1,]
names(binaryratings) <- c("user_id", "anime_id", "rating")
binaryratings[] <- lapply(binaryratings, function(x) as.numeric(as.character(x)))
binaryratings[, 3] <- ifelse(binaryratings[, 3] >= 7, 1, -1)
binaryratings <- dcast.data.table(data.table(binaryratings), anime_id~user_id, value.var="rating")

DT.for.set.sqln  <- function(x) { for (j in seq_len(ncol(x)))
  set(x,which(is.na(x[[j]])),j,0) }
DT.for.set.sqln(binaryratings)

# Remove unrated anime
anime_ids <- length(unique(table$anime_id))
ratings <- length(unique(rating$anime_id))
table <- table[-which((anime_ids %in% ratings) == FALSE),]
rownames(table) <- NULL

genre_matrix <- genre_matrix[-which((anime_ids %in% ratings) == FALSE),]
rownames(genre_matrix) <- NULL

# Calculate Dot Product
result = matrix(0, ncol(genre_matrix), ncol(binaryratings))
binaryratings <- data.frame(binaryratings)
for (c in 1:ncol(binaryratings)){
  for (i in 1:ncol(genre_matrix)){
    result[i, c] <- sum((genre_matrix[, i]) * (binaryratings[, c]))
  }
}

# Populate matrix with values
for (i in 1:nrow(result)) {
  if (result[i] < 0) {
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

# Calculate Jaccard distance and determine recommendations
result <- result[1,] 
sim_mat <- rbind.data.frame(result, genre_matrix)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:ratings]))
rows <- which(sim_results == min(sim_results))
movies[rows,2]