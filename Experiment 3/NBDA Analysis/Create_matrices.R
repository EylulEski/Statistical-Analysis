

#Create WS Matrices

# Initialize the list of pairs
pairs <- list(
  `1` = list(c(20, 1), c(21, 2), c(22, 4), c(23, 7), c(24, 10)),
  `2` = list(c(20, 2), c(21, 1), c(22, 7), c(23, 8), c(24, 13)),
  `3` = list(c(20, 9), c(1, 2), c(21, 4), c(22, 5), c(23, 10), c(24, 11)),
  `4` = list(c(20, 12), c(2, 4), c(21, 5), c(22, 8), c(23, 11), c(24, 14), c(13, 15), c(9, 3)),
  `5` = list(c(1, 12), c(5, 7), c(8, 10), c(11, 13), c(14, 15), c(3, 18), c(9, 6)),
  `6` = list(c(14, 16), c(15, 17), c(18, 19), c(6, 3), c(12, 9), c(4, 5), c(7, 8), c(10, 11)),
  `7` = list(c(15, 16), c(17, 18), c(3, 19), c(12, 6), c(13, 14)),
  `8` = list(c(16, 17), c(19, 6)),
  `9` = list(c(16, 18), c(17, 19))
)

# Function to create a 24x24 matrix with 1s at specified pairs
create_matrix <- function(pair_list) {
  # Initialize a 24x24 matrix with zeros
  mat <- matrix(0, nrow = 24, ncol = 24)
  
  # Iterate through the pairs and set the matrix values to 1
  for (pair in pair_list) {
    mat[pair[1], pair[2]] <- 1
    mat[pair[2], pair[1]] <- 1 # Symmetric matrix
  }
  
  return(mat)
}

# Create a list of matrices for all rounds
matrices <- lapply(pairs, create_matrix)

# Optional: Save the matrices or display
for (i in 1:length(matrices)) {
  cat(paste("Matrix for Round", i, "\n"))
  print(matrices[[i]])
}

# Save each matrix to a CSV file
for (i in 1:length(matrices)) {
  file_name <- paste0("matrix_round_", i, ".csv")
  write.csv(matrices[[i]], file_name, row.names = FALSE)
  cat(paste("Saved:", file_name, "\n"))
}
#########################################################################
#Create random matrices

# Initialize the new list of pairings
pairings <- list(
  `1` = list(c(20, 7), c(23, 1), c(21, 10), c(22, 4), c(24, 5)),
  `2` = list(c(20, 11), c(1, 4), c(23, 2), c(21, 17), c(5, 14), c(22, 16), c(24, 7), c(10, 18)),
  `3` = list(c(20, 3), c(2, 13), c(21, 19), c(4, 10), c(5, 16), c(22, 12), c(23, 15), c(24, 17)),
  `4` = list(c(20, 9), c(1, 13), c(21, 6), c(4, 14), c(5, 18), c(22, 11), c(7, 12), c(3, 8), 
             c(23, 17), c(15, 16), c(24, 19)),
  `5` = list(c(11, 1), c(19, 2), c(12, 8), c(10, 15), c(9, 18)),
  `6` = list(c(2, 6), c(7, 13), c(8, 18), c(17, 9), c(11, 14), c(15, 3)),
  `7` = list(c(8, 19), c(13, 16), c(14, 3), c(6, 9)),
  `8` = list(c(6, 12))
)

# Function to create a 24x24 matrix with 1s at specified pairs
create_matrix <- function(pair_list) {
  # Initialize a 24x24 matrix with zeros
  mat <- matrix(0, nrow = 24, ncol = 24)
  
  # Iterate through the pairs and set the matrix values to 1
  for (pair in pair_list) {
    mat[pair[1], pair[2]] <- 1
    mat[pair[2], pair[1]] <- 1 # Symmetric matrix
  }
  
  return(mat)
}

# Create a list of matrices for all pairings
random_matrices <- lapply(pairings, create_matrix)

# Save each matrix to a CSV file
for (i in 1:length(random_matrices)) {
  file_name <- paste0("random_matrix_round_", i, ".csv")
  write.csv(random_matrices[[i]], file_name, row.names = FALSE)
  cat(paste("Saved:", file_name, "\n"))
}


