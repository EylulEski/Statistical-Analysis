################################################################################
############################## EXTRACT THE DATA ################################
################################################################################


base_dir <- "C:/Users/eylul/Desktop/MatchingFigures-Stable/Network data"
base_dir <- "."
# List the specific file paths within the folder
file_paths <- c(
  "RANDOM12-OK-jdf4sgo1-2024-06-24/ChatMessages-2024-06-24.csv",
  "RANDOM12-OK-48b9jmgp-2024-06-26/ChatMessages-2024-06-27.csv",
  "RANDOM12-OK-ot0ui8sk-2024-06-24/ChatMessages-2024-06-24.csv",
  "RANDOM12-OK-sdth5ohy-2024-06-25/ChatMessages-2024-06-25.csv",
  "WS12-OK-9caxmijh-2024-06-21/ChatMessages-2024-06-21.csv",
  "WS12-OK-hg739rpo-2024-06-20/ChatMessages-2024-06-20.csv",
  "WS12-OK-j8urhz5m-2024-06-20/ChatMessages-2024-06-20.csv",
  "WS12-OK-slpk75c0-2024-06-26/ChatMessages-2024-06-26.csv"
)

# Generate full paths by combining the base directory and relative paths
full_paths <- file.path(base_dir, file_paths)

# Initialize an empty list to store each processed data frame
filtered_data_list <- list()

# Loop over each file path, process it, and store the result
for (file in full_paths) {
  # Read the data
  data <- read.csv(file)
  
  # Filter out rows where 'id_in_session' is 10, 11, or 12
  filtered_data <- subset(data, !(id_in_session %in% c(10, 11, 12)))
  
  # Select only the 'session_code' and 'body' columns
  filtered_data <- filtered_data[, c("id_in_session", "session_code", "body")]
  
  # Append the filtered data to the list
  filtered_data_list[[file]] <- filtered_data
}

# Combine all the filtered data into a single data frame
combined_data <- do.call(rbind, filtered_data_list)

# Save the combined data to a new CSV file
write.csv(combined_data, "combined_chat_data.csv", row.names = FALSE)

################################################################################
################################################################################
################################################################################
library(tm)
library(textstem)
combined_chat_data <- read.csv("combined_chat_data.csv")

#Create the corpus
corpus = VCorpus(VectorSource(combined_chat_data$body))
str(corpus)
#Clean raw data
corpus <- tm_map(corpus, content_transformer(tolower)) #lowers cases
corpus<- tm_map(corpus, stripWhitespace) # removes whitespaces
corpus<- tm_map(corpus, removePunctuation) # removes punctuations
corpus<- tm_map(corpus, removeNumbers) # removes numbers

# Stopwords such as “the”, “an” etc. do not provide much of valuable information and can be removed from the text. 
# Based on the context, you could also create custom stopwords list and remove them.
#mystopwords<- c(stopwords("english"),"figure", "fig", "hello", "yep", "yes", "no", "like", "call")
#corpus<- tm_map(corpus, removeWords, mystopwords)
corpus<- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus)
str(corpus)
# Lemmatization
corpus<- tm_map(corpus, content_transformer(lemmatize_strings))


#Create the Term-Document Matrix which represents document vectors in matrix format
tdm<- TermDocumentMatrix(corpus, control = list(wordlengths = c(1,Inf)))


# Convert the corpus back to a vector of strings
answers_vector <- sapply(corpus, as.character)

# Add the answers vector as a new column to the dataset
combined_chat_data$body_lemma <- answers_vector

#Create network column
# Define the session codes for the "highly-clustered" network type
highly_clustered_codes <- c("slpk75c0", "9caxmijh", "hg739rpo", "j8urhz5m")
# Create the 'network' column based on the 'session_code' values
combined_chat_data$network <- ifelse(
  combined_chat_data$session_code %in% highly_clustered_codes, 
  "highly-clustered", 
  "random"
)

