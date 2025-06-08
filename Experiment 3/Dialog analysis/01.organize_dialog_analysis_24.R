
################################################################################
############################## EXTRACT THE DATA ################################
################################################################################


base_dir <- "C:/Users/eylul/Desktop/2024_data"
base_dir <- "."
# List the specific file paths within the folder
file_paths <- c(
  "RANDOM24/fq1gn8j7-2024-11-27/ChatMessages-2024-11-27.csv",
  "RANDOM24/pqesetud-2024-12-09/ChatMessages-2024-12-09.csv",
  "WS24/e4n3narf-2024-12-02/ChatMessages-2024-12-03.csv",
  "WS24/lyrisecf-2024-12-04/ChatMessages-2024-12-04.csv"
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
#write.csv(combined_data, "chat_data_N24.csv", row.names = FALSE)

################################################################################
################################################################################
################################################################################
library(tm)
library(textstem)
combined_chat_data <- read.csv("chat_data_N24.csv")

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
highly_clustered_codes <- c("lyrisecf", "e4n3narf")
# Create the 'network' column based on the 'session_code' values
combined_chat_data$network <- ifelse(
  combined_chat_data$session_code %in% highly_clustered_codes, 
  "highly-clustered", 
  "random"
)
#save.image(file = "line94.RData")
#write.csv(combined_chat_data, "processed_chat_data_N24.csv", row.names = FALSE)
combined_chat_data <- read.csv("processed_chat_data_N24.csv")