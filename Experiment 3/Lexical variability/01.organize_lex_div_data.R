#combined_data_long <- readxl::read_excel("combined_data_long.xlsx")
combined_data_long <- combined_data_24_and_12
library(SnowballC)
library(tm)
library(textstem)

corpus = VCorpus(VectorSource(combined_data_long$answers))


removeHyphens <- function(x) {
  # Replace hyphens with spaces
  x <- gsub("-", " ", x)
  return(x)
}

corpus = tm_map(corpus, content_transformer(tolower)) #lowers cases
corpus = tm_map(corpus, stripWhitespace) # removes whitespaces
corpus = tm_map(corpus, removePunctuation) # removes punctuations
corpus = tm_map(corpus, removeNumbers) # removes numbers

corpus = tm_map(corpus, content_transformer(removeHyphens))
mystopwords <- c(stopwords("english"), "like")
corpus <- tm_map(corpus, removeWords, mystopwords)
corpus<- tm_map(corpus, content_transformer(lemmatize_strings))
corpus = tm_map(corpus, stripWhitespace)

# Convert the corpus back to a vector of strings
answers_vector <- sapply(corpus, as.character)

# Add the answers vector as a new column to the dataset
combined_data_long$Answers_clean <- answers_vector


# Convert the Answers_clean column into a text corpus
corpus <- VCorpus(VectorSource(combined_data_long$Answers_clean))

###########################################################
# Custom function to remove "head" if it is the second word
removeHeadIfSecondWord <- function(x) {
  words <- unlist(strsplit(x, " "))
  if (length(words) > 1 && tolower(words[2]) == "head") {
    words <- words[-2]
  }
  return(paste(words, collapse = " "))
}

# Apply the function to the corpus
corpus <- tm_map(corpus, content_transformer(removeHeadIfSecondWord))

# Update the Answers_clean column with the modified text
combined_data_long$Answers_clean <- sapply(corpus, as.character)

###########################################################
# Function to replace "hour glass" with "hourglass" and noseplug to nose plug
replaceWords <- function(x) {
  x <- gsub("hour glass", "hourglass", x, ignore.case = TRUE)
  x <- gsub("noseplug", "nose plug", x, ignore.case = TRUE)
  return(x)
}

# Apply the functions to the corpus
corpus <- tm_map(corpus, content_transformer(replaceWords))

# Update the Answers_clean column with the modified text
combined_data_long$Answers_clean <- sapply(corpus, as.character)

#############################################################
# Function to remove numbers from the text
removeNumbers <- function(x) {
  x <- gsub("[0-9]+", "", x)
  return(x)
}

# Apply the functions to the corpus
corpus <- tm_map(corpus, content_transformer(removeNumbers))

# Update the Answers_clean column with the modified text
combined_data_long$Answers_clean <- sapply(corpus, as.character)

#Remove empty cells
combined_data_long <- combined_data_long %>%
  filter(Answers_clean != "")



View(combined_data_long)
#write.csv(combined_data_long, "NLP_answers_24_12.csv", row.names = FALSE)