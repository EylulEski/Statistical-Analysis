# Preprocess data from lexical diffusion game
# Date: 2024-10
# Authors: Eylul Eski
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(car)
# Define the list of file paths ----
file_paths <- c(
  "RANDOM12-OK-jdf4sgo1-2024-06-24/figures_app_2024-06-24.csv",
  "RANDOM12-OK-48b9jmgp-2024-06-26/figures_app_2024-06-27.csv",
  "RANDOM12-OK-ot0ui8sk-2024-06-24/figures_app_2024-06-24.csv",
  "RANDOM12-OK-sdth5ohy-2024-06-25/figures_app_2024-06-25.csv",
  "WS12-OK-9caxmijh-2024-06-21/figures_app_2024-06-21.csv",
  "WS12-OK-hg739rpo-2024-06-20/figures_app_2024-06-20.csv",
  "WS12-OK-j8urhz5m-2024-06-20/figures_app_2024-06-20.csv",
  "WS12-OK-slpk75c0-2024-06-26/figures_app_2024-06-26.csv"
)

# Define a function to process each file
process_file <- function(file_path) {
  # Read the CSV file
  randmn <- read.csv(file_path)
  
  # Extract condition from file path
  condition <- ifelse(grepl("RANDOM12", file_path), "random", ifelse(grepl("WS12", file_path), "WS", NA))
  
  # Extract session_id from file path
  session_id <- sub(".*OK-(.*?)-2024.*", "\\1", file_path)
  
  # Select relevant columns (1, 25-34, 41)
  selected_columns <- randmn %>% select(1, 25:34, 41)
  
  # Function to reorder words based on numerical values
  reorder_words <- function(words, order) {
    ordered_words <- words[order(order)]
    return(ordered_words)
  }
  
  # Apply the reordering to each row
  selected_columns <- selected_columns %>%
    rowwise() %>%
    mutate(
      reordered_keywords = list(reorder_words(c_across(2:6), c_across(7:11)))
    ) %>%
    unnest_wider(reordered_keywords, names_sep = "_")
  
  # Rename columns for clarity
  colnames(selected_columns)[2:6] <- paste0("player.keyword", 0:4)
  colnames(selected_columns)[7:11] <- paste0("player.card", 0:4)
  
  # Add condition and session_id columns
  selected_columns <- selected_columns %>% 
    mutate(condition = condition, session_id = session_id)
  
  # Remove rows that are completely empty
  selected_columns <- selected_columns %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  # Remove rows where participant.id_in_session is 10, 11, or 12
  selected_columns <- selected_columns %>%
    filter(!participant.id_in_session %in% c(10, 11, 12))
  
  # Remove rows if reordered_keywords_1 to reordered_keywords_5 are entirely empty
  selected_columns <- selected_columns %>%
    filter(!(is.na(reordered_keywords_1) & is.na(reordered_keywords_2) & is.na(reordered_keywords_3) & is.na(reordered_keywords_4) & is.na(reordered_keywords_5) |
               reordered_keywords_1 == "" & reordered_keywords_2 == "" & reordered_keywords_3 == "" & reordered_keywords_4 == "" & reordered_keywords_5 == ""))
  
  # Keep only relevant columns including subsession.round_number
  selected_columns <- selected_columns %>%
    select(participant.id_in_session, condition, session_id, subsession.round_number, reordered_keywords_1, reordered_keywords_2, reordered_keywords_3, reordered_keywords_4, reordered_keywords_5)
  
  # Define the output file path
  output_file_path <- sub(".csv", "_cleaned.csv", file_path)
  
  # Save the cleaned data to a new CSV file
  # write.csv(selected_columns, output_file_path, row.names = FALSE)
  
  # Return the cleaned data frame
  return(selected_columns)
}

# Apply the processing function to each file in the list and save the results in a list
cleaned_data_list <- lapply(file_paths, process_file)

# Name the cleaned data frames based on the original file names
names(cleaned_data_list) <- sub(".csv", "_cleaned", basename(file_paths))

# Combine all cleaned data frames into one data frame
combined_data <- bind_rows(cleaned_data_list)

# Convert all character columns to lowercase
combined_data <- combined_data %>%
  mutate(across(where(is.character), tolower))

# View the combined data (Optional)
print(combined_data)

#########################
# Define the list of file paths
file_paths <- c(
  "RANDOM12-OK-jdf4sgo1-2024-06-24/figures_app_2024-06-24.csv",
  "RANDOM12-OK-48b9jmgp-2024-06-26/figures_app_2024-06-27.csv",
  "RANDOM12-OK-ot0ui8sk-2024-06-24/figures_app_2024-06-24.csv",
  "RANDOM12-OK-sdth5ohy-2024-06-25/figures_app_2024-06-25.csv",
  "WS12-OK-9caxmijh-2024-06-21/figures_app_2024-06-21.csv",
  "WS12-OK-hg739rpo-2024-06-20/figures_app_2024-06-20.csv",
  "WS12-OK-j8urhz5m-2024-06-20/figures_app_2024-06-20.csv",
  "WS12-OK-slpk75c0-2024-06-26/figures_app_2024-06-26.csv"
)

# Define a function to process each file
process_file <- function(file_path) {
  # Read the CSV file
  randmn <- read.csv(file_path)
  
  # Select relevant columns (1, 35-39)
  selected_columns <- randmn %>% select(1, 35:39, 41)
  
  # Extract condition from file path
  condition <- ifelse(grepl("RANDOM12", file_path), "random", ifelse(grepl("WS12", file_path), "WS", NA))
  
  # Extract session_id from file path
  session_id <- sub(".*OK-(.*?)-2024.*", "\\1", file_path)
  
  # Add condition and session_id columns
  selected_columns <- selected_columns %>% 
    mutate(condition = condition, session_id = session_id)
  
  #Change end survey round number to 10
  selected_columns <- selected_columns %>%
    mutate(subsession.round_number = ifelse(subsession.round_number == 9, 10, subsession.round_number))
  
  # Remove rows that are completely empty
  selected_columns <- selected_columns %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  # Remove rows where participant.id_in_session is 10, 11, or 12
  selected_columns <- selected_columns %>%
    filter(!participant.id_in_session %in% c(10, 11, 12))
  
  # Remove rows if reordered_keywords_1 to reordered_keywords_5 are entirely empty
  selected_columns <- selected_columns %>%
    filter(!(is.na(player.survey0) & is.na(player.survey1) & is.na(player.survey2) & is.na(player.survey3) & is.na(player.survey4) |
               player.survey0 == "" & player.survey1 == "" & player.survey2 == "" & player.survey3 == "" & player.survey4 == ""))
  
  # Reorder columns
  selected_columns <- selected_columns %>%
    select(participant.id_in_session, condition, session_id, subsession.round_number, player.survey0, player.survey1, player.survey2, player.survey3, player.survey4)
  
  # Return the cleaned data frame
  return(selected_columns)
}

# Apply the processing function to each file in the list and save the results in a list
cleaned_data_list2 <- lapply(file_paths, process_file)

# Name the cleaned data frames based on the original file names
names(cleaned_data_list2) <- sub(".csv", "_cleaned2", basename(file_paths))

# Combine all cleaned data frames into one data frame
combined_data2 <- bind_rows(cleaned_data_list2)

# Convert all character columns to lowercase
combined_data2 <- combined_data2 %>%
  mutate(across(where(is.character), tolower))

# View the combined data
combined_data2

# Make sure the column names are identical to append data
names(combined_data2) <- names(combined_data)

#appending combined data
combined_data <- rbind(combined_data, combined_data2)

#create a new column for participant number
combined_data <- combined_data %>%
  mutate(unique_combination = paste(participant.id_in_session, session_id, sep = "_")) %>%
  mutate(participant_number = paste0("p", as.numeric(factor(unique_combination)))) %>%
  select(-unique_combination)  # Remove the helper column

#in long data format
combined_data_long <- combined_data %>%
  pivot_longer(
    cols = starts_with("reordered_keywords"), # Select columns that start with "reordered_keywords"
    names_to = "item_number", # Name of the new column for the gathered column names
    values_to = "answers"      # Name of the new column for the gathered values
  ) %>%
  filter(answers != "" & !is.na(answers)) %>% # Filter out empty and NA answers
  mutate(
    answers = case_when(
      answers %in% c("antenna", "antenna ", "antennae ", "antennas", "bug antenna", "bug antennas", "bug antennae", "antennea") ~ "antennae",
      answers %in% c("sheild", "sheild ", "shield ", "a shield", "pointy shield", "spiky shield", "shield with spikes", "shield with spike", "a shield with four spikes") ~ "shield",
      answers %in% c("angel ", "angel wings", "angel wing", "angel shape", "clipped off angelwings") ~ "angel",
      answers %in% c("a flag", "flag ") ~ "flag",
      answers %in% c("hourglass or apron", "a apron", "a dresslike apron", "apron ") ~ "apron",
      TRUE ~ answers
    )
  )


# Create the target column based on the conditions specified
combined_data_long <- combined_data_long %>%
  mutate(target = case_when(
    item_number == 'reordered_keywords_1' & answers == 'shield' ~ 1,
    item_number == 'reordered_keywords_2' & answers == 'angel' ~ 1,
    item_number == 'reordered_keywords_3' & answers == 'flag' ~ 1,
    item_number == 'reordered_keywords_4' & answers == 'antennae' ~ 1, 
    item_number == 'reordered_keywords_5' & answers == 'apron' ~ 1,
    TRUE ~ 0
  ))

########################################################################
determine_game_number <- function(condition, id, round) {
  if (condition == "ws") {
    if (id == 1) {
      return(c(1, 2, 3, NA, 4, NA, NA, NA, NA, 5)[round])
    } else if (id == 2) {
      return(c(NA, 1, 3, 3, 4, NA, NA, NA, NA, 5)[round])
    } else if (id == 3) {
      return(c(NA, NA, 1, 2, 3, 4, NA, NA, NA, 5)[round])
    } else if (id == 4) {
      return(c(NA, NA, 1, 2, 3, 4, NA, NA, NA, 5)[round])
    } else if (id == 5) {
      return(c(NA, NA, NA, 1, 2, 3, 4, NA, NA, 5)[round])
    } else if (id == 6) {
      return(c(NA, NA, NA, 1, 2, 3, 4, NA, NA, 5)[round])
    } else if (id == 7) {
      return(c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5)[round])
    } else if (id == 8) {
      return(c(NA, NA, NA, 1, 2, 3, 4, NA, NA, 5)[round])
    } else if (id == 9) {
      return(c(NA, NA, NA, NA, 1, 2, 3, NA, 4, 5)[round])
    }
  } else if (condition == "random") {
    if (id == 1) {
      return(c(1, 2, 3, 4, NA, NA, NA, NA, NA, 5)[round])
    } else if (id == 2) {
      return(c(NA, 1, 3, 3, 4, NA, NA, NA, NA, 5)[round])
    } else if (id == 3) {
      return(c(NA, NA, 1, 2, 3, 4, NA, NA, NA, 5)[round])
    } else if (id == 4) {
      return(c(NA, NA, NA, 1, 2, 3, 4, NA, NA, 5)[round])
    } else if (id == 5) {
      return(c(NA, NA, NA, 1, 2, 3, 4, NA, NA, 5)[round])
    } else if (id == 6) {
      return(c(NA, NA, NA, 1, 2, 3, 4, NA, NA, 5)[round])
    } else if (id == 7) {
      return(c(NA, NA, NA, 1, 2, 3, NA, NA, 4, 5)[round])
    } else if (id == 8) {
      return(c(NA, NA, NA, NA, 1, 2, 3, NA, 4, 5)[round])
    } else if (id == 9) {
      return(c(NA, NA, 1, 2, 3, NA, 4, NA, 5, 5)[round])
    }
  }
  return(NA)
}
combined_data_long <- combined_data_long %>%
  mutate(game_number = mapply(determine_game_number, condition, participant.id_in_session, subsession.round_number))

names(combined_data_long)
View(combined_data_long)
str(combined_data_long$item_number)

################################################################################
#################################double check###################################
################################################################################
# Load necessary library
library(dplyr)

# Define the target keywords for each item_number
target_keywords <- list(
  reordered_keywords_1 = "shield",
  reordered_keywords_2 = "angel",
  reordered_keywords_3 = "flag",
  reordered_keywords_4 = "antenna",
  reordered_keywords_5 = "apron"
)

# Filter the data to find rows where target is 0 but Answers_clean contains the required keyword
incorrect_targets <- combined_data_long %>%
  filter(
    target == 0 &
      ((item_number == "reordered_keywords_1" & grepl(target_keywords$reordered_keywords_1, answers, ignore.case = TRUE)) |
         (item_number == "reordered_keywords_2" & grepl(target_keywords$reordered_keywords_2, answers, ignore.case = TRUE)) |
         (item_number == "reordered_keywords_3" & grepl(target_keywords$reordered_keywords_3, answers, ignore.case = TRUE)) |
         (item_number == "reordered_keywords_4" & grepl(target_keywords$reordered_keywords_4, answers, ignore.case = TRUE)) |
         (item_number == "reordered_keywords_5" & grepl(target_keywords$reordered_keywords_5, answers, ignore.case = TRUE)))
  )

# View the results
print(incorrect_targets)


# Create a seed column to rename item_number (added by Luca)
combined_data_long <- combined_data_long %>%
  mutate(seed = case_when(
    item_number == 'reordered_keywords_1' ~ 'shield',
    item_number == 'reordered_keywords_2' ~ 'angel',
    item_number == 'reordered_keywords_3' ~ 'flag',
    item_number == 'reordered_keywords_4' ~ 'antennae', 
    item_number == 'reordered_keywords_5' ~ 'apron',
    TRUE ~ NA
  ))

##### SAVE DATA IN LONG FORMAT #####
Sys.getlocale()
Sys.getlocale("LC_NUMERIC")
str(combined_data_long)
#write.csv(combined_data_long, "combined_data_long.csv")

