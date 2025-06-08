# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)


df <- raw

# Reshape the data into long format and remove rows where 'answers' is NA
df_long <- df %>%
  pivot_longer(cols = c(reordered_keywords_1, reordered_keywords_4), 
               names_to = "item_number", 
               values_to = "answers") %>%
  drop_na(answers)  # Remove rows with NA in the 'answers' column

# View the transformed data
print(df_long)

df_long <- df_long %>%
  mutate(
    across(where(is.character), tolower),  # Convert all text to lowercase
    
    # Standardize answers in a single case_when() call
    answers = case_when(
      item_number == "reordered_keywords_1" & answers %in% c("sheild", "a shield", "dragon shield", "figure 2 looks like a shield",
                                                             "my figure 2 looks like shield", "shield ") ~ "shield",
      item_number == "reordered_keywords_4" & answers %in% c("bug antenna", "antenna", "antenna ", "antena",
                                                             "antennae ", "antennas", "bug antennas", " antennae",
                                                             "bug antennae", "an antenna", "antennea", 	
                                                             "heart antenna", "figure 2 looks like an antenna", 
                                                             "my figure 1 looks like antennae", "snail antenna", "snails antenna") ~ "antennae",
      TRUE ~ answers  # Keep everything else unchanged
    )
  )


# View the transformed data
print(df_long)

df_long <- df_long %>%
  mutate(
    target = case_when(
      item_number == "reordered_keywords_1" & answers == "shield" ~ 1,
      item_number == "reordered_keywords_4" & answers == "antennae" ~ 1,
      TRUE ~ 0  # Everything else is 0
    )
  )

write.csv(df_long, "combined_data_long_N24_unclustered.csv")



