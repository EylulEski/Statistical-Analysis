
library(dplyr)
library(readxl)

#Lexical diversity analysis
combined_data_long <- read_excel("NLP_answers_24.xlsx")

#Lexical diversity is a measure of the variety of words used in a text relative to the total number of words.
# Function to calculate lexical diversity 
calculate_lv <- function(Answers_clean) {
  num_unique_words <- length(unique(Answers_clean))
  total_words <- length(Answers_clean)
  lv <- num_unique_words / total_words
  return(lv)
}

names(combined_data_long)

# Calculate lv by subsession.round_number, item_number, session_id, and condition
lvdata <- combined_data_long %>%
  dplyr::filter(Answers_clean != "") %>%  # Remove empty answers
  group_by(subsession.round_number, item_number, condition, session_id) %>% #include network_size if you analyse both N24 and N12
  summarize(lexical_diversity = calculate_lv(Answers_clean), .groups = 'drop')
View(lvdata)
# Display the results
print(lvdata, n = 256)
# Export the dataset to a CSV file

################################################################################
############################## glmer analysis###################################
library(lme4)
library(lmerTest)
lvdata <- rename(lvdata, figure = item_number) # new_name = old_name
lvdata <- rename(lvdata, network = condition) # new_name = old_name
lvdata <- rename(lvdata, round = subsession.round_number) # new_name = old_name
str(lvdata)
lvdata <- lvdata %>% mutate_if(is.character, as.factor)
str(lvdata)
contrasts(lvdata$network)


# Interaction effect between round number and condition for each item_number
lvmod0 <- lmer(lexical_diversity ~ 1 + (1 | session_id)  , data = lvdata) #does not converge

lm_model <- lm(lexical_diversity ~ network * round , data = lvdata)
summary(lm_model) # NS effect of network. *** round
library(car)
anova(lm_model)
Anova(lm_model, type = "III")

library(ggplot2)
# Line plot to show change in lexical diversity across rounds for each item_number and condition
ggplot(lvdata, aes(x = network, y = lexical_diversity, color = network)) + # group = interaction(item_number, condition)
  #  geom_line() +
  geom_point() +
  #facet_wrap(~ item_number, scales = "free_y") +  # Facet by item_number
  labs(title = "Change in Lexical Diversity Across Networks",
       # x = "Subsession Round Number", 
       y = "Type-Token Ratio (Lexical Diversity)") +
  theme_minimal()


################################################################################
#############################frequency of top 5 words ##########################
#calculate top 5 answers for each condition and item

# Group by condition, item_number, and answers to calculate the frequency
answer_frequencies <- combined_data_long %>%
  group_by(condition, item_number, Answers_clean) %>%
  summarise(frequency = n()) %>%
  ungroup()

# Calculate total frequency for each condition and item_number
total_frequencies <- answer_frequencies %>%
  group_by(condition, item_number) %>%
  summarise(total_frequency = sum(frequency))

# Join total frequencies back with answer frequencies
answer_frequencies <- answer_frequencies %>%
  left_join(total_frequencies, by = c("condition", "item_number"))

# Sort by frequency in descending order and find the top answers for each condition and item_number
top_answers <- answer_frequencies %>%
  arrange(condition, item_number, desc(frequency)) %>%
  group_by(condition, item_number) %>%
  slice_max(frequency, n = 1)

# Calculate percentage of each top 5 answer
top_answers <- top_answers %>%
  mutate(percentage = (frequency / total_frequency) * 100)

# View the result
print(top_answers)


# Plot the top 5 answers
library(ggplot2)

# Create a bar plot to show the top answers' percentage for each condition and item_number
ggplot(top_answers, aes(x = item_number, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percentage, 1)), vjust = -0.3, position = position_dodge(0.9)) +
  labs(title = "Top Answers by Item Number and Condition",
       x = "Item Number", 
       y = "Percentage of Use (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

################################################################################
#############################frequency of target words ##########################

# Filter for the target words and their respective item_numbers across conditions
target_words <- answer_frequencies %>%
  filter((item_number == "reordered_keywords_1" & Answers_clean == "shield") |
           (item_number == "reordered_keywords_2" & Answers_clean == "angel") |
           (item_number == "reordered_keywords_3" & Answers_clean == "flag") |
           (item_number == "reordered_keywords_4" & Answers_clean == "antenna") |
           (item_number == "reordered_keywords_5" & Answers_clean == "apron"))

# Calculate the percentage of each answer
target_words <- target_words %>%
  mutate(percentage = (frequency / total_frequency) * 100)

# Create a bar plot to visualize the percentages across conditions for these specific words
ggplot(target_words, aes(x = item_number, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percentage, 1)), vjust = -0.3, position = position_dodge(0.9)) +
  labs(title = "Percentage of Target Words Across Conditions",
       x = "Item Number",
       y = "Percentage of Use (%)",
       fill = "Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


################################################################################
######################Visualize target word percentages#########################

library(dplyr)

# Replace "ws" with "clustered" in the condition column
target_words <- target_words %>%
  mutate(condition = case_when(
    condition == "ws" ~ "clustered",
    TRUE ~ condition  # Keep other conditions unchanged
  ))

# Plot the data with the updated condition label
ggplot(target_words, aes(x = item_number, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percentage, 1)), vjust = -0.3, position = position_dodge(0.9)) +
  labs(title = "Percentage of Target Words",
       x = "Item Number",
       y = "Percentage of Use (%)",
       fill = "Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


#visualize the model
#set up a graphic device
# png("C:\\Users\\eylul\\Desktop\\newplot.png",width=3600,height=3000,units="px",res=400,bg="white") 
# 
# ggplot(target_words, aes(x = item_number, y = percentage, fill = condition)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(percentage, 1)), vjust = -0.3, position = position_dodge(0.9),size = 5) +
#   labs(title = "",
#        x = "Item",
#        y = "Percentage of Use (%)",
#        fill = "Condition") +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 18),     # Adjust axis title size
#     axis.text = element_text(size = 18),      # Adjust axis tick labels size
#     plot.title = element_text(size = 20, face = "bold"), # Adjust main title size
#     legend.title = element_text(size = 22),   # Adjust legend title size
#     legend.text = element_text(size = 20),    # Adjust legend text size
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
#   )
# #turn off device
# dev.off()
