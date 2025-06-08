
################################################################################
######################### Lexical Diversity of Chat ############################
################################################################################
library(dplyr)
library(tidytext)
combined_chat_data <- read.csv("processed_chat_data_N24.csv")
# Filter out empty or whitespace-only answers in `body_lemma`
combined_chat_data <- combined_chat_data %>%
  filter(!is.na(body_lemma) & body_lemma != "" & grepl("\\S", body_lemma))

# Tokenize the `body_lemma` column and calculate lexical diversity
lv_chat <- combined_chat_data %>%
  select(network, session_code, body_lemma) %>%
  unnest_tokens(word, body_lemma) %>%          # Tokenize the text into individual words
  group_by(network, session_code) %>%          # Group by network and session
  summarise(
    total_tokens = n(),                        # Count total words (tokens)
    unique_types = n_distinct(word),           # Count unique words (types)
    lexical_diversity = unique_types / total_tokens  # Calculate TTR
  ) %>%
  arrange(desc(lexical_diversity))             # Arrange by lexical diversity 

# View the results
print(lv_chat)

################################################################################
################################# Analysis #####################################
################################################################################
library(lmerTest)
library(car)

lv_chat <- lv_chat %>%
  mutate(network = as.factor(network))
lv_chat <- lv_chat %>% mutate_if(is.character, as.factor)
str(lv_chat)

contrasts(lv_chat$network)
lv_chat$network2 <- lv_chat$network

contrasts(lv_chat$network2) <- matrix(rev(contr.sum(2)))
lv_chat$network2 <- factor(lv_chat$network2, levels = c("random", "highly-clustered"))
contrasts(lv_chat$network2) <- matrix(rev(contr.sum(2)))
levels(lv_chat$network2)
lv_chat
# the effect of network on lexical diversity
lm_model <- lm(lexical_diversity ~ network, data = lv_chat)
summary(lm_model)
library("effects")
plot(effect("network", lm_model))

anova(lm_model)
Anova(lm_model, type = "III")
