# Authors: Eylul Eski, Luca Onnis
# Date: 2024-10-28
cdl <- read.csv("combined_data_long.csv") # !! file doesn't exist
cdl <- readxl::read_excel("combined_data_long_N12.xlsx")
library(tidyverse)
library(lme4)
library(bayestestR)
library(car)

library(dplyr)
#cdl <- combined_data_long
cdl <- combined_data_24_and_12
str(cdl)
cdl <- rename(cdl, item = item_number) # new_name = old_name
cdl <- rename(cdl, subject = participant_number) # new_name = old_name
cdl <- rename(cdl, round = subsession.round_number) # new_name = old_name
cdl <- rename(cdl, network = condition) # new_name = old_name
#cdl <- rename(cdl, subject_order = participant.id_in_session) # new_name = old_name
cdl <- cdl %>% mutate_if(is.character, as.factor)
cdl <- cdl %>% mutate_if(is.integer, as.factor)
cdl$target <- as.integer(cdl$target)
cdl$network_size <- as.factor(cdl$network_size)

levels(cdl$round)
levels(cdl$network_size)
#setting up the contrasts for target variable
contrasts(cdl$network)
#contrasts(cdl$network) <- matrix(rev(contr.sum(2)))
#View(cdl)
#head(cdl)
levels(cdl$item) # 5 items (pictures to be described)
levels(cdl$session_id) # 8 diffusion replications/chains (4 in each network type/network)
length(levels(cdl$subject)) # 72 real subjects, 9 subjects in each network + 3 confederates ( who were excluded from analysis)
names(cdl)
cdl$round
table(cdl$network)
str(cdl)
cdl$round <- as.integer(cdl$round)
#cdl$round <- scale(cdl$round)

str(cdl)
View(cdl)
head(cdl)
#cdl %>% filter(target == 1) %>% select (item,answers,target,seed) %>% unique() #%>% arrange_at(answers) %>% unique()

#summary_data <- cdl %>%
#  group_by(network, item) %>%
#  summarize(total_answers = sum(target)) %>%
#  ungroup()
#summary_data

proportion_data <- cdl %>%
  group_by(network) %>%
  summarize(
    total_responses = n(),              # Total responses per item and network
    target_count = sum(target == 1)     # Count of target responses (where target == 1)
  ) %>%
  mutate(proportion_target = ifelse(total_responses > 0, target_count / total_responses, 0)) %>%
  ungroup()

# View the result
print(proportion_data)


# 1. Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
# Perform the Mann-Whitney U Test
wilcox_result <- wilcox.test(proportion_target ~ network, data = proportion_data)

# Print the result
print(wilcox_result) # W = 22, p-value = 0.05556, but in favour of random network




# MODEL COMPARISONS -----
# model with random structure only:
# https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified
# nested: (1|School/Class) or equivalently (1|School) + (1|Class:School)

#m1 <- glmer(target ~  1 + (1 | session_id / subject) + (1 | seed)  , data = cdl, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
m1 <- glmer(target ~  1 + (1 | session_id / subject), 
            data = cdl, family = binomial)
summary(m1)
anova(m1)

#model with the manipulation of interest:
m2 <- update(m1, . ~ . + network )
#Anova(m2, type = "III") # network n.s.
summary(m2)

m3 <- update(m2, . ~ . + round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
Anova(m3, type = "III") # sign. interaction 
levels(cdl$round)
summary(m3)
# model comparison:
anova(m1,m2)
anova(m2,m3) # m3 

# m4 <- update(m3, . ~ . + network : round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


m3_v2 <- m1 <- glmer(target ~  1 + (1 | subject) + network : network_size, data = cdl, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
Anova(m3_v2, type = "III") 
summary(m3_v2)

# Full model
m4 <- update(m3, . ~ . + network : round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
Anova(m4, type = "III")
summary(m4)

# Only Round 10 -- when subjects are prompted to name the 5 images at the end of the game
round10 <- cdl %>% filter(round == 10)
View(round10)
mr1 <- glmer(target ~  1 + (1 | session_id / subject), data = round10, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#model with the manipulation of interest:
mr2 <- update(mr1, . ~ . + network )
#Anova(mr2, type = "III") # network n.s.
summary(mr2)

# Only N24 with 19 participants and 5 confs-- 
network24 <- cdl %>% filter(network_size == 24)
View(network24)
m_n24 <- glmer(target ~  1 + (1 | subject ), data = network24, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#model with the manipulation of interest:
m2_n24 <- update(m_n24, . ~ . + network )
#Anova(mr2, type = "III") # network n.s.
summary(m2_n24)

# Plot effects -----
library("effects")
plot(effect("network", m4))
plot(effect("scale(round)", m4))
plot(effect("network : round", m4))
# Bayes Factors ----
# A Bayes factor smaller than 1/3 indicates substantial evidence in favor of the null-model) (Wetzels et al. 2011).
threshold = 1/3
threshold
library(bayestestR)
bf_m2m1 <- bayesfactor_models(m2, denominator = m1)  # BF = 
bf_m3m2 <- bayesfactor_models(m3, denominator = m2) # BF = 
bf_m4m3 <- bayesfactor_models(m4,m3, denominator = m3) # BF = 
bf_m4m3

bf_mr2mr1 <- bayesfactor_models(mr2,mr1, denominator = mr1) # BF = 
bf_mr2mr1

# with flexplot library:
#install.packages("devtools") # Devtools is a package which allows to do this.
# devtools::install_github("dustinfife/flexplot")
library(flexplot)
model.comparison(m1,m2) # higher BF for m2 = 2.2
model.comparison(m2,m3) # higher BF for m3 = 12
model.comparison(m3,m4) # higher BF for m3 = 33
#library(emmeans)
#emtrends(m4, pairwise ~ network, var = "subject_order")
#emmip(m4, network ~ subject_order, cov.reduce = range) # same result as above

library(interactions)
interact_plot( model = m4, modx = network, pred = round, interval = TRUE, int.type = "confidence")

anova_results <- anova (m3, m4)
anova_results

# make a dataframe with the model output 
m1_table <- data.frame(coef(summary(m1)))
m2_table <- data.frame(coef(summary(m2)))
m3_table <- data.frame(coef(summary(m3)))
m4_table <- data.frame(coef(summary(m4)))
m3_table
m4_table
anova_results_df <- as.data.frame(anova_results)
