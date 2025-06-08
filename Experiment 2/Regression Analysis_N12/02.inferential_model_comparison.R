# Authors: Eylul Eski, Luca Onnis
# Date: 2024-10-28
#cdl <- read.csv("combined_data_long.csv")
library(tidyverse)
library(lme4)
library(bayestestR)
library(car)
library(dplyr)

cdl <- readxl::read_excel("combined_data_long_N12.xlsx")
str(cdl)
cdl <- rename(cdl, item = item_number) # new_name = old_name
cdl <- rename(cdl, subject = participant_number) # new_name = old_name
cdl <- rename(cdl, round = subsession.round_number) # new_name = old_name
cdl <- rename(cdl, network = condition) # new_name = old_name
cdl <- cdl %>% mutate_if(is.character, as.factor)
cdl <- cdl %>% mutate_if(is.integer, as.factor)
cdl$target <- factor(cdl$target)
levels(cdl$round)

#setting up the contrasts for target variable
contrasts(cdl$network)
contrasts(cdl$network) <- matrix(rev(contr.sum(2)))
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
str(cdl)

# MODEL COMPARISONS -----
# model with random structure only:

m1 <- glmer(target ~  1 + (1 | subject) + (1 |session_id), data = cdl, family = binomial)
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


# Bayes Factors ----
# A Bayes factor smaller than 1/3 indicates substantial evidence in favor of the null-model) (Wetzels et al. 2011).
# with flexplot library:
#install.packages("devtools") # Devtools is a package which allows to do this.
# devtools::install_github("dustinfife/flexplot")
library(flexplot)
model.comparison(m1,m2) # higher BF for m1 = 8.4
model.comparison(m2,m3) # higher BF for m3 = 5.6
model.comparison(m3,m4) # higher BF for m4 = 25.4

# Plot effects -----
library("effects")
plot(effect("network", m4))
plot(effect("scale(round)", m4))
plot(effect("network : round", m4))

library(interactions)
interact_plot( model = m4, modx = network, pred = round, interval = TRUE, int.type = "confidence")

# make a dataframe with the model output 
m1_table <- data.frame(coef(summary(m1)))
m2_table <- data.frame(coef(summary(m2)))
m3_table <- data.frame(coef(summary(m3)))
m4_table <- data.frame(coef(summary(m4)))
m3_table
m4_table
anova_results_df <- as.data.frame(anova_results)
