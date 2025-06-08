# Authors: Eylul Eski, Luca Onnis
# Date: 2024-10-28

library(tidyverse)
library(lme4)
library(bayestestR)
library(car)
library(dplyr)
library(readxl)

#choose the data 
cdl <- read_excel("combined_data_long_N24.xlsx")
#cdl <- read_excel("combined_data_24_and_12.xlsx")
str(cdl)
cdl <- rename(cdl, item = item_number) # new_name = old_name
cdl <- rename(cdl, subject = participant_number) # new_name = old_name
cdl <- rename(cdl, round = subsession.round_number) # new_name = old_name
cdl <- rename(cdl, network = condition) # new_name = old_name
cdl <- cdl %>% mutate_if(is.character, as.factor)
cdl <- cdl %>% mutate_if(is.integer, as.factor)
cdl$target <- factor(cdl$target)
cdl$network_size <- as.factor(cdl$network_size)

levels(cdl$round)
levels(cdl$network_size)
#setting up the contrasts for target variable
contrasts(cdl$network)
contrasts(cdl$network) <- matrix(rev(contr.sum(2)))
#View(cdl)
#head(cdl)
levels(cdl$item) # 2 items (pictures to be described)
levels(cdl$session_id) # 4 diffusion replications/chains (2 in each network type/network)
length(levels(cdl$subject)) # 73 real subjects
names(cdl)
cdl$round
table(cdl$network)
str(cdl)
cdl$round <- as.integer(cdl$round)

str(cdl)

# MODEL COMPARISONS -----
# model with random structure only:
# https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified
# nested: (1|School/Class) or equivalently (1|School) + (1|Class:School)


m1 <- glmer(target ~  1 + (1 | subject) + (1 |session_id), data = cdl, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) #does not converge
m1 <- glmer(target ~  1 + (1 | subject), data = cdl, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m1)
anova(m1)

#model with the manipulation of interest:
m2 <- update(m1, . ~ . + network )
Anova(m2, type = "III") # network n.s.
summary(m2)

m3 <- update(m2, . ~ . + round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
Anova(m3, type = "III") 
levels(cdl$round)
summary(m3)
# model comparison:
anova(m1,m2)
anova(m2,m3) 

# Full model
m4 <- update(m3, . ~ . + network : round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
Anova(m4, type = "III")
summary(m4)

# with flexplot library:
#install.packages("devtools") # Devtools is a package which allows to do this.
# devtools::install_github("dustinfife/flexplot")
library(flexplot)
model.comparison(m1,m2) 
model.comparison(m2,m3) 
model.comparison(m3,m4) 
