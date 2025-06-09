# Authors: Eylul Eski, Luca Onnis
# Date: 2024-10-28

library(tidyverse)
library(lme4)
library(bayestestR)
library(car)
library(readxl)


cdl  <- read_excel("N24_all_three.xlsx")


library(dplyr)

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
cdl$confederate <- factor(cdl$confederate, levels = c("seperated", "clustered", "random_seed"))
(levels(cdl$confederate))

contrasts(cdl$network)
contrasts(cdl$network) <- matrix(rev(contr.sum(2)))
#View(cdl)
#head(cdl)
levels(cdl$item) 
levels(cdl$session_id) 
length(levels(cdl$subject)) 
names(cdl)
cdl$round
table(cdl$network)
str(cdl)
cdl$round <- as.integer(cdl$round)

str(cdl)

#m1 <- glmer(target ~  1 + (1 | session_id / subject) + (1 | seed)  , data = cdl, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#m1 <- glmer(target ~  1 + (1 | subject) + (1 |session_id), data = cdl, family = binomial) #does not converge
m1 <- glmer(target ~  1 + (1 | subject), data = cdl, family = binomial) 
summary(m1)
#anova(m1)
Anova(m1, type = "III")

#model with the manipulation of interest:
m2 <- update(m1, . ~ . + confederate, control = glmerControl(optimizer = "bobyqa") )
Anova(m2, type = "III") # network n.s.
summary(m2)
#anova(m2)

m3 <- update(m2, . ~ . + round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#summary(m3)
Anova(m3, type = "III") # no sign. interaction 

#m4 <- update(m3, . ~ . + confederate : round, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#Anova(m4, type = "III")
#summary(m4)

# model comparison:
anova(m1,m2)
anova(m2,m3) 
#anova(m3,m4)

# with flexplot library:
#install.packages("devtools") # Devtools is a package which allows to do this.
# devtools::install_github("dustinfife/flexplot")
library(flexplot)
model.comparison(m1,m2) 
model.comparison(m2,m3) 
#model.comparison(m3,m4) 
