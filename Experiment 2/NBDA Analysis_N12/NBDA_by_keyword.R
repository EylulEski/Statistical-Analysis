library(NBDA)
library(igraph)
library(readxl)

####################################################################ANALYSIS WITH KEYWORDS##########################################################
####################################################################################################################################################

###############################################################WS NETWORK###########################################################################


# Create and store adjacency matrices for each round as separate variables
round_1 <-as.matrix(read.csv(file="Round_1_Matrix.csv"))
round_2 <-as.matrix(read.csv(file="Round_2_Matrix.csv"))
round_3 <-as.matrix(read.csv(file="Round_3_Matrix.csv"))
round_4 <-as.matrix(read.csv(file="Round_4_Matrix.csv"))
round_5 <-as.matrix(read.csv(file="Round_5_Matrix.csv"))
round_6 <-as.matrix(read.csv(file="Round_6_Matrix.csv"))
round_7 <-as.matrix(read.csv(file="Round_7_Matrix.csv"))
round_8 <-as.matrix(read.csv(file="Round_8_Matrix.csv"))
round_9 <-as.matrix(read.csv(file="Round_9_Matrix.csv"))

#########################################################
#Create a demons vector full of zeroes
demons<-rep(0,12)
#and slot in 1s for 9, 10 and 11
demons[10]<-demons[11]<-demons[12]<-1
demons

#########################################################
#Acquisition orders for each word in the networks

oa_ws_9cax_word1<- c(1, 2, 4, 6, 8, 9, 7, NA, NA) #3, 5 are 0
oa_ws_9cax_word2<- c(1, 2, 4, 5, 6, 3, 9, NA, NA) #7, 8 are 0
oa_ws_9cax_word3<- c(1, 2, 4, 6, NA, NA, NA, NA, NA) #3, 5, 7, 8, 9 are 0
oa_ws_9cax_word4<- c(1, 2, 4, 6, 3, 5, 7, 8, 9) #full
oa_ws_9cax_word5<- c(1, 2, 4, 6, 3, NA, NA, NA, NA) #5, 7, 8, 9 are 0

oa_ws_hg73_word1<- c(1, 2, 3, 4, 6, 9, 5, 7, 8) #full
oa_ws_hg73_word2<- c(1, 2, 4, 6, 3, 5, 7, 9, NA) #8 is 0
oa_ws_hg73_word3<- c(1, 2, 4, 6, 3, 8, 9, 7, NA) #5 is 0
oa_ws_hg73_word4<- c(1, 2, 4, 6, 9, NA, NA, NA, NA) #3, 5, 7, 8 are 0
oa_ws_hg73_word5<- c(1, 2, 3, 8, 4, 6, 5, 7, 9) #full

oa_ws_j8urh_word1<- c(1, 2, 4, 6, 7, 9, NA, NA, NA) #3, 5, 8 are 0
oa_ws_j8urh_word2<- c(1, 2, 4, 6, 3, 5, 7, 8, 9) #full
oa_ws_j8urh_word3<- c(1, 2, 4, NA, NA, NA, NA, NA, NA) #3, 5, 6, 7, 8, 9 are 0
oa_ws_j8urh_word4<- c(1, 2, 4, 6, 5, 7, 9, NA, NA) #3, 8 are 0
oa_ws_j8urh_word5<- c(1, 2, 4, 6, NA, NA, NA, NA, NA) #3, 5, 7, 8, 9 are 0

oa_ws_slpk_word1<- c(1, 2, 4, 5, 6, 3, 7, 8, 9) #full
oa_ws_slpk_word2<- c(1, 2, 3, 6, NA, NA, NA, NA, NA) #4, 5, 7, 8, 9 are 0
oa_ws_slpk_word3<- c(1, 2, 6, 3, NA, NA, NA, NA, NA)#4, 5, 7, 8, 9 are 0
oa_ws_slpk_word4<- c(3, 6, NA, NA, NA, NA, NA, NA, NA) #1, 2, 4, 5, 7, 8, 9 are 0
oa_ws_slpk_word5<- c(1, 2, 6, 3, NA, NA, NA, NA, NA) #4, 5, 7, 8, 9 are 0

#######################################################################
############################WS KEYWORD 1###############################
#######################################################################

#Create the empty array
ws_9cax_w1<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_9cax_w1[,,,1]<-round_3
ws_9cax_w1[,,,2]<-round_5
ws_9cax_w1[,,,3]<-round_6
ws_9cax_w1[,,,4]<-round_9


Index_9cax_word1<-c(rep(1,2),rep(2,2), rep(3,2),rep(4,1), NA)


#Create the empty array
ws_hg73_w1<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
ws_hg73_w1[,,,1]<-round_3
ws_hg73_w1[,,,2]<-round_5
ws_hg73_w1[,,,3]<-round_6

Index_hg73_word1<-c(rep(1,2),rep(2,4), rep(3,3))


#Create the empty array
ws_j8urh_w1<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
ws_j8urh_w1[,,,1]<-round_3
ws_j8urh_w1[,,,2]<-round_5
ws_j8urh_w1[,,,3]<-round_9

Index_j8urh_word1<-c(rep(1,2),rep(2,2), rep(3,2), NA)


#Create the empty array
ws_slpk_w1<-array(NA,dim=c(12,12,1,6))
#Slot in the network for each time period
ws_slpk_w1[,,,1]<-round_3
ws_slpk_w1[,,,2]<-round_4
ws_slpk_w1[,,,3]<-round_5
ws_slpk_w1[,,,4]<-round_6
ws_slpk_w1[,,,5]<-round_7
ws_slpk_w1[,,,6]<-round_9

Index_slpk_word1<-c(rep(1,2),rep(2,2), rep(3,1),rep(4,2), rep(5,1), rep(6,1))

#######################################################################
############################WS KEYWORD 2###############################
#######################################################################

#Create the empty array
ws_9cax_w2<-array(NA,dim=c(12,12,1,5))
#Slot in the network for each time period
ws_9cax_w2[,,,1]<-round_3
ws_9cax_w2[,,,2]<-round_4
ws_9cax_w2[,,,3]<-round_5
ws_9cax_w2[,,,4]<-round_6
ws_9cax_w2[,,,5]<-round_7


Index_9cax_word2<-c(rep(1,2),rep(2,2), rep(3,1),rep(4,1), rep(5,1), NA)


#Create the empty array
ws_hg73_w2<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_hg73_w2[,,,1]<-round_3
ws_hg73_w2[,,,2]<-round_5
ws_hg73_w2[,,,3]<-round_6
ws_hg73_w2[,,,4]<-round_7

Index_hg73_word2<-c(rep(1,2),rep(2,2), rep(3,3), rep(4,1), NA)


#Create the empty array
ws_j8urh_w2<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_j8urh_w2[,,,1]<-round_3
ws_j8urh_w2[,,,2]<-round_5
ws_j8urh_w2[,,,3]<-round_6
ws_j8urh_w2[,,,4]<-round_7


Index_j8urh_word2<-c(rep(1,2),rep(2,2), rep(3,3),rep(4,2))


#Create the empty array
ws_slpk_w2<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
ws_slpk_w2[,,,1]<-round_3
ws_slpk_w2[,,,2]<-round_6


Index_slpk_word2<-c(rep(1,2),rep(2,2), NA)


#######################################################################
############################WS KEYWORD 3###############################
#######################################################################

#Create the empty array
ws_9cax_w3<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
ws_9cax_w3[,,,1]<-round_3
ws_9cax_w3[,,,2]<-round_5


Index_9cax_word3<-c(rep(1,2),rep(2,2), NA)


#Create the empty array
ws_hg73_w3<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_hg73_w3[,,,1]<-round_3
ws_hg73_w3[,,,2]<-round_5
ws_hg73_w3[,,,3]<-round_6
ws_hg73_w3[,,,4]<-round_7


Index_hg73_word3<-c(rep(1,2),rep(2,2), rep(3,3), rep(4,1), NA)


#Create the empty array
ws_j8urh_w3<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
ws_j8urh_w3[,,,1]<-round_3
ws_j8urh_w3[,,,2]<-round_5

Index_j8urh_word3<-c(rep(1,2),rep(2,1), NA)


#Create the empty array
ws_slpk_w3<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
ws_slpk_w3[,,,1]<-round_3
ws_slpk_w3[,,,2]<-round_5
ws_slpk_w3[,,,3]<-round_6

Index_slpk_word3<-c(rep(1,2),rep(2,1), rep(3,1), NA)


#######################################################################
############################WS KEYWORD 4###############################
#######################################################################

#Create the empty array
ws_9cax_w4<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_9cax_w4[,,,1]<-round_3
ws_9cax_w4[,,,2]<-round_5
ws_9cax_w4[,,,3]<-round_6
ws_9cax_w4[,,,4]<-round_7


Index_9cax_word4<-c(rep(1,2),rep(2,2), rep(3,3),rep(4,2))


#Create the empty array
ws_hg73_w4<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
ws_hg73_w4[,,,1]<-round_3
ws_hg73_w4[,,,2]<-round_5
ws_hg73_w4[,,,3]<-round_7


Index_hg73_word4<-c(rep(1,2),rep(2,2), rep(3,1), NA)


#Create the empty array
ws_j8urh_w4<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_j8urh_w4[,,,1]<-round_3
ws_j8urh_w4[,,,2]<-round_5
ws_j8urh_w4[,,,3]<-round_6
ws_j8urh_w4[,,,4]<-round_7


Index_j8urh_word4<-c(rep(1,2),rep(2,2), rep(3,2), rep(4,1), NA)


#Create the empty array
ws_slpk_w4<-array(NA,dim=c(12,12,1,1))
#Slot in the network for each time period
ws_slpk_w4[,,,1]<-round_6


Index_slpk_word4<-c(rep(1,2),NA)

#######################################################################
############################WS KEYWORD 5###############################
#######################################################################

#Create the empty array
ws_9cax_w5<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
ws_9cax_w5[,,,1]<-round_3
ws_9cax_w5[,,,2]<-round_5
ws_9cax_w5[,,,3]<-round_6


Index_9cax_word5<-c(rep(1,2),rep(2,2), rep(3,1),NA)


#Create the empty array
ws_hg73_w5<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_hg73_w5[,,,1]<-round_3
ws_hg73_w5[,,,2]<-round_5
ws_hg73_w5[,,,3]<-round_7
ws_hg73_w5[,,,4]<-round_9

Index_hg73_word5<-c(rep(1,2),rep(2,2), rep(3,1), rep(4,4))


#Create the empty array
ws_j8urh_w5<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
ws_j8urh_w5[,,,1]<-round_3
ws_j8urh_w5[,,,2]<-round_5

Index_j8urh_word5<-c(rep(1,2),rep(2,2), NA)


#Create the empty array
ws_slpk_w5<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
ws_slpk_w5[,,,1]<-round_3
ws_slpk_w5[,,,2]<-round_5
ws_slpk_w5[,,,3]<-round_6

Index_slpk_word5<-c(rep(1,2),rep(2,1), rep(3,1), NA)

###################################################
#Fit the models

ws_9cax_w1<-nbdaData(label="diff_ws_9cax_w1",assMatrix=ws_9cax_w1,orderAcq = oa_ws_9cax_word1,demons = demons, assMatrixIndex = Index_9cax_word1)
ws_9cax_w2<-nbdaData(label="diff_ws_9cax_w2",assMatrix=ws_9cax_w2,orderAcq = oa_ws_9cax_word2,demons = demons, assMatrixIndex = Index_9cax_word2)
ws_9cax_w3<-nbdaData(label="diff_ws_9cax_w3",assMatrix=ws_9cax_w3,orderAcq = oa_ws_9cax_word3,demons = demons, assMatrixIndex = Index_9cax_word3)
ws_9cax_w4<-nbdaData(label="diff_ws_9cax_w4",assMatrix=ws_9cax_w4,orderAcq = oa_ws_9cax_word4,demons = demons, assMatrixIndex = Index_9cax_word4)
ws_9cax_w5<-nbdaData(label="diff_ws_9cax_w5",assMatrix=ws_9cax_w5,orderAcq = oa_ws_9cax_word5,demons = demons, assMatrixIndex = Index_9cax_word5)

ws_hg73_w1<-nbdaData(label="diff_ws_hg73_w1",assMatrix=ws_hg73_w1,orderAcq = oa_ws_hg73_word1,demons = demons, assMatrixIndex = Index_hg73_word1)
ws_hg73_w2<-nbdaData(label="diff_ws_hg73_w2",assMatrix=ws_hg73_w2,orderAcq = oa_ws_hg73_word2,demons = demons, assMatrixIndex = Index_hg73_word2)
ws_hg73_w3<-nbdaData(label="diff_ws_hg73_w3",assMatrix=ws_hg73_w3,orderAcq = oa_ws_hg73_word3,demons = demons, assMatrixIndex = Index_hg73_word3)
ws_hg73_w4<-nbdaData(label="diff_ws_hg73_w4",assMatrix=ws_hg73_w4,orderAcq = oa_ws_hg73_word4,demons = demons, assMatrixIndex = Index_hg73_word4)
ws_hg73_w5<-nbdaData(label="diff_ws_hg73_w5",assMatrix=ws_hg73_w5,orderAcq = oa_ws_hg73_word5,demons = demons, assMatrixIndex = Index_hg73_word5)

ws_j8urh_w1<-nbdaData(label="diff_ws_j8urh_w1",assMatrix=ws_j8urh_w1,orderAcq = oa_ws_j8urh_word1,demons = demons, assMatrixIndex = Index_j8urh_word1)
ws_j8urh_w2<-nbdaData(label="diff_ws_j8urh_w2",assMatrix=ws_j8urh_w2,orderAcq = oa_ws_j8urh_word2,demons = demons, assMatrixIndex = Index_j8urh_word2)
ws_j8urh_w3<-nbdaData(label="diff_ws_j8urh_w3",assMatrix=ws_j8urh_w3,orderAcq = oa_ws_j8urh_word3,demons = demons, assMatrixIndex = Index_j8urh_word3)
ws_j8urh_w4<-nbdaData(label="diff_ws_j8urh_w4",assMatrix=ws_j8urh_w4,orderAcq = oa_ws_j8urh_word4,demons = demons, assMatrixIndex = Index_j8urh_word4)
ws_j8urh_w5<-nbdaData(label="diff_ws_j8urh_w5",assMatrix=ws_j8urh_w5,orderAcq = oa_ws_j8urh_word5,demons = demons, assMatrixIndex = Index_j8urh_word5)

ws_slpk_w1<-nbdaData(label="diff_ws_slpk_w1",assMatrix=ws_slpk_w1,orderAcq = oa_ws_slpk_word1,demons = demons, assMatrixIndex = Index_slpk_word1)
ws_slpk_w2<-nbdaData(label="diff_ws_slpk_w2",assMatrix=ws_slpk_w2,orderAcq = oa_ws_slpk_word2,demons = demons, assMatrixIndex = Index_slpk_word2)
ws_slpk_w3<-nbdaData(label="diff_ws_slpk_w3",assMatrix=ws_slpk_w3,orderAcq = oa_ws_slpk_word3,demons = demons, assMatrixIndex = Index_slpk_word3)
ws_slpk_w4<-nbdaData(label="diff_ws_slpk_w4",assMatrix=ws_slpk_w4,orderAcq = oa_ws_slpk_word4,demons = demons, assMatrixIndex = Index_slpk_word4)
ws_slpk_w5<-nbdaData(label="diff_ws_slpk_w5",assMatrix=ws_slpk_w5,orderAcq = oa_ws_slpk_word5,demons = demons, assMatrixIndex = Index_slpk_word5)


#data.frame(Variable=model_dynamic@varNames,MLE=model_dynamic@outputPar,SE=model_dynamic@se)
multiDiffModel1<-oadaFit(list(ws_9cax_w1, ws_9cax_w2, ws_9cax_w3, ws_9cax_w4, ws_9cax_w5, ws_hg73_w1, ws_hg73_w2, ws_hg73_w3, ws_hg73_w4, ws_hg73_w5, ws_j8urh_w1, ws_j8urh_w2, ws_j8urh_w3, ws_j8urh_w4, ws_j8urh_w5, ws_slpk_w1, ws_slpk_w2, ws_slpk_w3, ws_slpk_w4, ws_slpk_w5))
multiDiffModel2<-oadaFit(list(ws_9cax_w1, ws_9cax_w2, ws_9cax_w3, ws_9cax_w4, ws_9cax_w5, ws_hg73_w1, ws_hg73_w2, ws_hg73_w3, ws_hg73_w4, ws_hg73_w5, ws_j8urh_w1, ws_j8urh_w2, ws_j8urh_w3, ws_j8urh_w4, ws_j8urh_w5, ws_slpk_w1, ws_slpk_w2, ws_slpk_w3, ws_slpk_w4, ws_slpk_w5), type= "asocial")

data.frame(Variable=multiDiffModel1@varNames,MLE=multiDiffModel1@outputPar,SE=multiDiffModel1@se)

#KEYWORD_1
multiDiffW1<-oadaFit(list(ws_9cax_w1, ws_hg73_w1, ws_j8urh_w1, ws_slpk_w1))
data.frame(Variable=multiDiffW1@varNames,MLE=multiDiffW1@outputPar,SE=multiDiffW1@se)
nbdaPropSolveByST(model=multiDiffW1)

#KEYWORD_2
multiDiffW2<-oadaFit(list(ws_9cax_w2, ws_hg73_w2, ws_j8urh_w2, ws_slpk_w2))
data.frame(Variable=multiDiffW2@varNames,MLE=multiDiffW2@outputPar,SE=multiDiffW2@se)
nbdaPropSolveByST(model=multiDiffW2)

#KEYWORD_3
multiDiffW3<-oadaFit(list(ws_9cax_w3, ws_hg73_w3, ws_j8urh_w3, ws_slpk_w3))
data.frame(Variable=multiDiffW3@varNames,MLE=multiDiffW3@outputPar,SE=multiDiffW3@se)


#KEYWORD_4
multiDiffW4<-oadaFit(list(ws_9cax_w4, ws_hg73_w4, ws_j8urh_w4, ws_slpk_w4))
data.frame(Variable=multiDiffW4@varNames,MLE=multiDiffW4@outputPar,SE=multiDiffW4@se)
nbdaPropSolveByST(model=multiDiffW4)

#KEYWORD_5
multiDiffW5<-oadaFit(list(ws_9cax_w5, ws_hg73_w5, ws_j8urh_w5, ws_slpk_w5))
data.frame(Variable=multiDiffW5@varNames,MLE=multiDiffW5@outputPar,SE=multiDiffW5@se)

multiDiffModel1@aicc #120, lower AICc and therefore is a better fit
multiDiffModel2@aicc #128 
#compare the social and asocial models
multiDiffModel2@aicc-multiDiffModel1@aicc #7.65

# SE looks large. Does this mean there is not good evidence for social transmission?
#Not necessarily, as we shall see.

#The value estimated for s might be difficult to interpret, depending on the network
#used. In such cases, we can obtain an estimate of the % of events that occured by social
#transmission as opposed to asocial learning (%ST) as follows:
nbdaPropSolveByST(model=multiDiffModel1)
#P(Network 1)  P(S offset) 
#0.47182    0.00000 
#This tells us that the estimated value for s corresponds to 55% (the function returns a 
#proportion so multiply by 100 to get %ST)
exp(0.5*(multiDiffModel2@aicc-multiDiffModel1@aicc))
#[1] 45.8
#the social model is 45.8x more likely to be the best K-L model, out of the two.
#Or we can say the social model has 45.8x more support than the asocial model.

#We can also conduct a likelihood ratio test (LRT) for social transmission
#The @loglik slot contains the -log-likelihood- i.e. minus the log-likelihood
#So we can get the test statistic as double the difference in -log-likelihood as follows:
2*(multiDiffModel2@loglik-multiDiffModel1@loglik)
#[1] 9.7
#There is 1 parameter in model_social, and 0 in model_asocial, so we have 1 d.f.
pchisq(2*(multiDiffModel2@loglik-multiDiffModel1@loglik),df=1,lower.tail=F)
#[1] 0.0018 (p<0.05), Social model is  statistically different than asocial model.


####################################################################################################################################################
#############################################################RANDOM NETWORK#########################################################################

# Create and store adjacency matrices for each round as separate variables
random_round_1 <-as.matrix(read.csv(file="random_round_1.csv"))
random_round_2 <-as.matrix(read.csv(file="random_round_2.csv"))
random_round_3 <-as.matrix(read.csv(file="random_round_3.csv"))
random_round_4 <-as.matrix(read.csv(file="random_round_4.csv"))
random_round_5 <-as.matrix(read.csv(file="random_round_5.csv"))
random_round_6 <-as.matrix(read.csv(file="random_round_6.csv"))
random_round_7 <-as.matrix(read.csv(file="random_round_7.csv"))
random_round_8 <-as.matrix(read.csv(file="random_round_8.csv"))
random_round_9 <-as.matrix(read.csv(file="random_round_9.csv"))

#Create a demons vector full of zeroes
demons<-rep(0,12)
#and slot in 1s for 9, 10 and 11
demons[10]<-demons[11]<-demons[12]<-1
demons

#########################################################
#Acquisition orders for each word in the networks

oa_random_b948_word1<- c(2, 5, 8, 9, 4, 6, 7, NA, NA) 
oa_random_jdf4_word1<- c(3, 7, 4, 6, 8, NA, NA, NA, NA) 
oa_random_ot0u_word1<- c(2, 3, 5, 7, 8, 9, NA, NA, NA) 
oa_random_sdth_word1<- c(1, 9, 4, 5, 6, 7, 8, NA, NA)

oa_random_b948_word2<- c(5, 6, 4, 8, NA, NA, NA, NA, NA) 
oa_random_jdf4_word2<- c(1, 9, 2, 4, 8, 5, 6, 7, NA) 
oa_random_ot0u_word2<- c(5, 9, 6, NA, NA, NA, NA, NA, NA)
# no acquisition in oa_random_sdth_word2

oa_random_b948_word3<- c(5, 9, 6, 4, 8, NA, NA, NA, NA) 
oa_random_jdf4_word3<- c(1, 9, 2, 5, 8, NA, NA, NA, NA)
oa_random_ot0u_word3<- c(3, 7, 8, NA, NA, NA, NA, NA, NA)
oa_random_sdth_word3<- c(1, 4, 2, 3, 7, 8, 5, 6, NA)


oa_random_b948_word4<- c(1, 9, 2, 4, 5, 6, 7, 8, NA)
oa_random_ot0u_word4<- c(2, 3, 7, 8, NA, NA, NA, NA, NA)
oa_random_sdth_word4<-  c(2, 5, 8, 9, 4, 6, 7, NA, NA) 
# no acquitision in oa_random_jdf4_word4

oa_random_jdf4_word5<-c(2, 9, 8, NA, NA, NA, NA, NA, NA) 
oa_random_ot0u_word5<-c(2, 3, 7, 8, 4, 5, 6, NA, NA)
oa_random_sdth_word5<-c(1, 4, 2, 8, 7, 6, NA, NA, NA)
#no acquisition in oa_random_b948_word5

#######################################################################
############################RANDOM KEYWORD 1###########################
#######################################################################

#Create the empty array
random_b948_w1<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_b948_w1[,,,1]<-random_round_5
random_b948_w1[,,,2]<-random_round_6

Index_b948_word1<-c(rep(1,4),rep(2,3), NA)


#Create the empty array
random_jdf4_w1<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
random_jdf4_w1[,,,1]<-random_round_5
random_jdf4_w1[,,,2]<-random_round_6
random_jdf4_w1[,,,3]<-random_round_7
random_jdf4_w1[,,,4]<-random_round_9

Index_jdf4_word1<-c(rep(1,2),rep(2,1), rep(3,1), rep(4,1), NA)


#Create the empty array
random_ot0u_w1<-array(NA,dim=c(12,12,1,1))
#Slot in the network for each time period
random_ot0u_w1[,,,1]<-random_round_5

Index_ot0u_word1<-c(rep(1,6), NA)


#Create the empty array
random_sdth_w1<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
random_sdth_w1[,,,1]<-random_round_3
random_sdth_w1[,,,2]<-random_round_4
random_sdth_w1[,,,3]<-random_round_6
random_sdth_w1[,,,4]<-random_round_9

Index_sdth_word1<-c(rep(1,2),rep(2,1),rep(3,3),rep(4,1), NA)

#######################################################################
############################RANDOM KEYWORD 2###########################
#######################################################################

#Create the empty array
random_b948_w2<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_b948_w2[,,,1]<-random_round_6
random_b948_w2[,,,2]<-random_round_7

Index_b948_word2<-c(rep(1,2),rep(2,2), NA)


#Create the empty array
random_jdf4_w2<-array(NA,dim=c(12,12,1,5))
#Slot in the network for each time period
random_jdf4_w2[,,,1]<-random_round_3
random_jdf4_w2[,,,2]<-random_round_4
random_jdf4_w2[,,,3]<-random_round_5
random_jdf4_w2[,,,4]<-random_round_7
random_jdf4_w2[,,,5]<-random_round_9

Index_jdf4_word2<-c(rep(1,2),rep(2,2), rep(3,1), rep(4,2), rep(5,1), NA)


#Create the empty array
random_ot0u_w2<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_ot0u_w2[,,,1]<-random_round_5
random_ot0u_w2[,,,2]<-random_round_6

Index_ot0u_word2<-c(rep(1,2), rep(2,1), NA)

#######################################################################
############################RANDOM KEYWORD 3###########################
#######################################################################

#Create the empty array
random_b948_w3<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
random_b948_w3[,,,1]<-random_round_5
random_b948_w3[,,,2]<-random_round_6
random_b948_w3[,,,3]<-random_round_7

Index_b948_word3<-c(rep(1,2),rep(2,1),rep(3,2), NA)


#Create the empty array
random_jdf4_w3<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
random_jdf4_w3[,,,1]<-random_round_3
random_jdf4_w3[,,,2]<-random_round_4
random_jdf4_w3[,,,3]<-random_round_5

Index_jdf4_word3<-c(rep(1,2),rep(2,1), rep(3,2), NA)


#Create the empty array
random_ot0u_w3<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_ot0u_w3[,,,1]<-random_round_5
random_ot0u_w3[,,,2]<-random_round_9

Index_ot0u_word3<-c(rep(1,2), rep(2,1), NA)


#Create the empty array
random_sdth_w3<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
random_sdth_w3[,,,1]<-random_round_4
random_sdth_w3[,,,2]<-random_round_5
random_sdth_w3[,,,3]<-random_round_6

Index_sdth_word3<-c(rep(1,2),rep(2,4),rep(3,2), NA)

#######################################################################
############################RANDOM KEYWORD 4###########################
#######################################################################

#Create the empty array
random_b948_w4<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
random_b948_w4[,,,1]<-random_round_3
random_b948_w4[,,,2]<-random_round_4
random_b948_w4[,,,3]<-random_round_6
random_b948_w4[,,,4]<-random_round_7

Index_b948_word4<-c(rep(1,2),rep(2,2), rep(3,3), rep(4,1), NA)


#Create the empty array
random_ot0u_w4<-array(NA,dim=c(12,12,1,1))
#Slot in the network for each time period
random_ot0u_w4[,,,1]<-random_round_5

Index_ot0u_word4<-c(rep(1,4), NA)


#Create the empty array
random_sdth_w4<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_sdth_w4[,,,1]<-random_round_5
random_sdth_w4[,,,2]<-random_round_6

Index_sdth_word4<-c(rep(1,4),rep(2,3), NA)

#######################################################################
############################RANDOM KEYWORD 5###########################
#######################################################################

#Create the empty array
random_jdf4_w5<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_jdf4_w5[,,,1]<-random_round_4
random_jdf4_w5[,,,2]<-random_round_5

Index_jdf4_word5<-c(rep(1,2),rep(2,1), NA)


#Create the empty array
random_ot0u_w5<-array(NA,dim=c(12,12,1,2))
#Slot in the network for each time period
random_ot0u_w5[,,,1]<-random_round_5
random_ot0u_w5[,,,2]<-random_round_6

Index_ot0u_word5<-c(rep(1,4), rep(2,3), NA)


#Create the empty array
random_sdth_w5<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
random_sdth_w5[,,,1]<-random_round_4
random_sdth_w5[,,,2]<-random_round_5
random_sdth_w5[,,,3]<-random_round_6
random_sdth_w5[,,,4]<-random_round_7

Index_sdth_word5<-c(rep(1,2),rep(2,2),rep(3,1),rep(4,1), NA)


###################################################
#Fit the models
random_b948_w1<-nbdaData(label="diff_ws_b948_w1",assMatrix=random_b948_w1,orderAcq = oa_random_b948_word1,demons = demons, assMatrixIndex = Index_b948_word1)
random_b948_w2<-nbdaData(label="diff_ws_b948_w2",assMatrix=random_b948_w2,orderAcq = oa_random_b948_word2,demons = demons, assMatrixIndex = Index_b948_word2)
random_b948_w3<-nbdaData(label="diff_ws_b948_w3",assMatrix=random_b948_w3,orderAcq = oa_random_b948_word3,demons = demons, assMatrixIndex = Index_b948_word3)
random_b948_w4<-nbdaData(label="diff_ws_b948_w4",assMatrix=random_b948_w4,orderAcq = oa_random_b948_word4,demons = demons, assMatrixIndex = Index_b948_word4)

random_jdf4_w1<-nbdaData(label="diff_random_jdf4_w1",assMatrix=random_jdf4_w1,orderAcq = oa_random_jdf4_word1,demons = demons, assMatrixIndex = Index_jdf4_word1)
random_jdf4_w2<-nbdaData(label="diff_random_jdf4_w2",assMatrix=random_jdf4_w2,orderAcq = oa_random_jdf4_word2,demons = demons, assMatrixIndex = Index_jdf4_word2)
random_jdf4_w3<-nbdaData(label="diff_random_jdf4_w3",assMatrix=random_jdf4_w3,orderAcq = oa_random_jdf4_word3,demons = demons, assMatrixIndex = Index_jdf4_word3)
random_jdf4_w5<-nbdaData(label="diff_random_jdf4_w5",assMatrix=random_jdf4_w5,orderAcq = oa_random_jdf4_word5,demons = demons, assMatrixIndex = Index_jdf4_word5)

random_ot0u_w1<-nbdaData(label="diff_random_ot0u_w1",assMatrix=random_ot0u_w1,orderAcq = oa_random_ot0u_word1,demons = demons, assMatrixIndex = Index_ot0u_word1)
random_ot0u_w2<-nbdaData(label="diff_random_ot0u_w2",assMatrix=random_ot0u_w2,orderAcq = oa_random_ot0u_word2,demons = demons, assMatrixIndex = Index_ot0u_word2)
random_ot0u_w3<-nbdaData(label="diff_random_ot0u_w3",assMatrix=random_ot0u_w3,orderAcq = oa_random_ot0u_word3,demons = demons, assMatrixIndex = Index_ot0u_word3)
random_ot0u_w4<-nbdaData(label="diff_random_ot0u_w4",assMatrix=random_ot0u_w4,orderAcq = oa_random_ot0u_word4,demons = demons, assMatrixIndex = Index_ot0u_word4)
random_ot0u_w5<-nbdaData(label="diff_random_ot0u_w5",assMatrix=random_ot0u_w5,orderAcq = oa_random_ot0u_word5,demons = demons, assMatrixIndex = Index_ot0u_word5)

random_sdth_w1<-nbdaData(label="diff_random_sdth_w1",assMatrix=random_sdth_w1,orderAcq = oa_random_sdth_word1,demons = demons, assMatrixIndex = Index_sdth_word1)
random_sdth_w3<-nbdaData(label="diff_random_sdth_w3",assMatrix=random_sdth_w3,orderAcq = oa_random_sdth_word3,demons = demons, assMatrixIndex = Index_sdth_word3)
random_sdth_w4<-nbdaData(label="diff_random_sdth_w4",assMatrix=random_sdth_w4,orderAcq = oa_random_sdth_word4,demons = demons, assMatrixIndex = Index_sdth_word4)
random_sdth_w5<-nbdaData(label="diff_random_sdth_w5",assMatrix=random_sdth_w5,orderAcq = oa_random_sdth_word5,demons = demons, assMatrixIndex = Index_sdth_word5)


#data.frame(Variable=model_dynamic@varNames,MLE=model_dynamic@outputPar,SE=model_dynamic@se)
random_multiDiffModel1<-oadaFit(list(random_b948_w1, random_b948_w2, random_b948_w3, random_b948_w4, random_jdf4_w1, random_jdf4_w2, random_jdf4_w3, random_jdf4_w5, random_ot0u_w1, random_ot0u_w2, random_ot0u_w3, random_ot0u_w4, random_ot0u_w5, random_sdth_w1, random_sdth_w3, random_sdth_w4, random_sdth_w5))
random_multiDiffModel2<-oadaFit(list(random_b948_w1, random_b948_w2, random_b948_w3, random_b948_w4, random_jdf4_w1, random_jdf4_w2, random_jdf4_w3, random_jdf4_w5, random_ot0u_w1, random_ot0u_w2, random_ot0u_w3, random_ot0u_w4, random_ot0u_w5, random_sdth_w1, random_sdth_w3, random_sdth_w4, random_sdth_w5), type= "asocial")

data.frame(Variable=random_multiDiffModel1@varNames,MLE=random_multiDiffModel1@outputPar,SE=random_multiDiffModel1@se)

#KEYWORD_1
random_multiDiffW1<-oadaFit(list(random_b948_w1, random_jdf4_w1, random_ot0u_w1, random_sdth_w1))
data.frame(Variable=random_multiDiffW1@varNames,MLE=random_multiDiffW1@outputPar,SE=random_multiDiffW1@se)

#KEYWORD_2
random_multiDiffW2<-oadaFit(list(random_b948_w2, random_jdf4_w2, random_ot0u_w2))
data.frame(Variable=random_multiDiffW2@varNames,MLE=random_multiDiffW2@outputPar,SE=random_multiDiffW2@se)

#KEYWORD_3
random_multiDiffW3<-oadaFit(list(random_b948_w3, random_jdf4_w3, random_ot0u_w3, random_sdth_w3))
data.frame(Variable=random_multiDiffW3@varNames,MLE=random_multiDiffW3@outputPar,SE=random_multiDiffW3@se)

#KEYWORD_4
random_multiDiffW4<-oadaFit(list(random_b948_w4, random_ot0u_w4, random_sdth_w4))
data.frame(Variable=random_multiDiffW4@varNames,MLE=random_multiDiffW4@outputPar,SE=random_multiDiffW4@se)

#KEYWORD_5
random_multiDiffW5<-oadaFit(list(random_jdf4_w5, random_ot0u_w5, random_sdth_w5))
data.frame(Variable=random_multiDiffW5@varNames,MLE=random_multiDiffW5@outputPar,SE=random_multiDiffW5@se)

random_multiDiffModel1@aicc 
random_multiDiffModel2@aicc 
#compare the social and asocial models
random_multiDiffModel2@aicc-random_multiDiffModel1@aicc 
