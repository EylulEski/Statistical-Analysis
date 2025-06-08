library(NBDA)
library(igraph)
library(readxl)

####################################################################ANALYSIS WITH KEYWORDS##########################################################
####################################################################################################################################################

###############################################################WS24 NETWORK###########################################################################


# Create and store adjacency matrices for each round as separate variables
round_1 <-as.matrix(read.csv(file="matrix_round_1.csv"))
round_2 <-as.matrix(read.csv(file="matrix_round_2.csv"))
round_3 <-as.matrix(read.csv(file="matrix_round_3.csv"))
round_4 <-as.matrix(read.csv(file="matrix_round_4.csv"))
round_5 <-as.matrix(read.csv(file="matrix_round_5.csv"))
round_6 <-as.matrix(read.csv(file="matrix_round_6.csv"))
round_7 <-as.matrix(read.csv(file="matrix_round_7.csv"))
round_8 <-as.matrix(read.csv(file="matrix_round_8.csv"))
round_9 <-as.matrix(read.csv(file="matrix_round_9.csv"))

#########################################################
#Create a demons vector full of zeroes
demons<-rep(0,24)
#and slot in 1s for 20, 21, 22, 23 and 24
demons[20]<-demons[21]<-demons[22]<-demons[23]<-demons[24]<-1
demons

#########################################################
#Acquisition orders for each word in the networks
oa_ws_e4n3_word1<- c(1, 2, 4, 5, 7, 8, 10, 12, 9, 11, 14, 16, 6, 17, 18, NA, NA, NA, NA) 
oa_ws_e4n3_word4<- c(1, 2, 4, 5, 7, 8, 10, 12, 3, 6, 9, 11, 19, 17, NA, NA, NA, NA, NA) 


oa_ws_lyri_word1<- c(1, 2, 4, 5, 7, 8, 10, 12, 9, 11, 13, 14, NA, NA, NA, NA, NA, NA, NA)
oa_ws_lyri_word4<- c(1, 2, 4, 13, 15, 5, 7, 8, 10, 11, 12, NA, NA, NA, NA, NA, NA, NA, NA)


#######################################################################
###########################WS24 KEYWORD 1##############################
#######################################################################

#Create the empty array
ws_e4n3_w1<-array(NA,dim=c(24,24,1,7))
#Slot in the network for each time period
ws_e4n3_w1[,,,1]<-round_3
ws_e4n3_w1[,,,2]<-round_4
ws_e4n3_w1[,,,3]<-round_5
ws_e4n3_w1[,,,4]<-round_6
ws_e4n3_w1[,,,5]<-round_7
ws_e4n3_w1[,,,6]<-round_8
ws_e4n3_w1[,,,7]<-round_9

Index_e4n3_word1<-c(rep(1,2),rep(2,1), rep(3,5), rep(4,4), rep(5,1), rep(6,1), rep(7,1), NA)


#Create the empty array
ws_lyri_w1<-array(NA,dim=c(24,24,1,5))
#Slot in the network for each time period
ws_lyri_w1[,,,1]<-round_3
ws_lyri_w1[,,,2]<-round_4
ws_lyri_w1[,,,3]<-round_5
ws_lyri_w1[,,,4]<-round_6
ws_lyri_w1[,,,5]<-round_7

Index_lyri_word1<-c(rep(1,2),rep(2,1), rep(3,5), rep(4,2), rep(5,2), NA)

#######################################################################
###########################WS24 KEYWORD 4##############################
#######################################################################

#Create the empty array
ws_e4n3_w4<-array(NA,dim=c(24,24,1,6))
#Slot in the network for each time period
ws_e4n3_w4[,,,1]<-round_3
ws_e4n3_w4[,,,2]<-round_4
ws_e4n3_w4[,,,3]<-round_5
ws_e4n3_w4[,,,4]<-round_6
ws_e4n3_w4[,,,5]<-round_8
ws_e4n3_w4[,,,6]<-round_9

Index_e4n3_word4<-c(rep(1,2),rep(2,1), rep(3,5), rep(4,4), rep(5,1), rep(6,1), NA)

#Create the empty array
ws_lyri_w4<-array(NA,dim=c(24,24,1,3))
#Slot in the network for each time period
ws_lyri_w4[,,,1]<-round_3
ws_lyri_w4[,,,2]<-round_4
ws_lyri_w4[,,,3]<-round_5

Index_lyri_word4<-c(rep(1,2),rep(2,3), rep(3,6), NA)

###################################################
#Fit the models

ws_e4n3_w1<-nbdaData(label="diff_ws_e4n3_w1",assMatrix=ws_e4n3_w1,orderAcq = oa_ws_e4n3_word1,demons = demons, assMatrixIndex = Index_e4n3_word1)
ws_e4n3_w4<-nbdaData(label="diff_ws_e4n3_w4",assMatrix=ws_e4n3_w4,orderAcq = oa_ws_e4n3_word4,demons = demons, assMatrixIndex = Index_e4n3_word4)

ws_lyri_w1<-nbdaData(label="diff_ws_lyri_w1",assMatrix=ws_lyri_w1,orderAcq = oa_ws_lyri_word1,demons = demons, assMatrixIndex = Index_lyri_word1)
ws_lyri_w4<-nbdaData(label="diff_ws_lyri_w4",assMatrix=ws_lyri_w4,orderAcq = oa_ws_lyri_word4,demons = demons, assMatrixIndex = Index_lyri_word4)

#data.frame(Variable=model_dynamic@varNames,MLE=model_dynamic@outputPar,SE=model_dynamic@se)
multiDiffModel1<-oadaFit(list(ws_e4n3_w1, ws_e4n3_w4, ws_lyri_w1, ws_lyri_w4))
multiDiffModel2<-oadaFit(list(ws_e4n3_w1, ws_e4n3_w4, ws_lyri_w1, ws_lyri_w4), type= "asocial")

data.frame(Variable=multiDiffModel1@varNames,MLE=multiDiffModel1@outputPar,SE=multiDiffModel1@se)


###############################################################Random24 NETWORK###########################################################################

# Create and store adjacency matrices for each round as separate variables
round_1 <-as.matrix(read.csv(file="random_matrix_round_1.csv"))
round_2 <-as.matrix(read.csv(file="random_matrix_round_2.csv"))
round_3 <-as.matrix(read.csv(file="random_matrix_round_3.csv"))
round_4 <-as.matrix(read.csv(file="random_matrix_round_4.csv"))
round_5 <-as.matrix(read.csv(file="random_matrix_round_5.csv"))
round_6 <-as.matrix(read.csv(file="random_matrix_round_6.csv"))
round_7 <-as.matrix(read.csv(file="random_matrix_round_7.csv"))
round_8 <-as.matrix(read.csv(file="random_matrix_round_8.csv"))

#########################################################
#Create a demons vector full of zeroes
demons<-rep(0,24)
#and slot in 1s for 20, 21, 22, 23 and 24
demons[20]<-demons[21]<-demons[22]<-demons[23]<-demons[24]<-1
demons

#########################################################
#Acquisition orders for each word in the networks
oa_random_fq1g_word1<- c(5, 16, 1, 7, 12, 13, 10, 11, 15, 3, 9, 14, 17, 6, NA, NA, NA, NA, NA)
oa_random_fq1g_word4<- c(1, 4, 2, 13, 7, 12, 15, 16, 11, 3, 6, 9, 14, 17, NA, NA, NA, NA, NA)

oa_random_pqes_word1<- c(1, 4, 10, 3, 8, 13, 14, 2, 9, 15, 18, 19, 11, 6, 12, NA, NA, NA, NA)
oa_random_pqes_word4<- c(5, 10, 14, 18, 16, 2, 9, 15, 19, 3, 6, 11, 8, 13, 12, NA, NA, NA, NA)

#######################################################################
#########################Random24 KEYWORD 1############################
#######################################################################

#Create the empty array
random_fq1g_w1<-array(NA,dim=c(24,24,1,5))
#Slot in the network for each time period
random_fq1g_w1[,,,1]<-round_3
random_fq1g_w1[,,,2]<-round_4
random_fq1g_w1[,,,3]<-round_5
random_fq1g_w1[,,,4]<-round_6
random_fq1g_w1[,,,5]<-round_7

Index_fq1g_word1<-c(rep(1,2),rep(2,4), rep(3,3), rep(4,4), rep(5,1), NA)


#Create the empty array
random_pqes_w1<-array(NA,dim=c(24,24,1,7))
#Slot in the network for each time period
random_pqes_w1[,,,1]<-round_2
random_pqes_w1[,,,2]<-round_3
random_pqes_w1[,,,3]<-round_4
random_pqes_w1[,,,4]<-round_5
random_pqes_w1[,,,5]<-round_6
random_pqes_w1[,,,6]<-round_7
random_pqes_w1[,,,7]<-round_8

Index_pqes_word1<-c(rep(1,2),rep(2,1), rep(3,4), rep(4,5), rep(5,1), rep(6,1), rep(7,1), NA)

#######################################################################
#########################Random24 KEYWORD 4############################
#######################################################################
  
#Create the empty array
random_fq1g_w4<-array(NA,dim=c(24,24,1,5))
#Slot in the network for each time period
random_fq1g_w4[,,,1]<-round_2
random_fq1g_w4[,,,2]<-round_3
random_fq1g_w4[,,,3]<-round_4
random_fq1g_w4[,,,4]<-round_5
random_fq1g_w4[,,,5]<-round_6

Index_fq1g_word4<-c(rep(1,2),rep(2,2), rep(3,4), rep(4,1), rep(5,5), NA)  

#Create the empty array
random_pqes_w4<-array(NA,dim=c(24,24,1,6))
#Slot in the network for each time period
random_pqes_w4[,,,1]<-round_2
random_pqes_w4[,,,2]<-round_3
random_pqes_w4[,,,3]<-round_5
random_pqes_w4[,,,4]<-round_6
random_pqes_w4[,,,5]<-round_7
random_pqes_w4[,,,6]<-round_8

Index_pqes_word4<-c(rep(1,4),rep(2,1), rep(3,4), rep(4,3), rep(5,2), rep(6,1), NA)

###################################################
#Fit the models

random_fq1g_w1<-nbdaData(label="diff_random_fq1g_w1",assMatrix=random_fq1g_w1,orderAcq = oa_random_fq1g_word1,demons = demons, assMatrixIndex = Index_fq1g_word1)
random_fq1g_w4<-nbdaData(label="diff_random_fq1g_w4",assMatrix=random_fq1g_w4,orderAcq = oa_random_fq1g_word4,demons = demons, assMatrixIndex = Index_fq1g_word4)

random_pqes_w1<-nbdaData(label="diff_random_pqes_w1",assMatrix=random_pqes_w1,orderAcq = oa_random_pqes_word1,demons = demons, assMatrixIndex = Index_pqes_word1)
random_pqes_w4<-nbdaData(label="diff_random_pqes_w4",assMatrix=random_pqes_w4,orderAcq = oa_random_pqes_word4,demons = demons, assMatrixIndex = Index_pqes_word4)

#data.frame(Variable=model_dynamic@varNames,MLE=model_dynamic@outputPar,SE=model_dynamic@se)
multiDiffModel1<-oadaFit(list(random_fq1g_w1, random_fq1g_w4, random_pqes_w1, random_pqes_w4))
multiDiffModel2<-oadaFit(list(random_fq1g_w1, random_fq1g_w4, random_pqes_w1, random_pqes_w4), type= "asocial")

data.frame(Variable=multiDiffModel1@varNames,MLE=multiDiffModel1@outputPar,SE=multiDiffModel1@se)
