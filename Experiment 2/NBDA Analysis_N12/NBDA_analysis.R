

library(NBDA)
library(igraph)
library(readxl)

#Create a demons vector full of zeroes
demons<-rep(0,12)
#and slot in 1s for 9, 10 and 11
demons[10]<-demons[11]<-demons[12]<-1
demons

oa_9cax<-c( 1, 2, 4, 5, 6, 3, 7, 8, 9)
oa_j8urh<-c(1, 2, 4, 5, 6, 3, 7, 8, 9)
oa_hg73<-c(1, 2, 8, 4, 6, 9, 3, 5, 7)
oa_slpk<-c(1, 2, 4, 5, 6, 3, 7, 8, 9)

#USING A DYNAMIC NETWORK IN OADA
#WHY? To create time periods for participants because we do not have exact acquisition times. 
#In addition, not all participants are playing at the same time

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


#Create the empty array
ws_9cax<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_9cax[,,,1]<-round_3
ws_9cax[,,,2]<-round_4
ws_9cax[,,,3]<-round_5
ws_9cax[,,,4]<-round_6

#Create the empty array
ws_hg73<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
ws_hg73[,,,1]<-round_3
ws_hg73[,,,2]<-round_4
ws_hg73[,,,3]<-round_5
ws_hg73[,,,4]<-round_6


#Create the empty array
ws_j8urh<-array(NA,dim=c(12,12,1,5))
#Slot in the network for each time period
ws_j8urh[,,,1]<-round_3
ws_j8urh[,,,2]<-round_4
ws_j8urh[,,,3]<-round_5
ws_j8urh[,,,4]<-round_6
ws_j8urh[,,,5]<-round_7

#Create the empty array
ws_slpk<-array(NA,dim=c(12,12,1,6))
#Slot in the network for each time period
ws_slpk[,,,1]<-round_3
ws_slpk[,,,2]<-round_4
ws_slpk[,,,3]<-round_5
ws_slpk[,,,4]<-round_6
ws_slpk[,,,5]<-round_7
ws_slpk[,,,6]<-round_9

#Now we need to create a vector specifying which time period corresponds to which acquisition event
#For example, in 9cax network, the first 2 events occured in the first period, the next 2 events occured in the second time period,
#the next 1 event occured in the third period and the last 4 events occured in the fourth time period
Index_9cax<-c(rep(1,2),rep(2,2), rep(3,1),rep(4,4))
Index_hg73<-c(rep(1,2),rep(2,1), rep(3,3),rep(4,3))
Index_j8urh<-c(rep(1,2),rep(2,2), rep(3,1),rep(4,2), rep(5,2))
Index_slpk<-c(rep(1,2),rep(2,2), rep(3,1),rep(4,2), rep(5,1),rep(6,1))

#Create an nbdaData object containing the data we need to fit an OADA model
#assMatrix stands for association matrix, since these are most commonly used, but this can be any type of social network
ws_9cax<-nbdaData(label="ExampleDiffusion1",assMatrix=ws_9cax,orderAcq = oa_9cax,demons = demons, assMatrixIndex = Index_9cax)
ws_hg73<-nbdaData(label="ExampleDiffusion2",assMatrix=ws_hg73,orderAcq = oa_hg73,demons = demons, assMatrixIndex =Index_hg73)
ws_j8urh<-nbdaData(label="ExampleDiffusion3",assMatrix=ws_j8urh,orderAcq = oa_j8urh,demons = demons, assMatrixIndex =Index_j8urh)
ws_slpk<-nbdaData(label="ExampleDiffusion4",assMatrix=ws_slpk,orderAcq = oa_slpk,demons = demons, assMatrixIndex =Index_slpk)


#Instead of specifying a single nbdaData object, we specify a list containing all of the diffusions we wish to include:
multiDiffModel1<-oadaFit(list(ws_9cax,ws_hg73, ws_j8urh, ws_slpk))
multiDiffModel2<-oadaFit(list(ws_9cax,ws_hg73, ws_j8urh, ws_slpk), type= "asocial")
data.frame(Variable=multiDiffModel1@varNames,MLE=multiDiffModel1@outputPar,SE=multiDiffModel1@se)

multiDiffModel1@aicc #89, lower AICc and therefore is a better fit
multiDiffModel2@aicc #102.4
#compare the social and asocial models
multiDiffModel2@aicc-multiDiffModel1@aicc #13.33

# SE looks large. Does this mean there is not good evidence for social transmission?
#Not necessarily, as we shall see.

#The value estimated for s might be difficult to interpret, depending on the network
#used. In such cases, we can obtain an estimate of the % of events that occured by social
#transmission as opposed to asocial learning (%ST) as follows:
nbdaPropSolveByST(model=multiDiffModel1)
#P(Network 1)  P(S offset) 
#0.55041      0.00000 
#This tells us that the estimated value for s corresponds to 55% (the function returns a 
#proportion so multiply by 100 to get %ST)
exp(0.5*(multiDiffModel2@aicc-multiDiffModel1@aicc))
#[1] 785
#the social model is 785x more likely to be the best K-L model, out of the two.
#Or we can say the social model has 785x more support than the asocial model.

#We can also conduct a likelihood ratio test (LRT) for social transmission
#The @loglik slot contains the -log-likelihood- i.e. minus the log-likelihood
#So we can get the test statistic as double the difference in -log-likelihood as follows:
2*(multiDiffModel2@loglik-multiDiffModel1@loglik)
#[1] 15
#There is 1 parameter in model_social, and 0 in model_asocial, so we have 1 d.f.
pchisq(2*(multiDiffModel2@loglik-multiDiffModel1@loglik),df=1,lower.tail=F)
#[1] 8.46904e-05 (p<0.05), Social model is  statistically different than asocial model.

##########################################
#MOVING ON TO RANDOM NETWORKS

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

#order of acq
oa_b948<-c(1, 9, 2, 4, 5, 8, 6, 7, 3)
oa_jdf4<-c(1, 9, 2, 4, 3, 5, 7, 8, 6)
oa_ot08<-c(2, 3, 5, 7, 8, 9, 4, 6, 1)
oa_sdth<-c(1, 9, 4, 2, 3, 5, 7, 8, 6)

#Create the empty array
random_b948<-array(NA,dim=c(12,12,1,5))
#Slot in the network for each time period
random_b948[,,,1]<-random_round_3
random_b948[,,,2]<-random_round_4
random_b948[,,,3]<-random_round_5
random_b948[,,,4]<-random_round_6
random_b948[,,,5]<-random_round_9

#Create the empty array
random_jdf4<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
random_jdf4[,,,1]<-random_round_3
random_jdf4[,,,2]<-random_round_4
random_jdf4[,,,3]<-random_round_5
random_jdf4[,,,4]<-random_round_7

#Create the empty array
random_sdth<-array(NA,dim=c(12,12,1,4))
#Slot in the network for each time period
random_sdth[,,,1]<-random_round_3
random_sdth[,,,2]<-random_round_4
random_sdth[,,,3]<-random_round_5
random_sdth[,,,4]<-random_round_6

#Create the empty array
random_ot08<-array(NA,dim=c(12,12,1,3))
#Slot in the network for each time period
random_ot08[,,,1]<-random_round_5
random_ot08[,,,2]<-random_round_6
random_ot08[,,,3]<-random_round_9

#Now we need to create a vector specifying which time period corresponds to which acquisition event
#For example, here, in ot08 network, the first 6 events occured in the first period, the next 2 events occured in the second time period
#and the next 1 event occured in the third time period
Index_b948<-c(rep(1,2),rep(2,2), rep(3,2),rep(4,2), rep(5,1))
Index_jdf4<-c(rep(1,2),rep(2,2), rep(3,4),rep(4,1))
Index_ot08<-c(rep(1,6),rep(2,2), rep(3,1))
Index_sdth<-c(rep(1,2),rep(2,1), rep(3,5),rep(4,1))

random_b948<-nbdaData(label="Diffusion_random_b948",assMatrix=random_b948,orderAcq = oa_b948,demons = demons, assMatrixIndex =Index_b948)
random_jdf4<-nbdaData(label="Diffusion_random_jdf4",assMatrix=random_jdf4,orderAcq = oa_jdf4,demons = demons, assMatrixIndex =Index_jdf4)
random_ot08<-nbdaData(label="Diffusion_random_ot08",assMatrix=random_ot08,orderAcq = oa_ot08,demons = demons, assMatrixIndex =Index_ot08)
random_sdth<-nbdaData(label="Diffusion_random_sdth",assMatrix=random_sdth,orderAcq = oa_sdth,demons = demons, assMatrixIndex =Index_sdth)

multiDiff_random<-oadaFit(list(random_b948,random_jdf4, random_ot08, random_sdth))
multiDiff_random_asoc<-oadaFit(list(random_b948,random_jdf4, random_ot08, random_sdth), type= "asocial")
data.frame(Variable=multiDiff_random@varNames,MLE=multiDiff_random@outputPar,SE=multiDiff_random@se)

multiDiff_random@aicc #102.6, 
multiDiff_random_asoc@aicc #102.4 lower AICc and therefore is a better fit
#compare the social and asocial models
multiDiff_random_asoc@aicc-multiDiff_random@aicc #-0.22

# SE looks large. Does this mean there is not good evidence for social transmission?
#Not necessarily, as we shall see.

#The value estimated for s might be difficult to interpret, depending on the network
#used. In such cases, we can obtain an estimate of the % of events that occured by social
#transmission as opposed to asocial learning (%ST) as follows:
nbdaPropSolveByST(model=multiDiff_random)
#P(Network 1)  P(S offset) 
#0.3165      0.00000
#This tells us that the estimated value for s corresponds to 18.3% (the function returns a 
#proportion so multiply by 100 to get %ST)
exp(0.5*(multiDiff_random_asoc@aicc-multiDiff_random@aicc))
#[1] 0.89

#We can also conduct a likelihood ratio test (LRT) for social transmission
#The @loglik slot contains the -log-likelihood- i.e. minus the log-likelihood
#So we can get the test statistic as double the difference in -log-likelihood as follows:
2*(multiDiff_random_asoc@loglik-multiDiff_random@loglik)
#[1] 1.89
#There is 1 parameter in model_social, and 0 in model_asocial, so we have 1 d.f.
pchisq(2*(multiDiff_random_asoc@loglik-multiDiff_random@loglik),df=1,lower.tail=F)
#[1] 0.16 (p>0.05), Social model is not statistically different than asocial model.


################
oa_ws_9cax_word1<- c(1, 2, 4, 6, 8, 9, 7, 3, 5)
oa_ws_9cax_word2<- c(1, 2, 4, 5, 6, 3, 9, 7, 8)
oa_ws_9cax_word3<-
oa_ws_9cax_word4<-
oa_ws_9cax_word5<-

oa_ws_hg73_word1<- c(1, 2, 3, 4, 6, 9, 5, 7, 8) 
oa_ws_hg73_word2<- c(1, 2, 4, 6, 3, 5, 7, 9, 8)
oa_ws_hg73_word3<-
oa_ws_hg73_word4<-
oa_ws_hg73_word5<-
  
oa_ws_j8urh_word1<- c(1, 2, 4, 6, 7, 9, 3, 5, 8)
oa_ws_j8urh_word2<-
oa_ws_j8urh_word3<-
oa_ws_j8urh_word4<-
oa_ws_j8urh_word5<-
  
oa_ws_slpk_word1<- c(1, 2, 4, 5, 6, 7, 8, 9, 3)
oa_ws_slpk_word2<-
oa_ws_slpk_word3<-
oa_ws_slpk_word4<-
oa_ws_slpk_word5<-
