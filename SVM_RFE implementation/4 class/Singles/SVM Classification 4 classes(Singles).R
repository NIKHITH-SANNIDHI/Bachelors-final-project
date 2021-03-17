################################################################################
#This program is to produce the performance of SVM 
#The rank list considered is 4-Class Singles
################################################################################

#install packages 
install.packages("xlsx")
install.packages("e1071")
install.packages("caret") #very latest - released in 2018
install.packages("readxl")
install.packages("kernlab")
library(e1071)
library(caret)
#library(xlsx)
library(readxl)
library(kernlab)
library(ggplot2)

#set the directory
setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\SVM_RFE implementation\\4 class\\Singles")

#import ranklist
ranklist<-read_excel("RankList using SVM_RFE-4 classes(Singles).xlsx", 
                     range=cell_cols("A"), col_names = FALSE)
ranklist<-as.matrix(ranklist)

ranklist
length(ranklist)

#Now take the selected features only from the ranklists
cutoff = 3+1
cutoff

ranklist<-ranklist[1:cutoff]
length(ranklist)
ranklist
as.matrix(ranklist)


#import the dataset
BIG.A<-read.csv("MBD_TARGETtype(Singles).csv")


#Remove records with zeroes
index<-rowSums(BIG.A[,ranklist] != 0 & BIG.A[,ranklist] != 1) != 0

index


Zero_rows<-which(index == FALSE)
Zero_rows
BIG.A[45,ranklist]
nrow(Zero_rows)

newBIG<-BIG.A[index,]

newBIG


#Number of zero rows
6629-nrow(newBIG)

nrow(newBIG)

summary(newBIG)

x<-newBIG[,ranklist]
x
summary(x)
nrow(x)


#Store the class labels in one variable
y<-newBIG$TARGETtype
summary(y)
nrow(y)

#import test data
testing<-read.csv("MBD_TARGETtype(Singles)_test.csv")
test_data<-subset(testing,select = -TARGETtype) #This is the last 1/3rd of data
test_data<-test_data[,ranklist]
test_label<-testing$TARGETtype

summary(testing)
nrow(testing)
head(test_data,3)
test_label



#x<-x[-(length(x)-1)]
#length(x)

numclasses<-nlevels(y)
numclasses

#Function for calculating MCC
computeMCC<-function(cm)
{
      cm<-as.table(cm)
      
      #Initiate the Numerator value to 0
      numer<-0
      
      #Compute the Numerator
      for (k in 1:numclasses) 
            for (l in 1:numclasses)
                  for (m in 1:numclasses)
                  {
                        numer<-numer+cm[k,k]*cm[l,m]-cm[k,l]*cm[m,k]
                        #cat("\n numer = ",numer," for k = ", k, " l = ", l, " m = ", m)
                        
                  }
      
      #Initialize values used in denominator
      sum1<-0
      sum2<-0
      sum3<-0
      sum4<-0
      grandsum1<-0
      grandsum2<-0
      
      # Compute the deonminator
      for (k in 1:numclasses) 
      {
            for (l in 1:numclasses)
            {
                  sum1<-sum1+cm[k,l]
                  #cat("\n sum1 = ",sum1," for k = ", k, " l = ", l)
                  
                  sum3<-sum3+cm[l,k]
                  #cat("\n sum3 = ",sum3," for k = ", k, " l = ", l)
                  
            }
            
            for(k_ in 1:numclasses)
            {
                  if(k_!=k)
                  {
                        for(l_ in 1:numclasses)
                        {
                              sum2<-sum2+cm[k_,l_]
                              #cat("\n sum2 = ",sum2," for k = ", k, " for k_ = ", k_, " l_ = ", l_)
                              
                              sum4<-sum4+cm[l_,k_]
                              #cat("\n sum4 = ",sum4," for k = ", k, " for k_ = ", k_, " l_ = ", l_)
                        }
                  }
            }
            
            grandsum1<-grandsum1+sum1*sum2
            #cat("\ngrandsum1 = ",grandsum1)  
            
            sum1<-0
            sum2<-0
            
            grandsum2<-grandsum2+sum3*sum4
            #cat("\ngrandsum2 = ",grandsum2)      
            
            sum3<-0
            sum4<-0
      }           
      
      mccMC<-numer/sqrt(grandsum1*grandsum2)
      
      cat("\nMcc  = ",mccMC)      
      
      return(mccMC)
}




cat("subset size is ", length(ranklist))
cat("Feature Rank list: ")
print(as.matrix(ranklist))

#only 1 iteration is required to produce the results


#NO NEED TO iterate through all cost values to get the best parameters
#We already retrieved Final the best Cost for every subset
best_cost<-1
cat("best_cost is ", best_cost)

#We take the combined data to compute final MCC
#for this feature subset
LE_data<-x #The first 2/3 rd of data
LE_label<-y

nrow(LE_data)
nrow(test_data)

#Need to select the specific columns as per the ranklist
LE_data<-subset(LE_data, select = ranklist)
ncol(LE_data)

#Get the Best Gamma Value for SVM from LE
best_GammaValues<-sigest(LE_label~., data = LE_data)
best_Gamma_for_LE<-mean(c(best_GammaValues[1],best_GammaValues[3]))

#We have Final the best Gamma 
cat("best_Gamma is ", best_Gamma_for_LE)

#Now compute the best model
best_svmfit<-svm(LE_label~., as.matrix(LE_data[,ranklist]),cost=best_cost, cachesize=500,
                 gamma = best_Gamma_for_LE,scale=FALSE, type="C-classification", kernel="radial")
print(best_svmfit)

#Now Finally Predict the test Data
final_pred<-predict(best_svmfit,test_data)

#obtain the confusion matrix
final_conMatrix<-confusionMatrix(final_pred,test_label)
print(final_conMatrix)
final_conMatrix$byClass

final_MCC<-computeMCC(final_conMatrix)

cat("MCC = ", final_MCC)

cat("\n========================================================================")
