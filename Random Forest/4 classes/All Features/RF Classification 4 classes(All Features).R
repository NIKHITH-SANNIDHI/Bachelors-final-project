################################################################################
#This program is to produce the performance of  
#Random Forests model
#The rank list considered is 4-Class Singles+ratios
################################################################################

#install packages 
install.packages("randomForest")
install.packages("caret")
install.packages("mccr")
install.packages("e1071")

library(randomForest)
library(caret)
library(mccr)
library(e1071)
library(readxl)

#set the directory
setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\Random Forests\\4 class\\All Features")

#import ranklist
ranklist<-read_excel("RankList using RF_RFE-4 classes(All Features).xlsx", 
                     range=cell_cols("A"), col_names = FALSE)
ranklist<-as.matrix(ranklist)

#Now take the selected features only from the ranklists
cutoff = 10

ranklist<-ranklist[1:cutoff]
length(ranklist)
ranklist
as.matrix(ranklist)



#import the dataset
BIG.A<-read.csv("MBD_TARGETtype(All Features).csv")

#Remove records with zeroes
index<-rowSums(BIG.A[,ranklist] != 0 & BIG.A[,ranklist] != 1) != 0

Zero_rows<-which(index == FALSE)
Zero_rows
BIG.A[274,ranklist]
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
testing<-read.csv("MBD_TARGETtype(All Features)_test.csv")
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


#Construct the MCC vs Feature subset plot by removing the last 
#feature after each iteration
#This loop continues until all the features are exhausted from
# the current feature set


cat("subset size is ", length(ranklist))
cat("Feature Rank list: ")
print(as.matrix(ranklist))


      #partitioning of data
      training_data<-x   # First 2/3rd of data
      training_label<-y
      
      
      #In the start of every iteration
      #Need to select the specific columns as per the ranklist
      training_data<-subset(training_data, select = ranklist)
      
      nrow(training_data)
      
      test_data<-subset(test_data, select = ranklist)
      ncol(test_data)
      nrow(test_data)
      
      
      cat("\n Now length(training_data): ",length(training_data))
      
      #No need to iterate through all numTrees values to get the best parameters
      #we retrieved the best numtrees
      OptimumTrees<-251
      cat("\nOptimumTrees", OptimumTrees)
      
      
      #training the random forest
      RFModel <- randomForest(training_data, training_label, ntree=OptimumTrees, proximity = FALSE, importance=TRUE)
      cat("\nRFModel: ")
      print(RFModel)
      plot(RFModel)
      
      #Now Finally Predict the test Data
      pred<-predict(RFModel,test_data)
      
      #obtain the confusion matrix
      final_conMatrix<-confusionMatrix(pred,test_label)
      print(final_conMatrix)
      final_conMatrix$byClass
      
      
      final_MCC<-computeMCC(final_conMatrix)
      cat("MCC = ", final_MCC)
      
      cat("\n========================================================================")
      
  
      
  