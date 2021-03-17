################################################################################
#This program is to plot the graph between RF MCC and subset of features.
#The rank list considered is 4-Class Singles
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
setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\Random Forests\\4 class\\Singles")

#import ranklist
ranklist<-read_excel("RankList using RF_RFE-4 classes(Singles).xlsx", 
                     range=cell_cols("A"), col_names = FALSE)
ranklist<-as.matrix(ranklist)

#import the dataset
BIG.A<-read.csv("MBD_TARGETtype(Singles).csv")
x<-subset(BIG.A,select = -TARGETtype)
summary(x)
nrow(x)

#Store the class labels in one variable
y<-BIG.A$TARGETtype
y

#import test data
testing<-read.csv("MBD_TARGETtype(Singles)_test.csv")
test_data<-subset(testing,select = -TARGETtype) #This is the last 1/3rd of data

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
                        cat("\n numer = ",numer," for k = ", k, " l = ", l, " m = ", m)
                        
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
                  cat("\n sum1 = ",sum1," for k = ", k, " l = ", l)
                  
                  sum3<-sum3+cm[l,k]
                  cat("\n sum3 = ",sum3," for k = ", k, " l = ", l)
                  
            }
            
            for(k_ in 1:numclasses)
            {
                  if(k_!=k)
                  {
                        for(l_ in 1:numclasses)
                        {
                              sum2<-sum2+cm[k_,l_]
                              cat("\n sum2 = ",sum2," for k = ", k, " for k_ = ", k_, " l_ = ", l_)
                              
                              sum4<-sum4+cm[l_,k_]
                              cat("\n sum4 = ",sum4," for k = ", k, " for k_ = ", k_, " l_ = ", l_)
                        }
                  }
            }
            
            grandsum1<-grandsum1+sum1*sum2
            cat("\ngrandsum1 = ",grandsum1)  
            
            sum1<-0
            sum2<-0
            
            grandsum2<-grandsum2+sum3*sum4
            cat("\ngrandsum2 = ",grandsum2)      
            
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
# the current feature set, which is, x

MatthewsCC<-vector()


#Initiate the start time
ptm<-proc.time()



while(length(ranklist))
{
      #partitioning of data
      training_data<-x[1:6629,]   # First 2/3rd of data
      training_label<-y[1:6629]
      
      nrow(training_data)
      nrow(test_data)
      
      
      #In the start of every iteration
      #Need to select the specific columns as per the ranklist
      training_data<-subset(training_data, select = ranklist)
      head(training_data,2)
      
      test_data<-subset(test_data, select = ranklist)
      head(test_data,2)
      
      cat("\n Now length(training_data): ",length(training_data))
      
      #We iterate through all numTrees values to get the best parameters
      numTrees<-round(10^seq(1,3,by=0.2))
      numTrees
      
      errorValues<-vector()
      print("Going to find the optimal trees for this subset.")
      
      #Iterate over every numTrees to find least OOB value
      for(nt in numTrees)
      {
            # print(nt)
            rf_Model_temp<-randomForest(training_data, training_label, ntree=nt, proximity = FALSE) #by default mtry=sqrt(Number of variables)
            #rf_Model_temp$err.rate
            
            #collect the error Values  
            errorValues<-c(errorValues,rf_Model_temp$err.rate[nt,1])
            
      }
      
      cat("\nerrorValues", errorValues)
      
      write("\nerrorValues", file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      write(as.table(errorValues), file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      
      #Find the number of trees of the least OOB error 
      OptimumTrees<-numTrees[as.integer(which.min(errorValues))] #which.min returns the index of the first min value
      cat("\nOptimumTrees", OptimumTrees)
      
      write("\nOptimumTrees:", file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      write(as.table(OptimumTrees), file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      
      
      #training the support vector machine
      RFModel <- randomForest(training_data, training_label, ntree=OptimumTrees, proximity = FALSE, importance=TRUE)
      cat("\nRFModel: ")
      print(RFModel)
      #plot(RFModel)
      
      #Now Finally Predict the test Data
      pred<-predict(RFModel,test_data)
      
      #obtain the confusion matrix
      final_conMatrix<-confusionMatrix(pred,test_label)
      print(final_conMatrix)
      final_MCC<-computeMCC(final_conMatrix)
      
      write("\nFinal Confusion Matrix:", file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      write(as.table(final_conMatrix), file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      
      
      #compute MCC and Append every MCC value in this vector
      MatthewsCC<-c(MatthewsCC,final_MCC)
      cat("\nMatthewsCC = ",MatthewsCC)
      
      #remove the last element from the ranklist
      ranklist<-ranklist[-(length(ranklist))]
      cat("\n length of ranklist = ",length(ranklist))
      print(ranklist)
      
      write("\n========================================================================",
            file = "RF MCC vs Feature subset 4-class Execution(Singles).txt",append=TRUE)
      
 }

#Calculate total time of execution of the loop
proc.time()-ptm


#Final list of MCCs
MatthewsCC

#Write the results to a file
write(MatthewsCC, file = "RF Feature subset MCCs for 4 classes(Singles).txt")
print("MCC List written to File!")

#Plot the Graph

Feature_subset_size<-c(30:1)
Feature_subset_size

pl<-ggplot(mapping = aes(y=MatthewsCC, x=Feature_subset_size))+ggtitle("RF MCC vs Feature subset plot for 4 classes(Singles)")+
      geom_line()+
      geom_point()+scale_x_reverse()
pl

