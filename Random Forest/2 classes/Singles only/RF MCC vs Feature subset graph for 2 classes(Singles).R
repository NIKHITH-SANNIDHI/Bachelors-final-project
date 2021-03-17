################################################################################
#This program is to plot the graph between MCC and subset of features.
#The rank list considered is 2-Class Singles
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
setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\Random Forests\\2 class\\Singles")

#import ranklist
ranklist<-read_excel("RankList using RF_RFE-2 classes(Singles).xlsx", 
                     range=cell_cols("A"), col_names = FALSE)
ranklist<-as.matrix(ranklist)

#import the dataset
BIG.A<-read.csv("MBD_CLASS(Singles).csv")
x<-subset(BIG.A,select = -CLASS)
summary(x)

#Store the class labels in one variable
y<-BIG.A$CLASS
y

#import test data
testing<-read.csv("MBD_CLASS(Singles)_test.csv")
test_data<-subset(testing,select = -CLASS) #This is the last 1/3rd of data

test_label<-testing$CLASS

summary(testing)
nrow(testing)
head(test_data,3)
test_label


#Construct the MCC vs Feature subset plot by removing the last 
#feature after each iteration
#This loop continues until all the features are exhausted from
#the current feature set, which is, x

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
      
      cat("\n Now length(training_data)",length(training_data))
      
      
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
      
      write("\nerrorValues", file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      write(as.table(errorValues), file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
      #Find the number of trees of the least OOB error 
      OptimumTrees<-numTrees[as.integer(which.min(errorValues))] #which.min returns the index of the first min value
      cat("\nOptimumTrees", OptimumTrees)
      
      write("\nOptimumTrees:", file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      write(as.table(OptimumTrees), file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
      
      #training the random forest
      RFModel <- randomForest(training_data, training_label, ntree=OptimumTrees, proximity = FALSE, importance=TRUE)
      cat("\nRFModel: ")
      print(RFModel)
      #plot(RFModel)
      
      #Now Finally Predict the test Data
      pred<-predict(RFModel,test_data)
      
      #obtain the confusion matrix
      final_conMatrix<-confusionMatrix(pred,test_label)
      print(final_conMatrix)
      final_MCC<-mccr(as.numeric(test_label)-1, as.numeric(pred)-1)
      
      
      write("\nFinal Confusion Matrix:", file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      write(as.table(final_conMatrix), file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
      
      #compute MCC and Append every MCC value in this vector
      MatthewsCC<-c(MatthewsCC,final_MCC)
      cat("\nMatthewsCC = ",MatthewsCC)
      
      #remove the last element from the ranklist
      ranklist<-ranklist[-(length(ranklist))]
      cat("\n length of ranklist = ",length(ranklist))
      print(ranklist)
      
      write("\n========================================================================",
            file = "RF MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
}

#Calculate total time of execution of the loop
proc.time()-ptm


#Final list of MCCs
MatthewsCC

#Write the results to a file
write(MatthewsCC, file = "RF Feature subset MCCs for 2 classes(Singles).txt")
print("MCC List written to File!")

#Plot the Graph

Feature_subset_size<-c(30:1)
Feature_subset_size

pl<-ggplot(mapping = aes(y=MatthewsCC, x=Feature_subset_size))+ggtitle("RF MCC vs Feature subset plot for 2 classes(Singles)")+
      geom_line()+
      geom_point()+scale_x_reverse()
pl

