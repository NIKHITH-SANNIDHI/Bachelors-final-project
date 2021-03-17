################################################################################
#This program is to plot the graph between MCC and subset of features.
#The rank list considered is 2-Class Singles
################################################################################

#install packages 
install.packages("e1071")
install.packages("caret") #very latest - released in 2018
install.packages("mccr")
install.packages("kernlab")
library(mccr)
library(e1071)
library(caret)
library(readxl)
library(kernlab)
library(ggplot2)

#set the directory
setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\SVM_RFE implementation\\2 class\\Singles")

#import ranklist
ranklist<-read_excel("RankList using SVM_RFE-2 classes(Singles).xlsx", 
                     range=cell_cols("A"), col_names = FALSE)
ranklist<-as.matrix(ranklist)

ranklist
length(ranklist)

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
      training_data<-x[1:3314,]   # First 1/3rd of data
      training_label<-y[1:3314]
      
      validation_data<-x[3315:6629,] #Second 1/3rd of Data
      validation_label<-y[3315:6629]
      
      nrow(training_data)
      nrow(validation_data)
      nrow(test_data)
      
      #In the start of every iteration
      #Need to select the specific columns as per the ranklist
      training_data<-subset(training_data, select = ranklist)
      validation_data<-subset(validation_data,select = ranklist)
      test_data<-subset(test_data, select = ranklist)
      
      head(training_data,2)
      cat("\n Now length(training_data)",length(training_data))
      
      #We iterate through all cost values to get the best parameters
      costValues<-c(0.1,1,5,20)
      best_MCC=-1000
      print(best_MCC)
      
      for (C in costValues) 
      {
            cat("\nCurrent cost value: ",C)
            
            #First Validation
            cat("\nFIRST Validation")
            cat("\n===========\n")
            
            #Get the Best Gamma Value for SVM
            best_GammaValues<-sigest(training_label~., data = training_data)
            bestGamma<-mean(c(best_GammaValues[1],best_GammaValues[3]))
            cat(best_GammaValues)
            cat("\n best Gamma = ",bestGamma)
            
            #Train the SVM
            svmfit<-svm(training_label~., as.matrix(training_data[,ranklist]),cost=C, cachesize=500,
                        gamma = bestGamma,scale=FALSE, type="C-classification", kernel="radial")
            print(svmfit)
            cat("\n svmfit computed!")
            
            #Predict the validation Data
            pred_validation<-predict(svmfit,validation_data)
            
            #obtain the confusion matrix
            conMatrix<-confusionMatrix(pred_validation, validation_label)
            print(conMatrix)
            MCC1<-mccr(as.numeric(validation_label)-1, as.numeric(pred_validation)-1)
            
            write("\nMCC1:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
            write(as.numeric(MCC1), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
            
            
            
            
            ########################################################################################################
            # Now swap Training and Validation sets
            
            temp_data<-validation_data
            temp_label<-validation_label
            
            validation_data<-training_data   # First 1/3rd of data
            validation_label<-training_label
            
            training_data<-temp_data   #Second 1/3rd of Data
            training_label<-temp_label
            
            nrow(training_data)
            nrow(validation_data)
            ########################################################################################################
            
            #Second Validation
            cat("\nSECOND Validation")
            cat("\n===========\n")
            
            #Get the Best Gamma Value for SVM
            best_GammaValues<-sigest(training_label~., data = training_data)
            bestGamma<-mean(c(best_GammaValues[1],best_GammaValues[3]))
            cat(best_GammaValues)
            cat("\n best Gamma = ",bestGamma)
            
            #Train the SVM
            svmfit<-svm(training_label~., as.matrix(training_data[,ranklist]),cost=C, cachesize=500,
                        gamma = bestGamma,scale=FALSE, type="C-classification", kernel="radial")
            print(svmfit)
            cat("\n svmfit computed!")
            
            #Predict the validation Data
            pred_validation<-predict(svmfit,validation_data)
            
            #obtain the confusion matrix
            conMatrix<-confusionMatrix(pred_validation, validation_label)
            print(conMatrix)
            MCC2<-mccr(as.numeric(validation_label)-1, as.numeric(pred_validation)-1)
            
            write("\nMCC2:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
            write(as.numeric(MCC2), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
            ########################################################################################################
            
            #Now average the MCCs
            
            MCCavg<-mean(c(MCC1,MCC2))
            cat("\nMCCavg",MCCavg)
            
            if(MCCavg > best_MCC)
            {
                  best_cost<-C
                  best_MCC<-MCCavg
                  cat("\nnow best cost is: ",best_cost)
                  cat("\nnow best MCC is: ",best_MCC)
                  
                  write("\nnow best cost is:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
                  write(as.numeric(best_cost), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
                  
                  write("\nnow best MCC is:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
                  write(as.numeric(best_MCC), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
                  
            }
      }
      
      #We have Final the best Cost 
      print(best_cost)
      write("\nFinal best_cost:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      write(as.numeric(best_cost), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
      #Lets have the combined data to compute final MCC
      #for this feature subset
      LE_data<-x[1:6629,] #The first 2/3 rd of data
      LE_label<-y[1:6629]
      
      nrow(LE_data)
      nrow(test_data)
      
      #Need to select the specific columns as per the ranklist
      LE_data<-subset(LE_data, select = ranklist)
      ncol(LE_data)
      
      #Get the Best Gamma Value for SVM from LE
      best_GammaValues<-sigest(LE_label~., data = LE_data)
      best_Gamma_for_LE<-mean(c(best_GammaValues[1],best_GammaValues[3]))
      
      #We have Final the best Gamma 
      print(best_Gamma_for_LE)
      write("\nbest_Gamma_for_LE:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      write(as.numeric(best_Gamma_for_LE), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
      
      #Now compute the best model
      best_svmfit<-svm(LE_label~., as.matrix(LE_data[,ranklist]),cost=best_cost, cachesize=500,
                       gamma = best_Gamma_for_LE,scale=FALSE, type="C-classification", kernel="radial")
      print(best_svmfit)
      cat("\n best_svmfit computed!")
      
      #Now Finally Predict the test Data
      final_pred<-predict(best_svmfit,test_data)
      
      #obtain the confusion matrix
      final_conMatrix<-confusionMatrix(final_pred, test_label)
      print(final_conMatrix)
      
      write("\nFinal Confusion Matrix:", file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      write(as.table(final_conMatrix), file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
      
      
      #compute MCC and Append every MCC value in this vector
      final_MCC<-mccr(as.numeric(test_label)-1, as.numeric(final_pred)-1)
      print("\nMCC for this subset: ")
      print(as.numeric(final_MCC))
      
      
      MatthewsCC<-c(MatthewsCC,final_MCC)
      cat("\nMatthewsCC = ",MatthewsCC)
      
      #remove the last element from the ranklist
      ranklist<-ranklist[-(length(ranklist))]
      cat("\n length of ranklist = ",length(ranklist))
      print(ranklist)
      
      write("\n========================================================================",
            file = "MCC vs Feature subset 2-class(Singles) Execution.txt",append=TRUE)
}

#Calculate total time of execution of the loop
proc.time()-ptm


#Final list of MCCs
MatthewsCC

#Write the results to a file
write(MatthewsCC, file = "SVM Feature subset MCCs for 2 classes(Singles).txt")
print("MCC List written to File!")

#Plot the Graph

Feature_subset_size<-c(30:2)
Feature_subset_size

pl<-ggplot(mapping = aes(y=MatthewsCC, x=Feature_subset_size))+ggtitle("SVM MCC vs Feature subset plot for 2 classes(Singles)")+
      geom_line()+
      geom_point()+scale_x_reverse()
pl

