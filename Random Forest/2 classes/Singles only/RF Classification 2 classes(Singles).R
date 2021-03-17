################################################################################
#This program is to produce the performance of  
#Random Forests model
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

#Now take the selected features only from the ranklists
cutoff = 9

ranklist<-ranklist[1:cutoff]
length(ranklist)
ranklist
as.matrix(ranklist)


#import the dataset
BIG.A<-read.csv("MBD_CLASS(Singles).csv")

#Remove records with zeroes
index<-rowSums(BIG.A[,ranklist] != 0 & BIG.A[,ranklist] != 1) != 0

Zero_rows<-which(index == FALSE)
Zero_rows
BIG.A[427,ranklist]
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
y<-newBIG$CLASS
summary(y)
nrow(y)


#import test data
testing<-read.csv("MBD_CLASS(Singles)_test.csv")
test_data<-subset(testing,select = -CLASS) #This is the last 1/3rd of data
test_data<-test_data[,ranklist]
test_label<-testing$CLASS

summary(testing)
nrow(testing)
head(test_data,3)
test_label


#Construct the MCC vs Feature subset plot by removing the last 
#feature after each iteration
#This loop continues until all the features are exhausted from
#the current feature set, which is, x


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
OptimumTrees<-1000
cat("\nOptimumTrees", OptimumTrees)


#training the random forest
RFModel <- randomForest(training_data, training_label, ntree=OptimumTrees, proximity = FALSE, importance=TRUE)
cat("\nRFModel: ")
print(RFModel)
plot(RFModel)

#Now Finally Predict the test Data
pred<-predict(RFModel,test_data)


#obtain the confusion matrix
final_conMatrix<-confusionMatrix(pred,test_label, positive = "human")
print(final_conMatrix)
final_conMatrix$byClass


final_conMatrix<-confusionMatrix(pred,test_label, positive = "nonhuman")
print(final_conMatrix)
final_conMatrix$byClass


final_MCC<-mccr(as.numeric(test_label)-1, as.numeric(pred)-1)
cat("MCC = ", final_MCC)

cat("\n========================================================================")



