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

#Now take the selected features only from the ranklists
cutoff = 16+1

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
BIG.A[271,ranklist]
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
nrow(test_data)

test_data<-test_data[,ranklist]
nrow(test_data)

test_label<-testing$CLASS

nrow(test_data)

summary(testing)
nrow(testing)
head(test_data,3)
test_label


#Construct the MCC vs Feature subset plot by removing the last 
#feature after each iteration
#This loop continues until all the features are exhausted from
#the current feature set, which is, x

MatthewsCC<-vector()


cat("subset size is ", length(ranklist))
cat("Feature Rank list: ")
print(as.matrix(ranklist))


#only 1 iteration is required to produce the results

#NO NEED TO iterate through all cost values to get the best parameters
#We already retrieved Final the best Cost for every subset
best_cost<-20
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
nrow(LE_data)

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
final_conMatrix<-confusionMatrix(final_pred, test_label, positive = "human")
print(final_conMatrix)
final_conMatrix$byClass


final_conMatrix<-confusionMatrix(final_pred, test_label, positive = "nonhuman")
print(final_conMatrix)
final_conMatrix$byClass



#compute MCC and Append every MCC value in this vector
final_MCC<-mccr(as.numeric(test_label)-1, as.numeric(final_pred)-1)
print("\nMCC for this subset: ")
print(as.numeric(final_MCC))

cat("\n========================================================================")


