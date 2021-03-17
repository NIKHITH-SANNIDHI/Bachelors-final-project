
#This program implements Binary class RF-RFE

#This program is for Feature eliminataion using Microbiological dataset using all the features.

#The dataset has 2 classes in the target variable

#Install Packages
install.packages("randomForest")
install.packages("writexl")


#Initiate the Library and data import
library(randomForest)
library(writexl)

setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\Random Forests\\2 class\\All Features")


BIG.A<-read.csv("MBD_CLASS(All Features).csv")
BIG.A
nrow(BIG.A)
summary(BIG.A)

#Initiate the start time
ptm<-proc.time()

############################################################

RF_RFE <- function(x,y){
      n = ncol(x)
      
      survivingFeaturesIndices = c(1:n)
      featureRankedList = vector(length=n)
      rankedFeatureIndex = n
      
      
      while(length(survivingFeaturesIndices)>1)
      {
            #First find the apt number of trees
            numTrees<-round(10^seq(1,3,by=0.2))
            numTrees
            
            errorValues<-vector()
            print("Going to find the optimal trees for this subset.")
            
            #Iterate over every numTrees to find least OOB value
            for(nt in numTrees)
            {
                 # print(nt)
                  rf_Model_temp<-randomForest(x[,survivingFeaturesIndices], y, ntree=nt, proximity = FALSE) #by default mtry=sqrt(Number of variables)
                  #rf_Model_temp$err.rate
                  
                  #collect the error Values  
                  errorValues<-c(errorValues,rf_Model_temp$err.rate[nt,1])
                  
            }
            
            cat("\nerrorValues", errorValues)
            
            #Find the number of trees of the least OOB error 
            OptimumTrees<-numTrees[as.integer(which.min(errorValues))] #which.min returns the index of the first min value
            cat("\nOptimumTrees", OptimumTrees)
            
            
            #training the support vector machine
            RFModel <- randomForest(x[,survivingFeaturesIndices], y, ntree=OptimumTrees, proximity = FALSE, importance=TRUE)
            cat("\nRFModel: ")
            print(RFModel)
            
            #Find the importance of the model
            imp<-importance(RFModel)
            
            #Take the MeanDecreaseGini Column
            MDG<-subset(imp, select = MeanDecreaseGini)
            cat("\nMDG: ")
            print(MDG)
            
            #rank the features
            ranking = sort(MDG, index.return = TRUE)$ix
            cat("\nranking: ", ranking)
            
            #update feature ranked list
            (featureRankedList[rankedFeatureIndex] = survivingFeaturesIndices[ranking[1]])
            rankedFeatureIndex = rankedFeatureIndex - 1
            
            #eliminate the feature with smallest ranking criterion. i.e. Least relevant is removed from further Execution
            survivingFeaturesIndices <- survivingFeaturesIndices[-ranking[1]]
            
            cat("\nsurvivingFeaturesIndices: ",survivingFeaturesIndices)
            cat("\ncurrent FeatureRankedList:", featureRankedList)
            cat("\n======================================================\n")
            
            if(length(survivingFeaturesIndices) == 1)
            {
                  print("inside if")
                  featureRankedList[rankedFeatureIndex] = survivingFeaturesIndices[1]
                  cat("\ncurrent FeatureRankedList:", featureRankedList)
                  
            }
      }
      
      return (featureRankedList)
}

final_list <-RF_RFE(BIG.A[1:45], BIG.A$CLASS)

final_list

#Calculate total time of execution
proc.time()-ptm


#Write to file

head(BIG.A[final_list],0)

ranked_features<-head(BIG.A[final_list],0)

Feature_names<-colnames(ranked_features)
Feature_names
Feature_names<-data.frame(Feature_names)
Feature_names



write_xlsx(Feature_names, path = "D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\Random Forests\\2 class\\All Features\\RankList using RF_RFE-2 classes(All Features).xlsx",
           col_names = FALSE)


