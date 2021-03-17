
#This program implements multiclass SVM-RFE

#This program is for Feature eliminataion using Microbiological dataset using the first 30 features.

#The dataset has 4 classes in the target variable

#Install Packages
install.packages("e1071")
install.packages("kernlab")



#Initiate the Library and data import
library(e1071)
library(kernlab)
library(writexl)

setwd("D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\SVM_RFE implementation\\4 class\\Singles")


BIG.A<-read.csv("MBD_TARGETtype(Singles).csv")
BIG.A
nrow(BIG.A)
summary(BIG.A)

#Initiate the start time
ptm<-proc.time()

############################################################

SVM_RFE <- function(x,y){
      n = ncol(x)
      
      survivingFeaturesIndices = c(1:n)
      featureRankedList = vector(length=n)
      rankedFeatureIndex = n
      
      
      while(length(survivingFeaturesIndices)>0)
      {
            #Get the Best Gamma Value for SVM
            best_GammaValues<-sigest(y~., data = x)
            bestGamma<-mean(c(best_GammaValues[1],best_GammaValues[3]))
            cat(best_GammaValues)
            cat("\n best Gamma = ",bestGamma)
            
            #training the support vector machine
            svmModel = svm(y~.,as.matrix(x[,survivingFeaturesIndices]), y, cost = 1, cachesize=500,
                           gamma=bestGamma, scale=F, type="C-classification", kernel="radial" )
            
            print(svmModel)
            
            #compute the weight vector using the function computeWeights
            Weights = computeWeights(svmModel)  #FYI Weights is a 2D Matrix
            
            #compute ranking criteria
            Weights = Weights * Weights
            
            rankingCriteria = 0
            
            #calculate the mean of weights obtained from all SVMs for a particular feature 
            for(i in 1:ncol(Weights))rankingCriteria[i] = mean(Weights[,i])
            
            #rank the features
            ranking = sort(rankingCriteria, index.return = TRUE)$ix
            
            #update feature ranked list
            (featureRankedList[rankedFeatureIndex] = survivingFeaturesIndices[ranking[1]])
            rankedFeatureIndex = rankedFeatureIndex - 1
            
            #eliminate the feature with smallest ranking criterion. i.e. Least relevant is removed from further Execution
            survivingFeaturesIndices <- survivingFeaturesIndices[-ranking[1]]
            
            cat("\ncurrent FeatureRankedList:", featureRankedList)
            cat("\n======================================================\n")
      }
      
      return (featureRankedList)
}

computeWeights<-function(model){
      #w=0
      if(model$nclasses==2){
            w=t(model$coefs)%*%model$SV
      }      
      else{ #when we deal with OVO svm classification
            ## compute start-index 
            start <- c(1, cumsum(model$nSV)+1)
            start <- start[-length(start)]
            
            calcw <- function (i,j) {
                  ## ranges for class i and j:
                  ri <- start[i] : (start[i] + model$nSV[i] - 1)
                  rj <- start[j] : (start[j] + model$nSV[j] - 1)
                  
                  ## coefs for (i,j):
                  coef1 <- model$coefs[ri, j-1]
                  coef2 <- model$coefs[rj, i]
                  
                  
                  ## return w values:
                  w=t(coef1)%*%model$SV[ri,]+t(coef2)%*%model$SV[rj,]
                  return(w)
            }
            
            W=NULL
            for (i in 1 : (model$nclasses - 1)){
                  for (j in (i + 1) : model$nclasses){
                        
                        wi=calcw(i,j)
                        #wi contains the weights of SVM constructed by Class i and Class j
                        W=rbind(W,wi)
                  }
            }
            w=W
      }
      
      return(w)
} 



final_list <-SVM_RFE(BIG.A[1:30], BIG.A$TARGETtype)

final_list

#Calculate total time of execution
proc.time()-ptm

#Write to File

head(BIG.A[final_list],0)

ranked_features<-head(BIG.A[final_list],0)

Feature_names<-colnames(ranked_features)
Feature_names
Feature_names<-data.frame(Feature_names)
Feature_names



write_xlsx(Feature_names, path = "D:\\ENGINEERING\\SEMESTER-8\\Project Work\\Experiment\\SVM_RFE implementation\\4 class\\Singles\\RankList using SVM_RFE-4 classes(Singles).xlsx",
           col_names = FALSE)

