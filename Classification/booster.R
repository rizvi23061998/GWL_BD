library(ROCR)
library(e1071)
library(randomForest)
library(DMwR)
library(dplyr)
library(caret)
library(caTools)
library(PRROC)
library(here)
library(readxl)
library(psycho)
library(tidyverse)
library("SuperLearner")
library(mlr)
# library(leaps)

source("featurefiltering.R")
source("learnWithCV_booster.R")

set.seed(123)

classifier = "randomForest"
# classifier = "svm"
type = "balanced"
targetVar = c("GWL04","GWL05","GWL06","GWL07","GWL08","GWL09","GWL10","GWL11","GWL12","GWL13","GWL14","GWL15","GWL16","GWL17")
maxFeature = 6
nFolds = 10
water_data <- read_excel("data_median_04_18_all_31march2020.xlsx")
water_data <- water_data[water_data$lithology != 0 ,]
# water_data$GWL18 = NULL
water_data <- water_data[sample(nrow(water_data)),]


rankFile = "rankedFeatures.rds"


models = list()
perfs = list()
lithology_levels = list()
rankedFeatures = NULL
#------------------------------Training Phase--------------------------

k = 1
for(target in targetVar){

  data <- as.data.frame(water_data[!is.na(water_data[[target]]),8:38])
  data$Type = NULL
  for (var in targetVar){
    if(var != target){
      data[[var]] = NULL
    }
  }
  data$Median = NULL


  i = 1

  for (x in data[,target]){
    if(x >7.5){

      data[i,target] = 2
    }
    else{
      data[i,target] = 1
    }
    i = i+ 1
  }

  # target = which(colnames(data) == target)


  data$lithology = factor(data$lithology)

  data$target = factor(data[[target]])
  data[[target]] = NULL
  
  
  rankFile = paste("rankedFeatures",".rds",sep="")
  if(!file.exists(rankFile)){
    frmula = as.formula(paste(target,"~.",spe = ""))
    rank_data = SMOTE( frmula, data, perc.over = 250, k = 10, perc.under = 150);
    rfmodel <- randomForest(frmula,rank_data,importance=TRUE)
    rankedFeatures <- names(rfmodel$importance[order(-rfmodel$importance[,4]),4])
    # print(rankedFeatures)
    
    write_rds(rankedFeatures,rankFile)
  }else{
    rankedFeatures = read_rds(rankFile)
  }

  # print(rankedFeatures)
  # data$target = as.data.frame(data[,target])

  data <- featurefiltering(data,rankedFeatures,'target',maxFeature)

  print(levels(data$lithology))
  lithology_levels[[k]] = levels(data$lithology)
  perf = learnWithCV_booster(as.formula(paste("target","~.",sep = "")),data,nFolds,classifier,type)

  models[[k]]  = perf$model
  perfs[[k]] = perf
  cat("model ",target,"--- acc: ",perf$acc,"\n")
  cat("model ",target,"--- f1: ",perf$f1,"\n")

  k = k + 1
}
write_rds(models,"models.rds")

# ------------------------------Testing Phase----------------------------

models = read_rds("models.rds")
results = list()

test_vals = c()
pred_vals = c()
pred_probs = c()

for (k in seq(length(targetVar),length(targetVar))){
  target = targetVar[k]
  test_data = as.data.frame(water_data[!is.na(water_data[[target]]),8:38])
  
  test_data$Type = NULL
  for (var in targetVar){
    if(var != target){
      test_data[[var]] = NULL
    }
  }
  test_data$Median = NULL
  
  i = 1
  
  for (x in test_data[,target]){
    if(x >7.5){
      
      test_data[i,target] = 2
    }
    else{
      test_data[i,target] = 1
    }
    i = i+ 1
  }
  
  # target = which(colnames(data) == target)
  
  
  test_data$lithology = factor(test_data$lithology)
  
  
  test_data$target = factor(test_data[[target]])
  test_data[[target]] = NULL
  
  
  rankFile = paste("rankedFeatures",".rds",sep="")
  rankedFeatures = read_rds(rankFile)
  test_data <- featurefiltering(test_data,rankedFeatures,"target",maxFeature)
  
  # print(test_data[1:11,])
  
  
  n = length(models)
  preds = matrix(0,nrow(test_data),n+2)  
  
  tmp_models = models[-k]
  tmp_targets = targetVar[-k]
  tmp_levels = lithology_levels[-k]
  tmp_w = perfs[-k]
  w = array(0,n-1)
  for(wi in seq(1,n-1)){
    
    w[wi] = tmp_w[[wi]]$f1
  }
  
  sum_val = sum(w)
  for (wi in seq(1,n-1)){
    w[wi] = w[wi]/sum_val
  }
  
  print(w)
  print(sum(w))
  
  for(j in seq(1,length(tmp_models))){
    # print(tmp_models[j])
    
    test_data_j <- test_data[which(test_data$lithology %in% tmp_levels[[j]]),]
    test_data_j$lithology <- factor(test_data_j$lithology)
    levels(test_data_j$lithology) = tmp_levels[[j]]
    test_data_j.task=makeClassifTask(data=test_data_j,target="target",fixup.data = "no",check.data = FALSE)
    pred = predict(tmp_models[[j]],task = test_data_j.task)
    # pred.th = setThreshold(pred,0.50)
    
    indexes = which(!(test_data$lithology %in% tmp_levels[[j]]))
    
    probs = pred$data$prob.2
    
    # print(target)
    # print(length(probs))
    # print(length(indexes))
    # print(nrow(test_data))
    
    for (ind in indexes){
      l = length(probs)
      if(l+1 != ind){
        probs[seq(ind+1,l+1)] <- probs[seq(ind,l)]  
      }
      
      probs[ind] = -1
      
    }
  
    preds[,j] = probs 
    preds[which(preds[,j]>=0),n] = preds[which(preds[,j]>=0),n] + preds[which(preds[,j]>=0),j] * w[j]  

  }
  
  for(j in seq(1,nrow(preds))){
    
    nas = sum(preds[j,which(preds[j,] < 0)])
    preds[j,n] = (preds[j,n] *1.0 )
    if(preds[j,n] >= .5){
      preds[j,n+1] = 2
    }
    else{
      preds[j,n+1] = 1
    }
  }
  preds[,n+2] <- test_data$target
  
  boosted_prediction = ROCR::prediction(as.numeric(preds[,n+1]),as.numeric(test_data$target))  
  results[[k]] = boosted_prediction
  
  test_vals <- c(test_vals,as.numeric(preds[,n+2]))
  pred_vals <- c(pred_vals,as.numeric(preds[,n+1]))
  pred_probs <- c(pred_probs,preds[,n])
  print(ROCR::prediction(as.numeric(preds[,n+1]), as.numeric(preds[,n+2])))
  write.csv(preds,paste("preds",k,".csv",sep = ""))
}

# k = 1

mlPrediction = ROCR::prediction(pred_vals,test_vals)

acc = unlist(ROCR::performance(mlPrediction,"acc")@y.values)[2]
sensitivity = unlist(ROCR::performance(mlPrediction,"sens")@y.values)[2];
specificity = unlist(ROCR::performance(mlPrediction,"spec")@y.values)[2];
precision   = unlist(ROCR::performance(mlPrediction,"prec")@y.values)[2];
mcc = unlist(ROCR::performance(mlPrediction,"mat")@y.values)[2];
f1  = unlist(ROCR::performance(mlPrediction,"f")@y.values)[2];


# print(targetVar[[k]])
result = list(
  "acc"  = acc,
  "sens" = sensitivity,
  "spec" = specificity,
  "mcc"  = mcc,
  "prec" = precision,
  "f1"   = f1
)
print(result)



