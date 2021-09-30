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
library(leaps)
library(tidyr)
# library(xlsx)

setwd("D:\StudyMaterials\L4T2\Thesis\Recent_GWL\GWL_BD")

source("featurefiltering.R")
source('learnWithCV.r')


set.seed(123);

classifier = "randomForest"
# classifier = "svm"
type = "balanced"
# targetVar = c("GWL04","GWL05","GWL06","GWL07","GWL08","GWL09","GWL10","GWL11","GWL12","GWL13","GWL14","GWL15","GWL16","GWL17","GWL18")
targetVar = "GWL18"
# output=paste(targetVar,"",sep="")
# output1=paste(targetVar,"",sep="")
# output_comb=paste(targetVar,"",sep="")
runType= "non_select"


nFolds = 10
# biophysical_data <- read_excel("data_median_04_18_all_31march2020.xlsx");
# biophysical_data <- read.csv("bd_fid.csv")
biophysical_data <- read_excel("new_data.xlsx")
# write.csv(biophysical_data,"bd_fid.csv",row.names = FALSE)
# biophysical_data <- biophysical_data[which(biophysical_data$Type != "DPHE"),]

serial_data <- read_excel("excluded_376.xls")
# serial_data <- read.csv("compare_new.csv")
# colnames(serial_data) = c("FID","Tv")
#print( serial_data$serial)
#print(biophysical_data)
# misPred<-semi_join(biophysical_data,serial_data,by="FID")

# misPred<-misPred[which( misPred$GWL18>=5 & misPred$GWL18<=10),]
# write.csv(misPred,file="misPred.csv")
# biophysical_data<-anti_join(biophysical_data, misPred, by="FID")

# write.xlsx(biophysical_data,"new_data.xlsx",sheetName="Sheet1",col.names = TRUE,row.names = TRUE)
# write.csv(biophysical_data,"new_data_376.csv")

comb_data <- biophysical_data[,9:22]
ti = 1
if (runType == 'select'){
  tg =  targetVar[ti]
  print(tg)
  print(colnames(biophysical_data))
  comb_data <- biophysical_data[!is.na(comb_data[,tg]),]
  comb_data$Type = NULL
  comb_data$FID = 1:nrow(comb_data)
  
  saveRDS(comb_data,paste(output_comb,"comb_raw.rds",sep=""));
  
  
}else{
  # comb_data <- read_csv("new_data_376.csv")
  # comb_data <- comb_data[!is.na(comb_data$Median),25:39]
  comb_data <- cbind(comb_data, "GWL18" = biophysical_data[,7])
  print(colnames(comb_data))
}

comb_data <- comb_data[sample(nrow(comb_data)),]
comb_data$Lithology <- factor(comb_data$Lithology)

# comb_data <- comb_data %>% drop_na(GWL18) 

# comb_data <- comb_data[533:nrow(comb_data),]
labelCol = which(colnames(comb_data) == "GWL18");


bin = 2
range = 7.5

comb_data$Median <- comb_data$lithology
h = array(dim = bin);
for(i in 1:bin){
  h[i] = 0;
}
x = 0
for(i in (comb_data$GWL18)){
  x = x+1
  # print(i)
  if(i <= range){
    h[1] = h[1]+1;
    comb_data$Median[x] = "1";
  }else{
    h[2] = h[2] + 1;
    comb_data$Median[x] = "2";
  }
}

print(h)

comb_data$Median <- factor(comb_data$Median)
# levels(comb_data$Median)
# comb_data <- as.data.frame(comb_data)
comb_data$GWL18 <- NULL

rankFile = "rankedFeatures_89.rds"
if(!file.exists(rankFile)){
  # comb_data <- as.matrix(comb_data)
  rank_data = SMOTE(Median~., comb_data, perc.over = 250, k = 10, perc.under = 150);
  rfmodel <- randomForest(Median~.,rank_data,importance=TRUE)
  rankedFeatures <- names(rfmodel$importance[order(-rfmodel$importance[,4]),4])
  print(rankedFeatures)
  write_rds(rankedFeatures,rankFile)
}else{
  rankedFeatures = read_rds(rankFile)
}

results<- data.frame(fsz=integer(),auc=double(),aucpr=double(),acc=double(),spec=double(),sens=double(),f1=double(),mcc=double(),prec=double() );
rocCurvePoints = NULL
prCurvePoints=NULL


comb_data_filtered <- featurefiltering(comb_data,rankedFeatures,"Median",4)
results <- learnWithCV(Median~.,comb_data_filtered,10,classifier,bType = "unbalanced")



comb_data_i <- featurefiltering(comb_data,rankedFeatures,"Median",6)
comb_data_i <- SMOTE(Median~., comb_data_i, perc.over = 250, k = 10, perc.under = 150)

dependentVar = "Median"
trainFolds <- comb_data_i
learner <- paste("classif.",classifier,sep="")
# testData <- read_excel("Rocwork_input_16April2020_correction_14indicators.xls")
testData <- read_excel("final4_features1(5k)_value30April2020.xls")
testData <- as.data.frame(testData)
testFold <- as.data.frame(testData)
testFold$Lithology <- factor(testFold$Lithology)
testFold$Median <- testFold$Lithology
levels(trainFolds$Lithology) <- c(levels(trainFolds$Lithology),levels(testFold$Lithology))
levels(testFold$Lithology) <- levels(trainFolds$Lithology)
# 
# x = 0
# for(i in (testFold$GWL18)){
#   x = x+1
#   # print(i)
#   if(i <= range){
#     h[1] = h[1]+1;
#     testFold$Median[x] <- "1";
#   }else{
#     h[2] = h[2] + 1;
#     testFold$Median[x] <- "2";
#   }
# }
testFold$Median <- factor(testFold$Median)

# trainFolds.task=makeClassifTask(data=trainFolds,target=dependentVar,fixup.data = "no",check.data = FALSE);
# lrn=makeLearner(learner,predict.type = "prob")
# model=mlr::train(lrn,trainFolds.task);
# 
# 
# # testData <- read_excel("Rocwork_input_16April2020_correction_14indicators.xls")
# 
# # testFold$Lithology <- factor(testFold$Lithology)
# # testFold$Median <- testFold$Lithology
# testFold <- featurefiltering(testFold,rankedFeatures,'Median',6)
# # testFold$Median <- NULL
# testFold.task=makeClassifTask(data=testFold,target=dependentVar,fixup.data = "no",check.data = FALSE);
# mlPred=predict(model,testFold.task)
# 
# # print(mlPred$data$response)
# testData$response6 <- mlPred$data$response
# 
# # ============================SVM predictions==========================
# mlPrediction = ROCR::prediction(as.numeric(mlPred$data$prob.2), as.numeric(testFold$Median))
# 
# AUCROC  = ROCR::performance(mlPrediction,"auc")@y.values[[1]];
# rocCurve = ROCR::performance(mlPrediction,"tpr", "fpr");
# plot(rocCurve,colorize=TRUE)
# 
# # Find the PR curve and AUCPR
# prCurve  = ROCR::performance(mlPrediction,"prec", "rec");
# plot(prCurve,colorize=TRUE)
# x = unlist(prCurve@x.values);
# y = unlist(prCurve@y.values);
# df = data.frame(x = x[2:length(x)], y = y[2:length(y)]);
# AUCPR  = trapz(df$x, df$y)
# 
# mlPrediction = ROCR::prediction(as.numeric(testData$response6), as.numeric(testFold$Median))      
# 
# acc = unlist(ROCR::performance(mlPrediction,"acc")@y.values)[2]
# sensitivity = unlist(ROCR::performance(mlPrediction,"sens")@y.values)[2];
# specificity = unlist(ROCR::performance(mlPrediction,"spec")@y.values)[2];
# precision   = unlist(ROCR::performance(mlPrediction,"prec")@y.values)[2];
# mcc = unlist(ROCR::performance(mlPrediction,"mat")@y.values)[2];
# f1  = unlist(ROCR::performance(mlPrediction,"f")@y.values)[2];
# 
# 
# prf6<-data.frame(list(
#   "AUCROC"= AUCROC,
#   "AUCPR" = AUCPR,
#   "acc"  = acc,
#   "sens" = sensitivity,
#   "spec" = specificity,
#   "mcc"  = mcc,
#   "prec" = precision,
#   "f1"   = f1
#   
#   
# ))
# 

# ==================================RF Training========================

classifier = "randomForest"

comb_data_i <- featurefiltering(comb_data,rankedFeatures,"Median",4)
comb_data_i <- SMOTE(Median~., comb_data_i, perc.over = 250, k = 10, perc.under = 150)
dependentVar = "Median"
trainFolds <- comb_data_i
learner <- paste("classif.",classifier,sep="")
levels(trainFolds$Lithology) <- c(levels(trainFolds$Lithology),testFold$Lithology)
levels(testFold$Lithology) <- levels(trainFolds$Lithology)


trainFolds.task=makeClassifTask(data=trainFolds,target=dependentVar,fixup.data = "no",check.data = FALSE);
lrn=makeLearner(learner,predict.type = "prob")
model=mlr::train(lrn,trainFolds.task);
# testData <- read_excel("Rocwork_input_16April2020_correction_14indicators.xls")

# testFold$Lithology <- factor(testFold$Lithology)
# testFold$Median <- testFold$Lithology
testFold <- featurefiltering(testFold,rankedFeatures,'Median',4)
print(testFold)
# testFold$Median <- NULL
testFold.task=makeClassifTask(data=testFold,target=dependentVar,fixup.data = "no",check.data = FALSE);
mlPred=predict(model,testFold.task)

# print(mlPred$data$response)
testData$response4 <- mlPred$data$response


# =========================RF PRedictions=======================
# 
# mlPrediction = ROCR::prediction(as.numeric(mlPred$data$prob.2), as.numeric(testFold$Median))
# 
# AUCROC  = ROCR::performance(mlPrediction,"auc")@y.values[[1]];
# rocCurve = ROCR::performance(mlPrediction,"tpr", "fpr");
# plot(rocCurve,colorize=TRUE)
# 
# # Find the PR curve and AUCPR
# prCurve  = ROCR::performance(mlPrediction,"prec", "rec");
# plot(prCurve,colorize=TRUE)
# x = unlist(prCurve@x.values);
# y = unlist(prCurve@y.values);
# df = data.frame(x = x[2:length(x)], y = y[2:length(y)]);
# AUCPR  = trapz(df$x, df$y)
# 
# mlPrediction = ROCR::prediction(as.numeric(testData$response4), as.numeric(testFold$Median))      
# 
# acc = unlist(ROCR::performance(mlPrediction,"acc")@y.values)[2]
# sensitivity = unlist(ROCR::performance(mlPrediction,"sens")@y.values)[2];
# specificity = unlist(ROCR::performance(mlPrediction,"spec")@y.values)[2];
# precision   = unlist(ROCR::performance(mlPrediction,"prec")@y.values)[2];
# mcc = unlist(ROCR::performance(mlPrediction,"mat")@y.values)[2];
# f1  = unlist(ROCR::performance(mlPrediction,"f")@y.values)[2];
# 
# 
# prf4<-data.frame(list(
#   "AUCROC"= AUCROC,
#   "AUCPR" = AUCPR,
#   "acc"  = acc,
#   "sens" = sensitivity,
#   "spec" = specificity,
#   "mcc"  = mcc,
#   "prec" = precision,
#   "f1"   = f1
#   
#   
# ))
# 
# testData$Real_val <- testFold$Median
# 
# prf <- rbind(prf6,prf4)
# rownames(prf) <- c('svm_6_features','rf_4_features')
# write.csv(prf,"outputs/performance_376.csv")
# 
















# colnames(results) <- c("Feature Size","AUC","AUCPR","Accuracy","Specificity","Sensitivity","F1","MCC","Precision")
# for(i in 6:6){
#   # rankedFeatures= c(rankedFeatures,"FID")
#   comb_data_i <- featurefiltering(comb_data,rankedFeatures,"Median",i)
#   # print(colnames(comb_data_i))
#   # FID = comb_data$FID
#   # comb_data_i = cbind(comb_data_i,FID)
#   # print(colnames(comb_data_i))
#   # comb_data_i$FID = 1:nrow(comb_data_i)
#   perf = learnWithCV(Median~.,comb_data_i,nFolds,classifier,type)
#   results <- rbind(results,cbind(i,perf$AUCROC,perf$AUCPR,perf$acc,perf$spec,perf$sens,perf$f1,perf$mcc,perf$prec))  
#   
#   cat("",i,perf$acc,perf$spec,perf$sens,perf$f1,perf$mcc,perf$prec)
#   df = data.frame(
#     x = unlist(perf$rocCurve@x.values),
#     y = unlist(perf$rocCurve@y.values),
#     Features = as.character(i)
#   );
#   rocCurvePoints = rbind(rocCurvePoints, df);
#   
#   df = data.frame(
#     x = unlist(perf$prCurve@x.values),
#     y = unlist(perf$prCurve@y.values),
#     Features = as.character(i)
#   );
#   
#   prCurvePoints = rbind(prCurvePoints, df);
#   # print(perf)
#   
# }
# 
# 
# 
# 
# 
# # colnames(results) <- c("Feature Size","AUC","AUCPR","Accuracy","Specificity","Sensitivity","F1","MCC","Precision")
# # write.csv(results,paste("perf_vs_features_",classifier,"_t.csv",sep= ""))
# # write_rds(rocCurvePoints,paste("roc_",classifier,"_t.rds",sep=""))
# # write_rds(prCurvePoints,paste("pr_",classifier,"_t.rds",sep = ""))
# # 
