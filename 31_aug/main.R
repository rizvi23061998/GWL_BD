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
# library(xlsx)

setwd("D:\\StudyMaterials\\L4T2\\Thesis\\Recent_GWL\\GWL_BD")

source("featurefiltering.R")
source('learnWithCV.r')


set.seed(123);

classifier = "randomForest"
#classifier = "svm"
type = "balanced"
targetVar = c("GWL04","GWL05","GWL06","GWL07","GWL08","GWL09","GWL10","GWL11","GWL12","GWL13","GWL14","GWL15","GWL16","GWL17","GWL18")
output=paste(targetVar,"",sep="")
output1=paste(targetVar,"",sep="")
output_comb=paste(targetVar,"",sep="")
runType= "non_select"

#change file name according to classifier

outFile="outputs/results_rf.csv";
rocFile="outputs/rocData_rf.rds";
prFile="outputs/prData_rf.rds";


nFolds = 10
#biophysical_data <- read_excel("data_median_04_18_all_31march2020.xlsx");

biophysical_data <- read_excel("outputs/data_acc_89.xlsx")
# write.csv(biophysical_data,"bd_fid.csv",row.names = FALSE)
# biophysical_data <- biophysical_data[which(biophysical_data$Type != "DPHE"),]

# serial_data <- read.csv("compare.csv")
# serial_data <- read.csv("compare_new.csv")
# colnames(serial_data) = c("FID","Tv")
#print( serial_data$serial)
#print(biophysical_data)
# misPred<-semi_join(biophysical_data,serial_data,by="FID")

# misPred<-misPred[which( misPred$GWL18>=5 & misPred$GWL18<=10),]
# write.csv(misPred,file="misPred.csv")
# biophysical_data<-anti_join(biophysical_data, misPred, by="FID")

# write.xlsx(biophysical_data,"new_data.xlsx",sheetName="Sheet1",col.names = TRUE,row.names = TRUE)
# write.csv(biophysical_data,"new_data.csv")

comb_data <- biophysical_data[,9:22] # getting 14 features
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
  # comb_data <- read_csv("new_data.csv")
  # comb_data <- comb_data[!is.na(comb_data$Median),25:39]
  comb_data <- cbind(comb_data, biophysical_data[,7])
  print(colnames(comb_data))
}

comb_data <- comb_data[sample(nrow(comb_data)),]
comb_data$Lithology <- factor(comb_data$Lithology)

# comb_data <- comb_data[533:nrow(comb_data),]
labelCol = which(colnames(comb_data) == "GWL18");


bin = 2
range = 7.5

comb_data$Median <- comb_data$Lithology
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

rankFile = "rankedFeatures_89_new.rds"
if(!file.exists(rankFile)){
  # comb_data <- as.matrix(comb_data)
  rank_data = SMOTE(Median~., comb_data, perc.over = 250, k = 10, perc.under = 150);
  rfmodel <- randomForest(Median~.,rank_data,importance=TRUE)
  rankedFeatures <- names(rfmodel$importance[order(-rfmodel$importance[,4]),4])
  print(rankedFeatures)
  rf_with_value <- cbind("feature_name"=rankedFeatures,"imp_value"=rfmodel$importance[order(-rfmodel$importance[,4]),4])
  rf_with_value <- as.data.frame(rf_with_value)
  write.csv(rf_with_value,"outputs/ranked_features_value.csv")
  write_rds(rankedFeatures,rankFile)
}else{
  rankedFeatures = read_rds(rankFile)
}


results<- data.frame(fsz=integer(),auc=double(),aucpr=double(),acc=double(),spec=double(),sens=double(),f1=double(),mcc=double(),prec=double() );
rocCurvePoints = NULL
prCurvePoints=NULL

for(i in 1:14){
  comb_data_i <- featurefiltering(comb_data,rankedFeatures,"Median",i)
  
  result_i <- learnWithCV(formula=Median~.,data = comb_data_i,cross = 10,learner = classifier,bType = type)
  
  df = data.frame(
    x = unlist(result_i$rocCurve@x.values), 
    y = unlist(result_i$rocCurve@y.values), 
    Features = as.character(i)
  );
  rocCurvePoints = rbind(rocCurvePoints, df);
  
  df = data.frame(
    x = unlist(result_i$prCurve@x.values), 
    y = unlist(result_i$prCurve@y.values), 
    Features = as.character(i)
  );
  prCurvePoints = rbind(prCurvePoints, df);
  
  
  cat(
    i,
    ",", round(result_i$AUCROC, 2),
    ",", round(result_i$AUCPR, 2),
    ",", round(result_i$acc, 2),
    ",", round(result_i$spec, 2),
    ",", round(result_i$sens, 2),
    ",", round(result_i$f1, 2),
    ",", round(result_i$mcc, 2),
    ",", round(result_i$prec, 2)
  );
  
  
  tmp <- cbind(i,result_i$AUCROC,result_i$AUCPR,result_i$acc,result_i$spec,result_i$sens,result_i$f1,result_i$mcc,result_i$prec);
  results <- rbind(results,tmp);
  
  
}
saveRDS(rocCurvePoints,rocFile);
saveRDS(prCurvePoints,prFile);
colnames(results) = c(
  "nF"
  , "AUCROC"
  , "AUCPR"
  , "Accuracy"
  , "Specificity"
  , "Sensitivity"
  , "F1"
  , "MCC"
  , "Precision"
);
write.csv(results, outFile)
