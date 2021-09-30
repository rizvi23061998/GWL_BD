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



source("featurefiltering.R")
source('learnWithCV.r')


set.seed(123);

# classifier = "randomForest"
classifier = "svm"
type = "unbalanced"
targetVar = "GWL18"
runType= "non_select"

outFile = paste0("outputs/results_",classifier,".csv")
rocFile = paste0("outputs/roc_",classifier,".rds")
prFile = paste0("outputs/pr_",classifier,".rds")

nFolds = 10
biophysical_data <- read_excel("new_data.xlsx")

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

rankFile = "rankedFeatures_89.rds"
if(!file.exists(rankFile)){
  # comb_data <- as.matrix(comb_data)
  # rank_data = SMOTE(Median~., comb_data, perc.over = 250, k = 10, perc.under = 150);
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
