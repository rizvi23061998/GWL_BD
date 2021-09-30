require("e1071");
require("randomForest");
require("ROCR");
require("pracma");
library(DMwR)
library(dplyr)
library(caret)
library(caTools)
library(here)
library(mlr)
library(e1071)
library(kernlab)
library(ada)
library(nnet)
library(glmnet)
# library(bimba)

configureMlr(on.par.without.desc = "quiet")

learnWithCV_booster <-
  function(formula, data, cross, learner,bType, ...) {
    dependentVar = all.vars(formula)[1];
    
    learner= paste("classif.",learner,sep = "")
    N = length(data[, 1])
    folds = seq(from=1,to=N, by=round(N/cross))
    folds[cross+1] = N+1
    predVector = c()
    newpredvec = c()
    predVector_r = c()
    for (i in 1:cross) {
      trainFolds = data
      testFold = data[(folds[i]:(folds[i+1]-1)),]
      trainFolds = data[-(folds[i]:(folds[i+1]-1)),]
      
      if(bType == "balanced"){
        trainFolds <- SMOTE( as.formula(paste(dependentVar,"~.",sep = "")), trainFolds, perc.over = 250,k=10, perc.under = 150);
        # trainFolds <- ADASYN(trainFolds,perc_over = 90,k=10);
        # print(as.data.frame(table(trainFolds$classe)));
        
      }
      
      # print(paste("model ",i,"is starting training...")
      
      trainFolds.task=makeClassifTask(data=trainFolds,target=dependentVar,fixup.data = "no",check.data = FALSE);
      lrn=makeLearner(learner,predict.type = "prob")
      model=mlr::train(lrn,trainFolds.task);
      testFold.task=makeClassifTask(data=testFold,target=dependentVar,fixup.data = "no",check.data = FALSE);
      mlPred=predict(model,testFold.task)
      mlPred.th=setThreshold(mlPred,0.5)
      
      # prf = performance(mlPred.th);
      
      
      print(paste("model ",i,"is finished training."))
      
      predVector = c(predVector, as.numeric(mlPred.th$data$prob.2))
      predVector_r = c(predVector_r, as.numeric(mlPred.th$data$response))
      i = i + 1
    }
    
    # Now generate the model on full dataset to find the no. of support vectors
    # This can be used for model selection in. Lesser number of SVs will
    # result in better generalization
    
    if(bType == "balanced"){
      new_data <- SMOTE( as.formula(paste(dependentVar,"~.",sep = "")), data, perc.over = 250,k=10, perc.under = 150);
      # trainFolds <- ADASYN(trainFolds,perc_over = 90,k=10);
      print(as.data.frame(table(new_data[[dependentVar]])));
      
    }
    else{
      new_data = data
    }
    new_data.task=makeClassifTask(data=new_data,target=dependentVar,fixup.data = "no",check.data = FALSE)
    
    lrn=makeLearner(learner,predict.type = "prob")
    model=mlr::train(lrn,new_data.task)
    # print(model)
    
    
    
    # perform classification based perf. measures
    if (is.factor(data[,dependentVar])) {
      print("classification based predictions");
      
      
      
      mlPrediction = ROCR::prediction(as.numeric(predVector), as.numeric(data[,dependentVar]))
      
      
      # Find the ROC curve and AUCROC
      AUCROC  = ROCR::performance(mlPrediction,"auc")@y.values[[1]];
      rocCurve = ROCR::performance(mlPrediction,"tpr", "fpr");
      plot(rocCurve,colorize=TRUE)
      # Find the PR curve and AUCPR
      prCurve  = ROCR::performance(mlPrediction,"prec", "rec");
      plot(prCurve,colorize=TRUE)
      x = unlist(prCurve@x.values);
      y = unlist(prCurve@y.values);
      df = data.frame(x = x[2:length(x)], y = y[2:length(y)]);
      AUCPR  = trapz(df$x, df$y)
      
      mlPrediction = ROCR::prediction(as.numeric(predVector_r), as.numeric(data[,dependentVar]))      
      
      acc = unlist(ROCR::performance(mlPrediction,"acc")@y.values)[2]
      sensitivity = unlist(ROCR::performance(mlPrediction,"sens")@y.values)[2];
      specificity = unlist(ROCR::performance(mlPrediction,"spec")@y.values)[2];
      precision   = unlist(ROCR::performance(mlPrediction,"prec")@y.values)[2];
      mcc = unlist(ROCR::performance(mlPrediction,"mat")@y.values)[2];
      f1  = unlist(ROCR::performance(mlPrediction,"f")@y.values)[2];
      
      
      
      return(list(
        "model"= model,
        "rocCurve"= rocCurve,
        "prCurve"= prCurve,
        "AUCROC"= AUCROC,
        "AUCPR" = AUCPR,
        "acc"  = acc,
        "sens" = sensitivity,
        "spec" = specificity,
        "mcc"  = mcc,
        "prec" = precision,
        "f1"   = f1
        
        
      ))
    }
    
    
  }