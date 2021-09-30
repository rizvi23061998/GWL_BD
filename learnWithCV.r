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

learnWithCV <-
  function(formula, data, cross, learner,bType, ...) {
    dependentVar = all.vars(formula)[1];
    trainType = "cs";
    FID = data$FID
    data$FID = NULL
    learner= paste("classif.",learner,sep = "")
    
    # print(paste("Starting training with ",learner));
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
        trainFolds <- SMOTE( Median~., trainFolds, perc.over = 250, k = 10, perc.under = 150);
        # trainFolds <- ADASYN(trainFolds,perc_over = 90,k=10);
        # print(as.data.frame(table(trainFolds$classe)));
        
      }
      
      # print(paste("model ",i,"is starting training..."))
      
      model = NULL;
      if(trainType == "cs"){
        # print((trainFolds))
        # print(str(trainFolds));
        # print(str(testFold));
        trainFolds.task=makeClassifTask(data=trainFolds,target=dependentVar,fixup.data = "no",check.data = FALSE);
        lrn=makeLearner(learner,predict.type = "prob")
        model=mlr::train(lrn,trainFolds.task);
        testFold.task=makeClassifTask(data=testFold,target=dependentVar,fixup.data = "no",check.data = FALSE);
        mlPred=predict(model,testFold.task);
        mlPred.th=setThreshold(mlPred,0.5);
        # print(mlPred)
        # print(mlPred.th$data$prob.1)
        # print(mlPred.th$data$response);
        prf = performance(mlPred.th);
        # print(mlPred);
        # model = randomForest::randomForest(formula,trainFolds,classwt=c(1,10));
        # # print(colnames(testFold))
        # labelCol = which(colnames(testFold) == dependentVar);
        # 
        # # print(dependentVar)
        # mlPred = predict(model,testFold[,-labelCol]);
        # 
        #model = svm(formula,trainFolds);
      }
      else{
        if(learner == "lm"){
          model = lm(formula,trainFolds);  
        }else if(learner == "svm"){
          # linear.tune = tune.svm(formula, data=trainFolds, kernel="linear", cost=c(0.001, 0.01, 0.1, 1,5,10))
          # model = linear.tune$best.model;
          model  = svm(formula,trainFolds,kernel = "radial");
          # cat(summary(model));
        }else if(learner == "rf"){
          model = randomForest(formula,trainFolds);
        }else if(learner == "lasso"){
          
          labelCol = which(colnames(trainFolds) == dependentVar);
          x <- as.matrix(trainFolds[,-labelCol]);
          model = cv.glmnet(x,trainFolds[,dependentVar],lambda = 10^seq(2,-2,by=-.1),alpha = seq(0.7,1.2,by=0.05));
        }
        mlPred = NULL;
        if(learner == "lasso"){
          mlPred = predict(model, as.matrix(testFold[,-labelCol]),s=model$lambda.min,type="response");
        }else{
          mlPred = predict(model, testFold[,-labelCol]);
        }
        
      }
      
      # print(paste("model ",i,"is finished training."))
      labelCol = which(colnames(testFold) == dependentVar);
      
      
      # print("printing confusion matrix")
      # print(levels(factor(testFold[,-labelCol])))
      # 
      # print(levels(mlPred))
      # print(confusionMatrix((testFold[,labelCol]),(mlPred)))
      # mlPrediction_tmp = DMwR::regr.eval(testFold[, dependentVar],mlPred);
      # print(mlPrediction_tmp);
      # newpredvec = c(newpredvec,mlPred)
      # print(newpredvec)
      # print("mlprd")
      
      # print(mlPred)
      
      predVector = c(predVector, as.numeric(mlPred.th$data$prob.2))
      predVector_r = c(predVector_r, as.numeric(mlPred.th$data$response))
      # predVector_prob = c(predVector_prob, as.numeric(mlPred.th$pr))
      
      # predVector = c(predVector, as.numeric(mlPred))
      
            # print(predVector)
      # print(summary(model))
      i = i + 1
    }
    
    # Now generate the model on full dataset to find the no. of support vectors
    # This can be used for model selection in. Lesser number of SVs will
    # result in better generalization
    # model = learn(formula, data, learner, ...);
    
    
    
    # perform classification based perf. measures
    if (is.factor(data[,dependentVar])) {
      print("classification based predictions");
       
      mlPrediction = ROCR::prediction(as.numeric(predVector), as.numeric(data[,dependentVar]))
      # print(mlPrediction)
      # Find the ROC curve and AUCROC
      AUCROC  = ROCR::performance(mlPrediction,"auc")@y.values[[1]];
      rocCurve = ROCR::performance(mlPrediction,"tpr", "fpr");
      # plot(rocCurve,colorize=TRUE)
      # Find the PR curve and AUCPR
      prCurve  = ROCR::performance(mlPrediction,"prec", "rec");
      # plot(prCurve,colorize=TRUE)
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
      
      
      # predicted<-as.numeric(predVector_r)
      # actual<- as.numeric(data[,dependentVar])
      # result<-cbind(actual,predicted)
      # result<-result[which(result[,2]!=result[,1]),]
      # write.csv(result,file=paste("ff",ncol(data),"_compare.csv"))
      # 
      
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
    
    else {
      # perform regression based perf. measurements
      # print("perform regression based perf. measurements");
      
      # print(predVector)
      mlPrediction = DMwR::regr.eval(data[, dependentVar],predVector);
      # print(mlPrediction)
      # 
      
      
      # Function that returns Root Mean Squared Error
      rmse <- function(error)
      {
        sqrt(mean(error^2))
      }
      
      # Function that returns Mean Absolute Error
      mae <- function(error)
      {
        mean(abs(error))
      }
      rsq <- function(x, y) summary(lm(y~x))$r.squared
      adjRsq <- function(x, y) summary(lm(y~x))$adj.r.squared
      
      
      error <- data[,dependentVar] - predVector;
      rmse <- rmse(error);
      mae <- mae(error);
      rsq = rsq(predVector,data[,dependentVar]);
      adj_rsq = adjRsq(predVector,data[,dependentVar]);
      return(list(
        "rsq" = rsq,
        "adj_rsq" = adj_rsq,
        "rmse" = rmse,
        "mae" = mae,
        "predictedRes" = predVector
      ))
      
    }
  }