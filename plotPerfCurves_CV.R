#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################

library("ggplot2")
# library("XLConnect")
library("reshape2")
library(RColorBrewer)
library(ggpubr)

###### Accuracy/MCC etc. vs. choice of nFeatures  ############

# type = "Balanced_";
# 
# # Use the appropriate data file here:
# xlsFile  = paste0("PerfSearch_RF_", type, "SvmRFE2_comb.xlsx");
# workBook = loadWorkbook(xlsFile);
# 
# for (xlsSheet in c("Tenfold_Coarse", "Tenfold_MidGrain", "Tenfold_Avg")) {

  mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(14)  
  # data = readWorksheet(workBook, xlsSheet);
  
  data_rf = read.csv("outputs/results_randomForest.csv")
  data_rf = data_rf[, c("nF", "AUCROC", "AUCPR", "Accuracy", "Sensitivity", "Specificity")];
  colnames(data_rf) = c("nF", "auROC", "auPR", "Acc", "Sn", "Sp");
  df_rf <- melt(data_rf,  id.vars = "nF", variable.name = 'Metric');
  
  nFeatureTuning_rf = ggplot(df_rf,aes(x=nF,y=value)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "top") +
    geom_line(aes(colour=Metric),size =2) +
    labs(title = "(a)Random Forest",x = "Num. of Features", y = "Perf. Score")+ 
    scale_colour_manual(values = mycolors);
  
  # ------------------------------- SVM ----------------------------------------
  data_svm = read.csv("outputs/results_svm.csv")
  data_svm = data_svm[, c("nF", "AUCROC", "AUCPR", "Accuracy", "Sensitivity", "Specificity")];
  colnames(data_svm) = c("nF", "auROC", "auPR", "Acc", "Sn", "Sp");
  df_svm <- melt(data_svm,  id.vars = "nF", variable.name = 'Metric');
  
  nFeatureTuning_svm = ggplot(df_svm,aes(x=nF,y=value)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "top") +
    geom_line(aes(colour=Metric),size =2) +
    labs(title = "(b)SVM",x = "Num. of Features", y = "Perf. Score")+ 
    scale_colour_manual(values = mycolors);
  
  
  
  feature_figure <- ggarrange(nFeatureTuning_rf,nFeatureTuning_svm,ncol = 2,
                              common.legend = T,legend = "left")
  
  tiff(filename = "outputs/tiff/Figure_5.tiff",res = 300,width=3000,height=1200);
  # print(nFeatureTuning + scale_colour_grey(start = 0, end = 0.8));
  print(feature_figure)
  
  dev.off()
# }