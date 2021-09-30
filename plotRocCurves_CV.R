library(ggplot2)
library(RColorBrewer)


plot_roc <- function(rocCurvePoints,title,col_palette){
  rocPlot = ggplot(rocCurvePoints,aes(x, y)) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "left",legend.direction = "vertical") +
    geom_line(aes(colour=Features),size = 1) +
    labs(title = title,x = "False Positive Rate", y = "True Positive Rate")+
    scale_colour_manual(name="No. of\nFeatures",values = col_palette);
  
    rocPlot
}

plot_pr <- function(prCurvePoints,title,col_palette){
  
  prPlot = ggplot(prCurvePoints,aes(x, y)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "left",legend.direction = "vertical") +
    
    geom_line(aes(colour=Features),size = 1) +
    labs(title = title,x = "Recall", y = "Precision")+
    scale_colour_manual(name="No. of\nFeatures",values = col_palette);
  
  prPlot
}

# for (type in c("Balanced_", "Unbalanced_")) {
  type = "_randomForest"
  mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(14)
  
  rocCurveFile = paste0( "ROCCurve",type,".eps");
  prCurveFile  = paste0( "PRCurve",type,".eps");
  
  rocCurvePoints_rf = readRDS(paste0("outputs/roc",type,".rds"));
  prCurvePoints_rf  = readRDS(paste0("outputs/pr",type,".rds"));
  
  t = which(rocCurvePoints_rf$Features %in% 1:14);
  rocCurvePoints_rf = rocCurvePoints_rf[t,]
  
  t = which(prCurvePoints_rf$Features %in% 1:14);
  prCurvePoints_rf = prCurvePoints_rf[t,]

  #------------------SVM ----------------------
  type = "_svm"
  
  rocCurvePoints_svm = readRDS(paste0("outputs/roc",type,".rds"));
  prCurvePoints_svm  = readRDS(paste0("outputs/pr",type,".rds"));
  
  t = which(rocCurvePoints_svm$Features %in% 1:14);
  rocCurvePoints_svm = rocCurvePoints_svm[t,]
  
  t = which(prCurvePoints_svm$Features %in% 1:14);
  prCurvePoints_svm = prCurvePoints_svm[t,]
  
  roc_plot_rf <- plot_roc(rocCurvePoints_rf,"(a)ROC Curve(Random Forest)",mycolors)
  pr_plot_rf <- plot_pr(prCurvePoints_rf,"(b)PR Curve(Random Forest)",mycolors)
  
  roc_plot_svm <- plot_roc(rocCurvePoints_svm,"(c)ROC Curve(SVM)",mycolors)
  pr_plot_svm <- plot_pr(prCurvePoints_svm,"(d)PR Curve(SVM)",mycolors)
  
  figure_roc_pr <- ggarrange(roc_plot_rf,pr_plot_rf,roc_plot_svm,pr_plot_svm,
                             nrow = 2,ncol = 2,common.legend = T,legend = "left")
  
  tiff(filename = "outputs/tiff/Figure_4.tiff",res = 300,width=2200,height = 1800);
  # print(nFeatureTuning + scale_colour_grey(start = 0, end = 0.8));
  print(figure_roc_pr)
  
  dev.off()
  
    
  # postscript(file = rocCurveFile, paper = "letter");
  # # print(rocPlot + scale_colour_grey(start = 0, end = 0.8))
  # 
  # 
  # print(rocPlot + scale_colour_manual(values = mycolors))
  # dev.off();
  # 
  # postscript(file = prCurveFile, paper = "letter");
  # mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(14)
  # # print(prPlot + scale_colour_grey(start = 0, end = 0.6));
  # print(prPlot + scale_colour_manual(values = mycolors))
  # dev.off()
# }