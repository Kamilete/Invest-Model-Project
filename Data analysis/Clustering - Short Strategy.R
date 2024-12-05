# Subsetting the data

PDB <- subset(PD, Strategy == "Buy")
PDS <- subset(PD, Strategy == "Sell")

# Creating the test data set: By randomly splitting data, 70% of the data in the training data set.

library(caTools)

set.seed(100)
spl = sample.split(PDB$MaxCorr5Per, SplitRatio = 0.7)

TrainPDS <- subset(PDB, spl == TRUE)
TestPDS <- subset(PDB, spl == FALSE)

set.seed(100)
spl = sample.split(PDS$MaxCorr5Per, SplitRatio = 0.7)

TrainPDS <- subset(PDS, spl == TRUE)
TestPDS <- subset(PDS, spl == FALSE)

# Subset to cluster

TrainPDSClus <- TrainPDS[,c("DM", "MomentumCh", "ChngSP500D5", "VIX.Close", "ChngVIXD5", "BuyIS")]
TestPDSClus <- TestPDS[,c("DM", "MomentumCh", "ChngSP500D5", "VIX.Close", "ChngVIXD5", "BuyIS")]

# Normalizing the Data

library(caret)

preproc <- preProcess(TrainPDSClus)
#preproc <- preProcess(TrainPDSClus[!(HierGroups == 3),])
TrainPDSClusNorm <- predict(preproc, TrainPDSClus) 
#TrainPDSClusNorm <- predict(preproc, TrainPDSClus[!(HierGroups == 3),]) 
TestPDSClusNorm <- predict(preproc, TestPDSClus) 

summary(TrainPDSClusNorm)
apply(TrainPDSClusNorm, 2, sd)

# Hierarchical Clustering

  distances <- dist(TrainPDSClusNorm, method = "euclidean")
  HierClust <- hclust(distances, method = "ward.D") 
  HierClust <- hclust(distances, method = "complete") 

  plot(HierClust)
  rect.hclust(HierClust, k = 4, border = "red")
  # Given the dendrograms 4 clusters seems to be a good choice

  HierGroups <- cutree(HierClust, k = 4)
  table(HierGroups)

  # Cluster Centroids analysis

  tapply(TrainPDS$DT, HierGroups, summary)
  tapply(TrainPDS$MomentumCh, HierGroups, summary)
  tapply(TrainPDS$DVolUp, HierGroups, summary)
  tapply(TrainPDS$VIX.Close, HierGroups, summary)
  tapply(TrainPDS$BuyIS, HierGroups, summary)
  tapply(TrainPDS$SellIS, HierGroups, summary)

  lapply(split(TrainPDSClus, HierGroups), summary)
  Matr <- sapply(1:4, function(x) colMeans(subset(TrainPDSClus, HierGroups == x)))
  Matr

  MaxCorr5Per <- tapply(TrainPDS$MaxCorr5Per, HierGroups, mean)
  tapply(TrainPDS$MaxCorr10D, HierGroups, summary)
  MaxCorr10D <- tapply(TrainPDS$MaxCorr10D, HierGroups, mean)

  rbind(Matr, MaxCorr5Per, MaxCorr10D)

  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$Month, HierGroups == x))))
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$Weekday, HierGroups == x))))
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$Quarter, HierGroups == x))))
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$MomentumSP500, HierGroups == x))))

  prop.table(table(TrainPDS$Month))
  
  # Cluster assignments for the testing set
  
  library(flexclust)
  
  km.kcca <- as.kcca(HierClust, TrainPDSClusNorm, k = 4)

    # To check if the cluster assignment is equall to the cutree assignment
    HCTrain = predict(km.kcca)
    prop.table(table(HierGroups, HCTrain), 1)
    # The cluster assignment is different
  
  HCTest <- predict(km.kcca, newdata = TestPDSClusNorm)
  
  table(HCTest)

# Scree plot for k-mean clustering

NumClusters <- seq(2,15,1)
SumWithinss <- sapply(2:15, function(x) sum(kmeans(TrainPDSClusNorm, centers=x, nstart=20, iter.max=1000)$withinss))

plot(NumClusters, SumWithinss, type="b")

# K-Means Clustering

  kmeansClust <- kmeans(TrainPDSClusNorm, centers=4, nstart=20)
  kmeansClust
  kmeansClust$size

  # Cluster Centroids analysis

  tapply(TrainPDS$DT, kmeansClust$cluster, summary)
  tapply(TrainPDS$MomentumCh, kmeansClust$cluster, summary)
  tapply(TrainPDS$DVolUp, kmeansClust$cluster, summary)
  tapply(TrainPDS$VIX.Close, kmeansClust$cluster, summary)
  tapply(TrainPDS$BuyIS, kmeansClust$cluster, summary)
  tapply(TrainPDS$SellIS, kmeansClust$cluster, summary)
  
  lapply(split(TrainPDSClus, kmeansClust$cluster), summary)
  Matr <- sapply(1:4, function(x) colMeans(subset(TrainPDSClus, kmeansClust$cluster == x)))
  Matr

  MaxCorr5Per <- tapply(TrainPDS$MaxCorr5Per, kmeansClust$cluster, mean)
  tapply(TrainPDS$MaxCorr10D, kmeansClust$cluster, summary)
  MaxCorr10D <- tapply(TrainPDS$MaxCorr10D, kmeansClust$cluster, mean)

  rbind(Matr, MaxCorr5Per, MaxCorr10D)
  
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$Month, kmeansClust$cluster == x))))
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$Weekday, kmeansClust$cluster == x))))
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$Quarter, kmeansClust$cluster == x))))
  sapply(1:4, function(x) prop.table(table(subset(TrainPDS$MomentumSP500, kmeansClust$cluster == x))))
  
  # Cluster assignments for the testing set

  library(flexclust)
  
  km.kcca <- as.kcca(kmeansClust, TrainPDSClusNorm)
  KmTest <- predict(km.kcca, newdata = TestPDSClusNorm)
  
  table(KmTest)

# Comparison between the hierarchical clustering and the k-mean clustering

  # Training test
  prop.table(table(HierGroups, kmeansClust$cluster),1)
  prop.table(table(HCTrain, kmeansClust$cluster),1)

  # Testing set
  prop.table(table(HCTest, KmTest),1)
  
# Cluster-Specific Predictions base in the K-Means Clustering
  
  TrainPDS.C1 <- subset(TrainPDS, kmeansClust$cluster == 1)
  TrainPDS.C2 <- subset(TrainPDS, kmeansClust$cluster == 2)
  TrainPDS.C3 <- subset(TrainPDS, kmeansClust$cluster == 3)
  TrainPDS.C4 <- subset(TrainPDS, kmeansClust$cluster == 4)
  TestPDS.C1 <- subset(TestPDS, KmTest == 1)
  TestPDS.C2 <- subset(TestPDS, KmTest == 2)
  TestPDS.C3 <- subset(TestPDS, KmTest == 3)
  TestPDS.C4 <- subset(TestPDS, KmTest == 4)


  LogBuymodelC1 <- glm(MaxCorr5Per ~ DT + MomentumCh + VolUp + DVolUp + Month + VIX.Close + BuyIS, data = TrainPDS.C1, family = binomial)
  LogBuymodelC2 <- glm(MaxCorr5Per ~ MomentumCh + DVolUp + Month + log(VIX.Close) + BuyIS, data = TrainPDS.C2, family = binomial)
  LogBuymodelC3 <- glm(MaxCorr5Per ~ 0 + MomentumCh, data = TrainPDS.C3, family = binomial)
  LogBuymodelC4 <- glm(MaxCorr5Per ~ MomentumCh + VolUp + Month + BuyIS + SellIS, data = TrainPDS.C4, family = binomial)
  
  LogBuymodelC1 <- glm(MaxCorr5Per ~ MomentumCh + Month + VIX.Close + BuyIS, data = TrainPDS.C1, family = binomial)
  LogBuymodelC2 <- glm(MaxCorr5Per ~ 0 + MomentumCh, data = TrainPDS.C2, family = binomial)
  LogBuymodelC3 <- glm(MaxCorr5Per ~ MomentumCh + VIX.Close + BuyIS, data = TrainPDS.C3, family = binomial)
  LogBuymodelC4 <- glm(MaxCorr5Per ~ MomentumCh + Month + BuyIS + SellIS, data = TrainPDS.C4, family = binomial)

  summary(LogBuymodelC1)
  summary(LogBuymodelC2)
  summary(LogBuymodelC3)
  summary(LogBuymodelC4)

  # Predictions on training set
  
  predTrain.C1 <- predict(LogBuymodelC1, type = "response", data = TrainPDS.C1)
  predTrain.C2 <- predict(LogBuymodelC2, type = "response", data = TrainPDS.C2)
  predTrain.C3 <- predict(LogBuymodelC3, type = "response", data = TrainPDS.C3)
  predTrain.C4 <- predict(LogBuymodelC4, type = "response", data = TrainPDS.C4)

    # Confusion matrices
  
    prop.table(table(TrainPDS.C1$MaxCorr5Per, as.numeric(predTrain.C1 >= 0.5)), 1)  
    prop.table(table(TrainPDS.C2$MaxCorr5Per, as.numeric(predTrain.C2 >= 0.5)), 1)
    prop.table(table(TrainPDS.C3$MaxCorr5Per, as.numeric(predTrain.C3 >= 0.5)), 1) 
    prop.table(table(TrainPDS.C4$MaxCorr5Per, as.numeric(predTrain.C4 >= 0.5)), 1)
    
      # ROC Curve per cluster
    
      ROCPred <- prediction(predTrain.C4, TrainPDS.C4$MaxCorr5Per)
      ROCPerf <- performance(ROCPred, "tpr", "fpr")
      plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))

    # Overall confusion matrix
    AllTrainPredictions = c(predTrain.C1, predTrain.C2, predTrain.C3, predTrain.C4)
    AllTrainOutcomes = c(TrainPDS.C1$MaxCorr5Per, TrainPDS.C2$MaxCorr5Per, TrainPDS.C3$MaxCorr5Per, TrainPDS.C4$MaxCorr5Per)
    
    table(AllTrainOutcomes, as.numeric(AllTrainPredictions > 0.55))
    (236+383) / length(AllTrainOutcomes)
    # In sample accuracy: 70.02%
    
    # The baseline model has an accuracy of 59.73% 
    
    prop.table(table(AllTrainOutcomes, as.numeric(AllTrainPredictions > 0.55)), 1)
    # Sensitivity: 72.54%
    # Specificity: 66.29%
    
    prop.table(table(AllTrainOutcomes, as.numeric(AllTrainPredictions > 0.55)), 2)
    # Profit: 76.14%
    
    # ROC Curve (Optimum threshold: 0.55)
    
    library(ROCR)
    
    ROCPred <- prediction(AllTrainPredictions, AllTrainOutcomes)
    ROCPerf <- performance(ROCPred, "tpr", "fpr")
    plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))
    
    # AUC: 78.77
    
    as.numeric(performance(ROCPred, "auc")@y.values)

  # Predictions on test set
  
  predTest.C1 <- predict(LogBuymodelC1, type = "response", newdata = na.omit(TestPDS.C1))
  predTest.C2 <- predict(LogBuymodelC2, type = "response", newdata = na.omit(TestPDS.C2))
  predTest.C3 <- predict(LogBuymodelC3, type = "response", newdata = na.omit(TestPDS.C3))
  predTest.C4 <- predict(LogBuymodelC4, type = "response", newdata = na.omit(TestPDS.C4))

    # Confusion matrices
  
    table(na.omit(TestPDS.C1$MaxCorr5Per), as.numeric(predTest.C1 >= 0.5))  
    table(na.omit(TestPDS.C2$MaxCorr5Per), as.numeric(predTest.C2 >= 0.5))
    table(na.omit(TestPDS.C3$MaxCorr5Per), as.numeric(predTest.C3 >= 0.5)) 
    table(na.omit(TestPDS.C4$MaxCorr5Per), as.numeric(predTest.C4 >= 0.5)) 

    # Overall confusion matrix
    AllPredictions = c(predTest.C1, predTest.C2, predTest.C3, predTest.C4)
    AllOutcomes = c(na.omit(TestPDS.C1$MaxCorr5Per), na.omit(TestPDS.C2$MaxCorr5Per), na.omit(TestPDS.C3$MaxCorr5Per), na.omit(TestPDS.C4$MaxCorr5Per))
  
    table(AllOutcomes, as.numeric(AllPredictions > 0.55))
    (99+160) / length(AllOutcomes)
    # Out-of-sample accuracy: 68.52%
    
    # The baseline model has an accuracy of 59.79%.
    
    prop.table(table(AllOutcomes, as.numeric(AllPredictions > 0.55)), 1)
    # Sensitivity: 70.8%
    # Specificity: 65.13%
    
    prop.table(table(AllOutcomes, as.numeric(AllPredictions > 0.55)), 2)
    # Profit: 75.12%
    
    # Test set AUC: 74.7 
    
    ROCPredTest <- prediction(AllPredictions, AllOutcomes)
    as.numeric(performance(ROCPredTest, "auc")@y.values)
    