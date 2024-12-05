# Subsetting the data

PDB <- subset(PD, Strategy == "Buy")
PDS <- subset(PD, Strategy == "Sell")

# Creating the test data set: By randomly splitting data, 70% of the data in the training data set.

library(caTools)

set.seed(100)
spl = sample.split(PDB$MaxCorr5Per, SplitRatio = 0.7)

TrainPDB <- subset(PDB, spl == TRUE)
TestPDB <- subset(PDB, spl == FALSE)

set.seed(100)
spl = sample.split(PDS$MaxCorr5Per, SplitRatio = 0.7)

TrainPDS <- subset(PDS, spl == TRUE)
TestPDS <- subset(PDS, spl == FALSE)

# Subset to cluster

TrainPDBClus <- TrainPDB[,c("DT", "TCh", "DVolUp", "VIX.Close", "BuyIS")]
TestPDBClus <- TestPDB[,c("DT", "TCh", "DVolUp", "VIX.Close", "BuyIS")]

# Normalizing the Data

library(caret)

preproc <- preProcess(TrainPDBClus)
#preproc <- preProcess(TrainPDBClus[!(HierGroups == 3),])
TrainPDBClusNorm <- predict(preproc, TrainPDBClus) 
#TrainPDBClusNorm <- predict(preproc, TrainPDBClus[!(HierGroups == 3),]) 
TestPDBClusNorm <- predict(preproc, TestPDBClus) 

summary(TrainPDBClusNorm)
apply(TrainPDBClusNorm, 2, sd)

# Hierarchical Clustering

  distances <- dist(TrainPDBClusNorm, method = "euclidean")
  HierClust <- hclust(distances, method = "ward.D") 
  # HierClust <- hclust(distances, method = "complete") 

  plot(HierClust)
  rect.hclust(HierClust, k = 5, border = "red")
  # Given the dendrograms 4 clusters seems to be a good choice

  HierGroups <- cutree(HierClust, k = 4)
  table(HierGroups)

  # Cluster Centroids analysis

  tapply(TrainPDB$DT, HierGroups, summary)
  tapply(TrainPDB$MomentumCh, HierGroups, summary)
  tapply(TrainPDB$DVolUp, HierGroups, summary)
  tapply(TrainPDB$VIX.Close, HierGroups, summary)
  tapply(TrainPDB$BuyIS, HierGroups, summary)
  tapply(TrainPDB$SellIS, HierGroups, summary)

  lapply(split(TrainPDBClus, HierGroups), summary)
  Matr <- sapply(1:5, function(x) colMeans(subset(TrainPDBClus, HierGroups == x)))
  Matr

  MaxCorr5Per <- tapply(TrainPDB$MaxCorr5Per, HierGroups, mean)
  tapply(TrainPDB$MaxCorr10D, HierGroups, summary)
  MaxCorr10D <- tapply(TrainPDB$MaxCorr10D, HierGroups, mean)

  rbind(Matr, MaxCorr5Per, MaxCorr10D)

  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$Month, HierGroups == x))))
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$Weekday, HierGroups == x))))
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$Quarter, HierGroups == x))))
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$MomentumSP500, HierGroups == x))))

  prop.table(table(TrainPDB$Month))
  
  # Cluster assignments for the testing set
  
  library(flexclust)
  
  km.kcca <- as.kcca(HierClust, TrainPDBClusNorm, k = 5)

    # To check if the cluster assignment is equall to the cutree assignment
    HCTrain = predict(km.kcca)
    prop.table(table(HierGroups, HCTrain), 1)
    # The cluster assignment is different
  
  HCTest <- predict(km.kcca, newdata = TestPDBClusNorm)
  
  table(HCTest)

# Scree plot for k-mean clustering

NumClusters <- seq(2,15,1)
SumWithinss <- sapply(2:15, function(x) sum(kmeans(TrainPDBClusNorm, centers=x, nstart=20, iter.max=1000)$withinss))

plot(NumClusters, SumWithinss, type="b")

# K-Means Clustering

  kmeansClust <- kmeans(TrainPDBClusNorm, centers=4, nstart=20)
  kmeansClust
  kmeansClust$size

  # Cluster Centroids analysis

  tapply(TrainPDB$DT, kmeansClust$cluster, summary)
  tapply(TrainPDB$MomentumCh, kmeansClust$cluster, summary)
  tapply(TrainPDB$DVolUp, kmeansClust$cluster, summary)
  tapply(TrainPDB$VIX.Close, kmeansClust$cluster, summary)
  tapply(TrainPDB$BuyIS, kmeansClust$cluster, summary)
  tapply(TrainPDB$SellIS, kmeansClust$cluster, summary)
  
  lapply(split(TrainPDBClus, kmeansClust$cluster), summary)
  Matr <- sapply(1:5, function(x) colMeans(subset(TrainPDBClus, kmeansClust$cluster == x)))
  Matr

  MaxCorr5Per <- tapply(TrainPDB$MaxCorr5Per, kmeansClust$cluster, mean)
  tapply(TrainPDB$MaxCorr10D, kmeansClust$cluster, summary)
  MaxCorr10D <- tapply(TrainPDB$MaxCorr10D, kmeansClust$cluster, mean)

  rbind(Matr, MaxCorr5Per, MaxCorr10D)
  
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$Month, kmeansClust$cluster == x))))
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$Weekday, kmeansClust$cluster == x))))
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$Quarter, kmeansClust$cluster == x))))
  sapply(1:5, function(x) prop.table(table(subset(TrainPDB$MomentumSP500, kmeansClust$cluster == x))))
  
  # Cluster assignments for the testing set

  library(flexclust)
  
  km.kcca <- as.kcca(kmeansClust, TrainPDBClusNorm)
  KmTest <- predict(km.kcca, newdata = TestPDBClusNorm)
  
  table(KmTest)

# Comparison between the hierarchical clustering and the k-mean clustering

  # Training test
  prop.table(table(HierGroups, kmeansClust$cluster),1)
  prop.table(table(HCTrain, kmeansClust$cluster),1)

  # Testing set
  prop.table(table(HCTest, KmTest),1)
  
# Cluster-Specific Predictions base in the K-Means Clustering
  
  TrainPDB.C1 <- subset(TrainPDB, kmeansClust$cluster == 1)
  TrainPDB.C2 <- subset(TrainPDB, kmeansClust$cluster == 2)
  TrainPDB.C3 <- subset(TrainPDB, kmeansClust$cluster == 3)
  TrainPDB.C4 <- subset(TrainPDB, kmeansClust$cluster == 4)
  TestPDB.C1 <- subset(TestPDB, KmTest == 1)
  TestPDB.C2 <- subset(TestPDB, KmTest == 2)
  TestPDB.C3 <- subset(TestPDB, KmTest == 3)
  TestPDB.C4 <- subset(TestPDB, KmTest == 4)


  LogBuymodelC1 <- glm(MaxCorr5Per ~ DT + TCh + VolUp + DVolUp + Month + VIX.Close + BuyIS, data = TrainPDB.C1, family = binomial)
  LogBuymodelC2 <- glm(MaxCorr5Per ~ TCh + DVolUp + Month + log(VIX.Close) + BuyIS, data = TrainPDB.C2, family = binomial)
  LogBuymodelC3 <- glm(MaxCorr5Per ~ 0 + TCh, data = TrainPDB.C3, family = binomial)
  LogBuymodelC4 <- glm(MaxCorr5Per ~ TCh + VolUp + Month + BuyIS + SellIS, data = TrainPDB.C4, family = binomial)
  
  LogBuymodelC1 <- glm(MaxCorr5Per ~ TCh + Month + VIX.Close + BuyIS, data = TrainPDB.C1, family = binomial)
  LogBuymodelC2 <- glm(MaxCorr5Per ~ 0 + TCh, data = TrainPDB.C2, family = binomial)
  LogBuymodelC3 <- glm(MaxCorr5Per ~ TCh + VIX.Close + BuyIS, data = TrainPDB.C3, family = binomial)
  LogBuymodelC4 <- glm(MaxCorr5Per ~ TCh + Month + BuyIS + SellIS, data = TrainPDB.C4, family = binomial)
  
  summary(LogBuymodelC1)
  summary(LogBuymodelC2)
  summary(LogBuymodelC3)
  summary(LogBuymodelC4)
  summary(LogBuymodelC5)

  # Predictions on training set
  
  predTrain.C1 <- predict(LogBuymodelC1, type = "response", data = TrainPDB.C1)
  predTrain.C2 <- predict(LogBuymodelC2, type = "response", data = TrainPDB.C2)
  predTrain.C3 <- predict(LogBuymodelC3, type = "response", data = TrainPDB.C3)
  predTrain.C4 <- predict(LogBuymodelC4, type = "response", data = TrainPDB.C4)

    # Confusion matrices
  
    prop.table(table(TrainPDB.C1$MaxCorr5Per, as.numeric(predTrain.C1 >= 0.5)), 1)  
    prop.table(table(TrainPDB.C2$MaxCorr5Per, as.numeric(predTrain.C2 >= 0.5)), 1)
    prop.table(table(TrainPDB.C3$MaxCorr5Per, as.numeric(predTrain.C3 >= 0.5)), 1) 
    prop.table(table(TrainPDB.C4$MaxCorr5Per, as.numeric(predTrain.C4 >= 0.5)), 1)
    
      # ROC Curve per cluster
    
      ROCPred <- prediction(predTrain.C4, TrainPDB.C4$MaxCorr5Per)
      ROCPerf <- performance(ROCPred, "tpr", "fpr")
      plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))

    # Overall confusion matrix
    AllTrainPredictions = c(predTrain.C1, predTrain.C2, predTrain.C3, predTrain.C4)
    AllTrainOutcomes = c(TrainPDB.C1$MaxCorr5Per, TrainPDB.C2$MaxCorr5Per, TrainPDB.C3$MaxCorr5Per, TrainPDB.C4$MaxCorr5Per)
    
    table(AllTrainOutcomes, as.numeric(AllTrainPredictions > 0.55))
    (240+383) / length(AllTrainOutcomes)
    # In sample accuracy: 70.48%
    
    # The baseline model has an accuracy of 59.73% 
    
    prop.table(table(AllTrainOutcomes, as.numeric(AllTrainPredictions > 0.55)), 1)
    # Sensitivity: 72.54%
    # Specificity: 67.42%
    
    prop.table(table(AllTrainOutcomes, as.numeric(AllTrainPredictions > 0.55)), 2)
    # Profit: 76.75%
    
    # ROC Curve (Optimum threshold: 0.55)
    
    library(ROCR)
    
    ROCPred <- prediction(AllTrainPredictions, AllTrainOutcomes)
    ROCPerf <- performance(ROCPred, "tpr", "fpr")
    plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))
    
    # AUC: 80.38
    
    as.numeric(performance(ROCPred, "auc")@y.values)

  # Predictions on test set
  
  predTest.C1 <- predict(LogBuymodelC1, type = "response", newdata = na.omit(TestPDB.C1))
  predTest.C2 <- predict(LogBuymodelC2, type = "response", newdata = na.omit(TestPDB.C2))
  predTest.C3 <- predict(LogBuymodelC3, type = "response", newdata = na.omit(TestPDB.C3))
  predTest.C4 <- predict(LogBuymodelC4, type = "response", newdata = na.omit(TestPDB.C4))

    # Confusion matrices
  
    table(na.omit(TestPDB.C1$MaxCorr5Per), as.numeric(predTest.C1 >= 0.5))  
    table(na.omit(TestPDB.C2$MaxCorr5Per), as.numeric(predTest.C2 >= 0.5))
    table(na.omit(TestPDB.C3$MaxCorr5Per), as.numeric(predTest.C3 >= 0.5)) 
    table(na.omit(TestPDB.C4$MaxCorr5Per), as.numeric(predTest.C4 >= 0.5)) 

    # Overall confusion matrix
    AllPredictions = c(predTest.C1, predTest.C2, predTest.C3, predTest.C4)
    AllOutcomes = c(na.omit(TestPDB.C1$MaxCorr5Per), na.omit(TestPDB.C2$MaxCorr5Per), na.omit(TestPDB.C3$MaxCorr5Per), na.omit(TestPDB.C4$MaxCorr5Per))
  
    table(AllOutcomes, as.numeric(AllPredictions > 0.55))
    (104+155) / length(AllOutcomes)
    # Out-of-sample accuracy: 68.52%
    
    # The baseline model has an accuracy of 59.79%.
    
    prop.table(table(AllOutcomes, as.numeric(AllPredictions > 0.55)), 1)
    # Sensitivity: 68.58%
    # Specificity: 68.42%
    
    prop.table(table(AllOutcomes, as.numeric(AllPredictions > 0.55)), 2)
    # Profit: 76.35%
    
    # Test set AUC: 72.78 
    
    ROCPredTest <- prediction(AllPredictions, AllOutcomes)
    as.numeric(performance(ROCPredTest, "auc")@y.values)
    