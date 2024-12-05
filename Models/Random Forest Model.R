# Convert outcome to factor

PD$MaxCorr5Per <- as.factor(PD$MaxCorr5Per)

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

# Cross-validation to identify the optimal model

library(caret)
library(e1071)

set.seed(100)

numFolds <- trainControl(method = "cv", number = 10)

rf_model = train(MaxCorr5Per ~ TCh + VIX.Close + ChngVIXD5 + BuyIS, data = TrainPDB, method="rf", trControl = numFolds, prox=TRUE, allowParallel=TRUE)

BuyRForest2 <- rf_model$finalModel

# Random Forest buy model for PD Training Set

  library(randomForest)

  set.seed(100)

  BuyRForest1 <- randomForest(MaxCorr5Per ~ DT + TCh + DM + VolUp + DVolUp + VIX.Close + ChngVIXD5 + BuyIS + SellIS, data = TrainPDB, ntree = 500, nodesize = 35)

  # Significant variables: TCh, VIX.Close, ChngVIXD5, BuyIS, DT and DVolUp

  # Variables that are the most important in terms of the number of splits

  vu <- varUsed(BuyRForest1, count=TRUE)
  vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
  dotchart(vusorted$x, names(BuyRForest1$forest$xlevels[vusorted$ix]))

  # Variables that are the most important in terms of mean reduction in impurity

  varImpPlot(BuyRForest1)

  # Predictions on training set

  PredictForest <- predict(BuyRForest2)
  PredForest <- predict(BuyRForest2, type="prob")[,2]
  tapply(PredForest, TrainPDB$MaxCorr5Per, mean)

  # Confusion matrix for a threshold of 0.56

  table(TrainPDB$MaxCorr5Per, as.numeric(PredForest >= 0.56))

    prop.table(table(TrainPDB$MaxCorr5Per, as.numeric(PredForest >= 0.56)), 1)
    # Sensitivity: 64.96%
    # Specificity: 61.8%

    # Overall accuracy of 63.69%
    (220+343)/nrow(TrainPDB)
    # The baseline model has an accuracy of 59.73%

    prop.table(table(TrainPDB$MaxCorr5Per, as.numeric(PredForest >= 0.56)), 2)
    # Profit: 71.61%
  
  # ROC Curve
  
  library(ROCR)
  
  ROCPred <- prediction(PredForest, TrainPDB$MaxCorr5Per)
  ROCPerf <- performance(ROCPred, "tpr", "fpr")
  plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))
  
  # AUC: 72.22
  
  as.numeric(performance(ROCPred, "auc")@y.values)
  
  # Predictions on test set
  
  PredTestForest <- predict(BuyRForest2, newdata = na.omit(TestPDB))
  PredTForest <- predict(BuyRForest2, newdata = na.omit(TestPDB), type="prob")[,2]

  # Confusion matrix
  
  table(na.omit(TestPDB$MaxCorr5Per), as.numeric(PredTForest >= 0.56))
  
    prop.table(table(na.omit(TestPDB$MaxCorr5Per), as.numeric(PredTForest >= 0.56)), 1)
    # Sensitivity: 70.8%
    # Specificity: 69.08%
  
    # Out-of-sample accuracy: 70.11%
    (105+160)/nrow(na.omit(TestPDB))
    # The baseline model has an accuracy of 59.79%
  
    prop.table(table(na.omit(TestPDB$MaxCorr5Per), as.numeric(PredTForest >= 0.56)), 2)
    # Profit: 77.29%
  
  # Test set AUC: 76.38
  
  ROCPredTest <- prediction(PredTForest, TestPDB$MaxCorr5Per[!is.na(TestPDB$MaxCorr5Per)])
  as.numeric(performance(ROCPredTest, "auc")@y.values)
  
# Cross-validation to identify the optimal model
  
library(caret)
library(e1071)

set.seed(100)
  
numFolds <- trainControl(method = "cv", number = 10)
  
rf_model <- train(MaxCorr5Per ~ MomentumCh + VIX.Close + ChngSP500D5 + ChngVIXD5, data = na.omit(TrainPDS), method="rf", trControl = numFolds, prox=TRUE, allowParallel=TRUE)
  
SellRForest2 <- rf_model$finalModel
  
# Random Forest sell model for PD Training Set
  
  library(randomForest)
  
  set.seed(100)
  
  SellRForest1 <- randomForest(MaxCorr5Per ~ DT + MomentumCh + DM + VolUp + DVolUp + VIX.Close + ChngSP500D5 + ChngVIXD5 + BuyIS + SellIS, data = na.omit(TrainPDS), ntree = 500, nodesize = 35)
  
  # Significant variables: MomentumCh, VIX.Close, ChngSP500D5 and ChngVIXD5
  
  # Variables that are the most important in terms of the number of splits
  
  vu <- varUsed(SellRForest1, count=TRUE)
  vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
  dotchart(vusorted$x, names(SellRForest1$forest$xlevels[vusorted$ix]))
  
  # Variables that are the most important in terms of mean reduction in impurity
  
  varImpPlot(SellRForest1)
  
  # Predictions on training set
  
  PredictForest <- predict(SellRForest2)
  PredForest <- predict(SellRForest2, type="prob")[,2]
  tapply(PredForest, na.omit(TrainPDS)$MaxCorr5Per, mean)
  
  # Confusion matrix for a threshold of 0.59
  
  table(na.omit(TrainPDS)$MaxCorr5Per, as.numeric(PredForest >= 0.59))
  
  prop.table(table(na.omit(TrainPDS)$MaxCorr5Per, as.numeric(PredForest >= 0.59)), 1)
  # Sensitivity: 36.79%
  # Specificity: 85.14%
  
  # Overall accuracy of 66.62%
  (384+103)/nrow(na.omit(TrainPDS))
  # The baseline model has an accuracy of 61.61%
  
  prop.table(table(na.omit(TrainPDS)$MaxCorr5Per, as.numeric(PredForest >= 0.59)), 2)
  # Profit: 60.59%
  
  # ROC Curve
  
  library(ROCR)
  
  ROCPred <- prediction(PredForest, na.omit(TrainPDS)$MaxCorr5Per)
  ROCPerf <- performance(ROCPred, "tpr", "fpr")
  plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))
  
  # AUC: 69.17
  
  as.numeric(performance(ROCPred, "auc")@y.values)
  
  # Predictions on test set
  
  PredTestForest <- predict(SellRForest2, newdata = na.omit(TestPDS))
  PredTForest <- predict(SellRForest2, newdata = na.omit(TestPDS), type="prob")[,2]
  
  # Confusion matrix
  
  table(na.omit(TestPDS$MaxCorr5Per), as.numeric(PredTForest >= 0.59))
  
  prop.table(table(na.omit(TestPDS$MaxCorr5Per), as.numeric(PredTForest >= 0.59)), 1)
  # Sensitivity: 43.33%
  # Specificity: 88.14%
  
  # Out-of-sample accuracy: 71.02%
  (171+52)/nrow(na.omit(TestPDS))
  # The baseline model has an accuracy of 61.78%
  
  prop.table(table(na.omit(TestPDS$MaxCorr5Per), as.numeric(PredTForest >= 0.59)), 2)
  # Profit: 69.33%
  
  # Test set AUC: 74.97
  
  ROCPredTest <- prediction(PredTForest, TestPDS$MaxCorr5Per[!is.na(TestPDS$MaxCorr5Per)])
  as.numeric(performance(ROCPredTest, "auc")@y.values)
  