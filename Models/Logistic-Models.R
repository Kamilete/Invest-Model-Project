# Binary variable for % change momentum correction in 10 days greater than +/-5%:

PD$MaxCorr5Per <- as.integer(ifelse(PD$Strategy == "Buy", ifelse(PD$MaxCorr10D >= 0.05, 1, 0),
                                    ifelse(PD$Strategy == "Sell" & PD$MaxCorr10D <= -0.05, 1, 0)))
# Subsetting the data

PDB <- subset(PD, Strategy == "Buy")
PDS <- subset(PD, Strategy == "Sell")

# Creating the test data set: By randomly splitting data, 70% of the data in the training data set.

# install.packages("caTools")
library(caTools)

set.seed(100)
spl = sample.split(PDB$MaxCorr5Per, SplitRatio = 0.7)

TrainPDB <- subset(PDB, spl == TRUE)
TestPDB <- subset(PDB, spl == FALSE)

set.seed(100)
spl = sample.split(PDS$MaxCorr5Per, SplitRatio = 0.7)

TrainPDS <- subset(PDS, spl == TRUE)
TestPDS <- subset(PDS, spl == FALSE)

# Correlations

CorPDB <- subset(TrainPDB, select = c(DT:DVolUp,MonthN:QuarterN,TotIS:ChngSP500D5,ChngSP500:MaxCorr5Per))
CorPDS <- subset(TrainPDS, select = c(DT:DVolUp,MonthN:QuarterN,TotIS:ChngSP500D5,ChngSP500:MaxCorr5Per))

cor(CorPDB, use = "pairwise.complete.obs")
cor(CorPDS, use = "pairwise.complete.obs")

# Logistic buy model for PD Training Set

  # Bivariate models 

  summary(glm(MaxCorr5Per ~ DT, data = TrainPDB, family = binomial))
    
    # Significant variables: TCh, DVolUp, MomentumSP500, Month, Weekday, Quarter, VIX.Close, ChngVIXD5, TotIS and BuyIS
  
  # Multivariate Model 
  
  summary(glm(MaxCorr5Per ~ DT + TCh + DM + VolUp + DVolUp + MomentumSP500 + Month + Weekday + VIX.Close + ChngVIXD5 + BuyIS + SellIS, data = TrainPDB, family = binomial))
  
  LogistBuymodel1 <- glm(MaxCorr5Per ~ DT + TCh + DVolUp + MomentumSP500 + Month + VIX.Close + BuyIS, data = TrainPDB, family = binomial)
  summary(LogistBuymodel1)
  # Missing: + SellIS
  
  # Automatically Building the Model with the Akaike information criterion (AIC)
  
  LogModel <- glm(MaxCorr5Per ~ DT + TCh + DM + VolUp + DVolUp + MomentumSP500 + Month + Weekday + VIX.Close + ChngVIXD5 + BuyIS + SellIS, data = TrainPDB, family = binomial)
  
  BuymodelAIC <- step(LogModel)
  summary(BuymodelAIC)	
  
  # Make predictions on training set

  predTrainB <- predict(LogistBuymodel1, type="response")
  summary(predTrainB)
  tapply(predTrainB, TrainPDB$MaxCorr5Per, mean)

  # Confusion matrix for a threshold of 0.53
  table(TrainPDB$MaxCorr5Per, as.numeric(predTrainB >= 0.53))

    prop.table(table(TrainPDB$MaxCorr5Per, as.numeric(predTrainB >= 0.53)), 1)
    # Sensitivity: 72.78%
    # Specificity: 68.99%

    # Overall accuracy of 71.25%
    (247+385)/nrow(TrainPDB)

    prop.table(table(TrainPDB$MaxCorr5Per, as.numeric(predTrainB >= 0.53)), 2)
    # Profit: 77.62%

  # Accuracy of the baseline model on the training set

  table(TrainPDB$MaxCorr5Per)
  # A correction greater than 5% is the most frequent outcome 
  529/(358+529)
    # The baseline model has an accuracy of 59.64%  

  # ROC Curve

  library(ROCR)
  
  ROCPred <- prediction(predTrainB, TrainPDB$MaxCorr5Per)
  ROCPerf <- performance(ROCPred, "tpr", "fpr")
  plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))

  # AUC: 79.14

  as.numeric(performance(ROCPred, "auc")@y.values)

  # Predictions on test set

  predTestB <- predict(LogistBuymodel1, type = "response", newdata = na.omit(TestPDB))

  # Confusion matrix with a threshold of 0.53
  table(na.omit(TestPDB$MaxCorr5Per), as.numeric(predTestB >= 0.53))

    prop.table(table(na.omit(TestPDB$MaxCorr5Per), as.numeric(predTestB >= 0.53)),1)
    # Sensitivity: 66.08%
    # Specificity: 68.18%

    # Out-of-sample accuracy: 66.93%
    (105+150)/nrow(na.omit(TestPDB))

    prop.table(table(na.omit(TestPDB$MaxCorr5Per), as.numeric(predTestB >= 0.53)),2)
    # Profit: 75.37%

  # Accuracy of the baseline model on the test set

  table(TestPDB$MaxCorr5Per)
  227/(154+227)  
    # The baseline model has an accuracy of 59.58%.  

  # Test set AUC: 75.04

  ROCpredTestB <- prediction(predTestB, TestPDB$MaxCorr5Per[!is.na(TestPDB$MaxCorr5Per)])
  as.numeric(performance(ROCpredTestB, "auc")@y.values)

# Logistic sell model for PD Training Set

  # Bivariate models 

  summary(glm(MaxCorr5Per ~ VIX.Close, data = TrainPDS, family = binomial))

    # Significant variables:  DT, MomentumCh, VIX.Close and BuyIS

  # Multivariate Model
  
  summary(glm(MaxCorr5Per ~ DT + MomentumCh + DM + VolUp + DVolUp + VIX.Close + ChngSP500 + ChngVIXD5 + Month + Weekday + BuyIS + SellIS, data = TrainPDS, family = binomial))

  LogistSellmodel1 <- glm(MaxCorr5Per ~ MomentumCh + VIX.Close + Month, data = TrainPDS, family = binomial)
  summary(LogistSellmodel1)
  # Missing: + DT + BuyIS - Month
  
  # Automatically Building the Model with the Akaike information criterion (AIC)
  
  LogModel <- glm(MaxCorr5Per ~ DT + MomentumCh + DM + VolUp + DVolUp + VIX.Close + ChngSP500 + ChngVIXD5 + Month + Weekday + BuyIS + SellIS, data = TrainPDS, family = binomial)
  
  BuymodelAIC <- step(LogModel)
  summary(BuymodelAIC)	

  # Make predictions on training set

  predTrainS <- predict(LogistSellmodel1, type="response")
  summary(predTrainS)
  tapply(predTrainS, TrainPDS$MaxCorr5Per, mean)

  # Confusion matrix for a threshold of 0.5
  table(TrainPDS$MaxCorr5Per, as.numeric(predTrainS >= 0.5))

    prop.table(table(TrainPDS$MaxCorr5Per, as.numeric(predTrainS >= 0.5)), 1)
    # Sensitivity: 42.34%
    # Specificity: 88.29%

    # Overall accuracy of 71.16
    (407+116)/nrow(TrainPDS)

    prop.table(table(TrainPDS$MaxCorr5Per, as.numeric(predTrainS >= 0.5)), 2)
    # Profit: 68.24%

  # Accuracy of the baseline model on the training set

  table(TrainPDS$MaxCorr5Per)
  # A correction lower than -5% is the most frequent outcome  
  461/(461+274)
  # The baseline model has an accuracy of 62.72%.  

  # ROC Curve

  library(ROCR)

  ROCPred <- prediction(predTrainS, TrainPDS$MaxCorr5Per)
  ROCPerf <- performance(ROCPred, "tpr", "fpr")
  plot(ROCPerf, print.cutoffs.at=seq(0,1,by=0.05), text.adj=c(-0.2,1.7))

  # AUC: 75.40

  as.numeric(performance(ROCPred, "auc")@y.values)

  # Predictions on test set

  predTestS <- predict(LogistSellmodel1, type = "response", newdata = na.omit(TestPDS))

  # Confusion matrix with a threshold of 0.5
  table(na.omit(TestPDS$MaxCorr5Per), as.numeric(predTestS >= 0.5))

    prop.table(table(na.omit(TestPDS$MaxCorr5Per), as.numeric(predTestS >= 0.5)),1)
    # Sensitivity: 44.07%
    # Specificity: 87.82%

    # Out-of-sample accuracy: 71.43%
    (173+52)/nrow(na.omit(TestPDS))

    prop.table(table(na.omit(TestPDS$MaxCorr5Per), as.numeric(predTestS >= 0.5)),2)
    # Profit: 68.42%

  # Accuracy of the baseline model on the test set

  table(TestPDS$MaxCorr5Per)
  197/(197+118)  
    # The baseline model has an accuracy of 62.54%.  

  # Test set AUC: 76.46 

  ROCpredTestS <- prediction(predTestS, TestPDS$MaxCorr5Per[!is.na(TestPDS$MaxCorr5Per)])
  as.numeric(performance(ROCpredTestS, "auc")@y.values)

# Plot of the independent variables

library(ggplot2)

ggplot(PDB, aes(x = TCh, y = VIX.Close, color = factor(MaxCorr5Per))) + geom_point() + scale_colour_manual(values = c("darkolivegreen3", "brown"))
ggplot(PDS, aes(x = MomentumCh, y = VIX.Close, color = factor(MaxCorr5Per))) + geom_point() + scale_colour_manual(values = c("darkolivegreen3", "brown"))

# Tables of independent variables

tapply(PDB$MaxCorr5Per, PDB$DT, mean, na.rm=TRUE)
tapply(PDB$MaxCorr5Per, PDB$DVolUp, mean, na.rm=TRUE)
tapply(PDB$MaxCorr5Per, PDB$MomentumSP500, mean, na.rm=TRUE)
tapply(PDB$MaxCorr5Per, PDB$Month, mean, na.rm=TRUE)
tapply(PDB$MaxCorr5Per, PDB$BuyIS, mean, na.rm=TRUE)
tapply(PDB$MaxCorr5Per, PDB$SellIS, mean, na.rm=TRUE)

tapply(PDS$MaxCorr5Per, PDS$DVolUp, mean, na.rm=TRUE)
tapply(PDS$MaxCorr5Per, PDS$MomentumSP500, mean, na.rm=TRUE)
tapply(PDS$MaxCorr5Per, PDS$Month, mean, na.rm=TRUE)
tapply(PDS$MaxCorr5Per, PDS$BuyIS, mean, na.rm=TRUE)
tapply(PDS$MaxCorr5Per, PDS$SellIS, mean, na.rm=TRUE)
tapply(PDS$MaxCorr5Per, PDS$DM, mean, na.rm=TRUE)

# Track Record Buy model

library(lubridate)

TrainPDB$pred <- predTrainB
TestPDB$pred <- predTestB

NewPDB <- rbind(TrainPDB, TestPDB)

NewPDB$Year <- year(NewPDB$Date)

  # Analysis per year

  Year <- subset(NewPDB, Year == 1990)

  (table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))[1,1]+table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))[2,2])/nrow(Year)

  table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))
  prop.table(table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53)),2)
  
  table(NewPDB$Year)

# General analysis

  YS <- c(1990:2017)
  TrackB <- matrix(rep(0, length(YS)*2), ncol = 2)
  rownames(TrackB) <- YS
  colnames(TrackB) <- c("AccuracyB","ProfitB")
  Year <- 0
  Test1 <- ""
  Test2 <- ""
  
  for (i in 1:length(YS)){
    YN <- YS[i]
    Year <- subset(NewPDB, Year == YN)
    Test1 <- class(try(table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["0","0"]))
    Test2 <- class(try(table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["1","1"]))
    if(Test1 == "try-error") {TrackB[i,1] =  table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["1","1"]/nrow(Year)
    TrackB[i,2] = 1}
    else if (Test2 == "try-error") {TrackB[i,1] = table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["0","0"]/nrow(Year)
    TrackB[i,2] = 0}
    else {TrackB[i,1] = (table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["0","0"]+table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["1","1"])/nrow(Year)
    TrackB[i,2] = table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["1","1"] / (table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["1","1"] + table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.53))["0","1"])}
  }
  
  TrackB
  
# Track Record Sell model
  
library(lubridate)
  
TrainPDS$pred <- predTrainS
TestPDS$pred <- predTestS
  
NewPDS <- rbind(TrainPDS, TestPDS)
  
NewPDS$Year <- year(NewPDS$Date)
  
  # Analysis per year
  
  Year <- subset(NewPDS, Year == 2017)
  
  (table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))[1,1]+table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))[2,2])/nrow(Year)
  
  table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))
  prop.table(table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5)),2)
  
  table(NewPDS$Year)
  
  # General analysis
  
  YS <- c(1990:2017)
  TrackS <- matrix(rep(0, length(YS)*2), ncol = 2)
  rownames(TrackS) <- YS
  colnames(TrackS) <- c("AccuracyS","ProfitS")
  Year <- 0
  Test1 <- ""
  Test2 <- ""
  
  for (i in 1:length(YS)){
    YN <- YS[i]
    Year <- subset(NewPDS, Year == YN)
    Test1 <- class(try(table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["0","0"]))
    Test2 <- class(try(table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["1","1"]))
    if(Test1 == "try-error") {TrackS[i,1] = table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["1","1"]/nrow(Year)
    TrackS[i,2] = 1}
    else if (Test2 == "try-error") {TrackS[i,1] = table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["0","0"]/nrow(Year)
    TrackS[i,2] = 0}
    else {TrackS[i,1] = (table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["0","0"]+table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["1","1"])/nrow(Year)
    TrackS[i,2] = table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["1","1"] / (table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["1","1"] + table(Year$MaxCorr5Per, as.numeric(Year$pred >= 0.5))["0","1"])}
  }
  
  TrackS
  
Track <- cbind(TrackB, TrackS)
Track
