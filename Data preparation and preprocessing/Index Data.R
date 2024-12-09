# Disabling scientific notation

options(scipen=999)

# Open the S&P 500 series from Yahoo

load("C:/Index-Data.RData")
SP500_2 <- SP500

# Open the market data files

load("C:/Daily-market-Data.RData")
load("C:/Weekly-market-Data.RData")
load("C:/Monthly-market-Data.RData")
load("C:/Quarterly-market-Data.RData")

# Reordering the data frames

SP500 <- SP500[order(SP500$date, decreasing = TRUE),]

VIX <- VIX[order(VIX$date, decreasing = TRUE),]

VXO <- VXO[order(VXO$date, decreasing = TRUE),]

PutCallRatio <- PutCallRatio[order(PutCallRatio$date, decreasing = TRUE),]

# Deleting NA price data

SP500 <- subset(SP500, !is.na(PX_LAST))

# Libraries

# For lag, zoo and na.locf:

library(zoo)

# For rowSds: 

library(matrixStats)

# For adjust:

library(RQuantLib)

# S&P 500

  # Adding columns for % change

  SP500$ChngSP500D5 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +5, na.pad=TRUE)))-1
  SP500$ChngSP500D6 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +6, na.pad=TRUE)))-1
  SP500$ChngSP500D7 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +7, na.pad=TRUE)))-1
  SP500$ChngSP500D8 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +8, na.pad=TRUE)))-1
  SP500$ChngSP500D9 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +9, na.pad=TRUE)))-1
  SP500$ChngSP500D10 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +10, na.pad=TRUE)))-1
  SP500$ChngSP500D11 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +11, na.pad=TRUE)))-1
  SP500$ChngSP500D12 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +12, na.pad=TRUE)))-1
  SP500$ChngSP500D13 <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +13, na.pad=TRUE)))-1
  
  # Momentum variables:
  
  SP500$AvgVol30D = rowMeans(coredata(lag(zoo(SP500$PX_VOLUME), k=1:30, na.pad=TRUE)))
  
  SP500$AvgVol30DSD = rowSds(coredata(lag(zoo(SP500$PX_VOLUME), k=1:30, na.pad=TRUE)))
  
  SP500$Vol30D0.75 = SP500$AvgVol30D + 0.75 * SP500$AvgVol30DSD
  
  SP500$MCloseUp = as.numeric(SP500$PX_LAST > coredata(lag(zoo(SP500$PX_LAST), +1, na.pad=TRUE)))
  
  SP500$MLow = as.numeric(SP500$PX_LOW > coredata(lag(zoo(SP500$PX_LOW), +1, na.pad=TRUE)))
  
  SP500$MCloseDown = as.numeric(SP500$PX_LAST < coredata(lag(zoo(SP500$PX_LAST), +1, na.pad=TRUE)))
  
  SP500$MHigh = as.numeric(SP500$PX_HIGH < coredata(lag(zoo(SP500$PX_HIGH), +1, na.pad=TRUE)))
  
  SP500$MVolume = as.numeric(SP500$PX_VOLUME > SP500$Vol30D0.75)
  
  SP500$Up = as.numeric(SP500$MCloseUp + SP500$MLow + SP500$MVolume == 3)
  
  SP500$Down = as.numeric(SP500$MCloseDown + SP500$MHigh + SP500$MVolume == 3)
  
  SP500$MomentumSP500 = as.factor(ifelse(rowSums(coredata(lag(zoo(SP500$Up), k=0:4, na.pad=TRUE))) == 5, "UpTrend", 
                                         ifelse(rowSums(coredata(lag(zoo(SP500$Down), k=0:4, na.pad=TRUE))) == 5, "DownTrend", "NonTrend")))
  
  # Volume against the 30 days average volume:
  
  SP500$SP500_Vol_Relatv_30D <- SP500$PX_VOLUME / SP500$AvgVol30D
  
  # Average of the volume against the 30 days average volume during the momentum pattern
  
  SP500$SP500_Mom_Vol_Relatv_30D = rowMeans(coredata(lag(zoo(SP500$SP500_Vol_Relatv_30D), k=0:4, na.pad=TRUE)))
  
  # Returns:
  
  SP500$Rt <- (SP500$PX_LAST/coredata(lag(zoo(SP500$PX_LAST), +1, na.pad=TRUE)))-1
  
  # 12 months volatility (260 working days):
  
  SP500$Volat_12m <- rowSds(coredata(lag(zoo(SP500$Rt), k=0:259, na.pad=TRUE)))
  
  # 5 year volatility (1300 working days):
  
  SP500$Volat_5y <- rowSds(coredata(lag(zoo(SP500$Rt), k=0:1299, na.pad=TRUE)))
  
  # 30 days volatility:
  
  SP500$SP500_Volat_30d <- rowSds(coredata(lag(zoo(SP500$Rt), k=0:29, na.pad=TRUE)))
  
  # 30 days volatility against the 12 months volatility:
  
  SP500$SP500_Volat_30d_Relatv_12m <- SP500$SP500_Volat_30d / SP500$Volat_12m
  
  # 30 days volatility against the 5 year volatility:
  
  SP500$SP500_Volat_30d_Relatv_5y <- SP500$SP500_Volat_30d / SP500$Volat_5y
  
  # Removing columns
  
  SP500$PX_HIGH <- NULL
  SP500$PX_LOW <- NULL
  SP500$PX_LAST <- NULL
  SP500$PX_VOLUME <- NULL
  SP500$AvgVol30D <- NULL
  SP500$AvgVol30DSD <- NULL
  SP500$Vol30D0.75 <- NULL
  SP500$MCloseUp <- NULL
  SP500$MLow <- NULL
  SP500$MCloseDown <- NULL
  SP500$MHigh <- NULL
  SP500$MVolume <- NULL
  SP500$Up <- NULL
  SP500$Down <- NULL
  SP500$Rt <- NULL
  SP500$Volat_12m <- NULL
  SP500$Volat_5y <- NULL
    
  # Filling NA missing data
  
  names(SP500_2)[11] <- "MomentumSP500_2"
  
  SP500 <- merge(SP500, SP500_2[,c(1,11)], by.x = "date", by.y = "Date", all.x = TRUE)
  SP500 <- SP500[order(SP500$date, decreasing = TRUE),]
  
  SP500$MomentumSP500 <- factor(ifelse(is.na(SP500$MomentumSP500) == TRUE, SP500$MomentumSP500_2, SP500$MomentumSP500), labels = c("DownTrend", "NonTrend", "UpTrend"))
  
  SP500$MomentumSP500_2 <- NULL
  
# VIX Index
  
  # Daily change VIX and VXO Index:
  
  VIX$Chng <- (VIX$PX_LAST/coredata(lag(zoo(VIX$PX_LAST), +1, na.pad=TRUE)))-1
  VXO$Chng <- (VXO$PX_LAST/coredata(lag(zoo(VXO$PX_LAST), +1, na.pad=TRUE)))-1
  
  # Merging VIX and VXO dates and daily changes:
  
  date <- c(VIX$date, VXO$date[7557:8566])
  PX_LAST <- c(VIX$PX_LAST, rep(NA, 1010))
  Chng <- c(VIX$Chng[1:7555], VXO$Chng[7556:8566])
  
  VIX <- data.frame(date, PX_LAST, Chng)
  
  # Estimating VIX last price:
  
  for (i in 1:nrow(VIX)){
    if(is.na(VIX$PX_LAST[i]) == TRUE){
      VIX$PX_LAST[i] <- VIX$PX_LAST[i-1] / (1 + VIX$Chng[i-1])
    }
  }
  
  VIX$PX_LAST <- round(VIX$PX_LAST, digits = 2)
  
  # Removing column
  
  VIX$Chng <- NULL

  # Adding columns for the last VIX Index before the momentum pattern
  
  VIX$VIX_LASTD5 <- coredata(lag(zoo(VIX$PX_LAST), +5, na.pad=TRUE))
  VIX$VIX_LASTD6 <- coredata(lag(zoo(VIX$PX_LAST), +6, na.pad=TRUE))
  VIX$VIX_LASTD7 <- coredata(lag(zoo(VIX$PX_LAST), +7, na.pad=TRUE))
  VIX$VIX_LASTD8 <- coredata(lag(zoo(VIX$PX_LAST), +8, na.pad=TRUE))
  VIX$VIX_LASTD9 <- coredata(lag(zoo(VIX$PX_LAST), +9, na.pad=TRUE))
  VIX$VIX_LASTD10 <- coredata(lag(zoo(VIX$PX_LAST), +10, na.pad=TRUE))
  VIX$VIX_LASTD11 <- coredata(lag(zoo(VIX$PX_LAST), +11, na.pad=TRUE))
  VIX$VIX_LASTD12 <- coredata(lag(zoo(VIX$PX_LAST), +12, na.pad=TRUE))
  VIX$VIX_LASTD13 <- coredata(lag(zoo(VIX$PX_LAST), +13, na.pad=TRUE))
  
  # 5-days moving average (Short term):
  
  VIX$Avg_VIX_5D <- rowMeans(coredata(lag(zoo(VIX$PX_LAST), k=0:4, na.pad=TRUE)))
  
  # 30-days moving average (Long term):
  
  VIX$Avg_VIX_30D <- rowMeans(coredata(lag(zoo(VIX$PX_LAST), k=0:29, na.pad=TRUE)))
  
  # 50-days moving average (Long term):
  
  VIX$Avg_VIX_50D <- rowMeans(coredata(lag(zoo(VIX$PX_LAST), k=0:49, na.pad=TRUE)))
  
  # 200-days moving average:
  
  VIX$Avg_VIX_200D <- rowMeans(coredata(lag(zoo(VIX$PX_LAST), k=0:199, na.pad=TRUE)))
  
  # Changing column name
  
  names(VIX)[2] <- "VIX"
  
# Equity Put/Call Ratio
  
  # Adding columns for % change
  
  PutCallRatio$Chng_Eq_PutCalD5 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +5, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD6 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +6, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD7 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +7, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD8 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +8, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD9 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +9, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD10 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +10, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD11 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +11, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD12 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +12, na.pad=TRUE)))-1
  PutCallRatio$Chng_Eq_PutCalD13 <- (PutCallRatio$PX_LAST/coredata(lag(zoo(PutCallRatio$PX_LAST), +13, na.pad=TRUE)))-1
  
  # 5 days moving average
  
  PutCallRatio$Avg_5d_Eq_PutCallR <- rowMeans(coredata(lag(zoo(PutCallRatio$PX_LAST), k=0:4, na.pad=TRUE)))
  
  # 30 days moving average 5 days before the start of the momentum pattern
  
  PutCallRatio$Avg_30d_Eq_PutCallR <- rowMeans(coredata(lag(zoo(PutCallRatio$PX_LAST), k=5:34, na.pad=TRUE)))
  
  # Put-call ratio - 30 days moving average ratio
  
  PutCallRatio$Eq_PutCall_Avg30_Ratio <- PutCallRatio$PX_LAST/PutCallRatio$Avg_30d_Eq_PutCallR
  
  # 5 days moving average - 30 days moving average ratio
  
  PutCallRatio$Eq_PutCall_Avg5_30_Ratio <- PutCallRatio$Avg_5d_Eq_PutCallR/PutCallRatio$Avg_30d_Eq_PutCallR
  
  # Changing column name
  
  names(PutCallRatio)[2] <- "Eq_PutCall"
  
  # Removing columns
  
  PutCallRatio$Avg_30d_Eq_PutCallR <- NULL

# US Investor sentiment
  
  # Changing column names
  
  names(BearSent)[2] <- "Bearish"
  names(BullSent)[2] <- "Bullish"
  names(NeutralSent)[2] <- "Neutral"
  
  # Merging
  
  Sent <- merge(BullSent, NeutralSent)
  Sent <- merge(Sent, BearSent)
  Sent <- Sent[order(Sent$date, decreasing = TRUE),]
  
  # 8-week Mov Avg:

  Sent$Bull_Avg <- round(rowMeans(coredata(lag(zoo(Sent$Bullish), k=0:7, na.pad=TRUE))), digits = 2)
  Sent$Bear_Avg <- round(rowMeans(coredata(lag(zoo(Sent$Bearish), k=0:7, na.pad=TRUE))), digits = 2)
  
  # Bearish/Bullish sentiment against the 8-week Mov Avg:
  
  Sent$Bull_Relatv_Avg <- Sent$Bullish / Sent$Bull_Avg
  Sent$Bear_Relatv_Avg <- Sent$Bearish / Sent$Bear_Avg
  
  # Bull-Bear Ratio:
  
  Sent$Bull_Bear_R <- Sent$Bullish / Sent$Bearish
  
  # Removing columns:
  
  Sent$Bull_Avg <- NULL
  Sent$Bear_Avg <- NULL
  
  # Realeses in holidays are moved to the following business day:
  
  Sent$date <- adjust("UnitedStates/NYSE", Sent$date)
  
  # Full data frame since 1987-07-16:
  
  load("C:/Daily Equity Data/IBM_US_D.RData")
  
  Sent <- merge(DF[c(1:8182),c(1,2)], Sent, by = "date", all.x = TRUE)
  Sent <- Sent[order(Sent$date, decreasing = TRUE),]
  
  Sent$Close_unadj <- NULL
  
  Sent$Bullish <- na.locf(Sent$Bullish, na.rm = FALSE, fromLast = TRUE)
  Sent$Neutral <- na.locf(Sent$Neutral, na.rm = FALSE, fromLast = TRUE)
  Sent$Bearish <- na.locf(Sent$Bearish, na.rm = FALSE, fromLast = TRUE)
  Sent$Bull_Relatv_Avg <- na.locf(Sent$Bull_Relatv_Avg, na.rm = FALSE, fromLast = TRUE)
  Sent$Bear_Relatv_Avg <- na.locf(Sent$Bear_Relatv_Avg, na.rm = FALSE, fromLast = TRUE)
  Sent$Bull_Bear_R <- na.locf(Sent$Bull_Bear_R, na.rm = FALSE, fromLast = TRUE)
  
  Sent$Bullish <- ifelse(is.na(Sent$Bullish) == TRUE, na.locf(Sent$Bullish, na.rm = FALSE), Sent$Bullish)
  Sent$Neutral <- ifelse(is.na(Sent$Neutral) == TRUE, na.locf(Sent$Neutral, na.rm = FALSE), Sent$Neutral)
  Sent$Bearish <- ifelse(is.na(Sent$Bearish) == TRUE, na.locf(Sent$Bearish, na.rm = FALSE), Sent$Bearish)
  Sent$Bull_Bear_R <- ifelse(is.na(Sent$Bull_Bear_R) == TRUE, na.locf(Sent$Bull_Bear_R, na.rm = FALSE), Sent$Bull_Bear_R)
  
# U.S. Recession Indicator
  
  # Changing column name
  
  names(USRecession)[2] <- "Recession_Ind"
  
  # Realeses in holidays are moved to the following business day:
  
  USRecession$date <- adjust("UnitedStates/NYSE", USRecession$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  USRecession <- merge(DF[,c(1,2)], USRecession, by = "date", all.x = TRUE)
  USRecession <- USRecession[order(USRecession$date, decreasing = TRUE),]
  
  USRecession$Close_unadj <- NULL
  
  USRecession$Recession_Ind <- na.locf(USRecession$Recession_Ind, na.rm = FALSE, fromLast = TRUE)
  
# Nonfarm payrolls
  
  # Changing columns
  
  NonfarmPayRelease$date <- NULL
  names(NonfarmPayRelease)[1] <- "date"
  NonfarmPayRelease$Nonfarm_Past_DT <- NonfarmPayRelease$date
  NonfarmPayRelease$Nonfarm_Next_DT <- NonfarmPayRelease$date
  
  # Realeses in holidays are moved to the following business day:
  
  NonfarmPayRelease$date <- adjust("UnitedStates/NYSE", NonfarmPayRelease$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  NonfarmPayRelease <- merge(DF[,c(1,2)], NonfarmPayRelease, by = "date", all.x = TRUE)
  NonfarmPayRelease <- NonfarmPayRelease[order(NonfarmPayRelease$date, decreasing = TRUE),]
  
  NonfarmPayRelease$Close_unadj <- NULL
  
  NonfarmPayRelease$Nonfarm_Past_DT <- na.locf(NonfarmPayRelease$Nonfarm_Past_DT, na.rm = FALSE, fromLast = TRUE)
  NonfarmPayRelease$Nonfarm_Next_DT <- na.locf(NonfarmPayRelease$Nonfarm_Next_DT, na.rm = FALSE)
  
# Housing starts
  
  # Changing columns
  
  HouseStartRelease$date <- NULL
  names(HouseStartRelease)[1] <- "date"
  HouseStartRelease$HouseStart_Past_DT <- HouseStartRelease$date
  HouseStartRelease$HouseStart_Next_DT <- HouseStartRelease$date
  
  # Realeses in holidays are moved to the following business day:
  
  HouseStartRelease$date <- adjust("UnitedStates/NYSE", HouseStartRelease$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  HouseStartRelease <- merge(DF[,c(1,2)], HouseStartRelease, by = "date", all.x = TRUE)
  HouseStartRelease <- HouseStartRelease[order(HouseStartRelease$date, decreasing = TRUE),]
  
  HouseStartRelease$Close_unadj <- NULL
  
  HouseStartRelease$HouseStart_Past_DT <- na.locf(HouseStartRelease$HouseStart_Past_DT, na.rm = FALSE, fromLast = TRUE)
  HouseStartRelease$HouseStart_Next_DT <- na.locf(HouseStartRelease$HouseStart_Next_DT, na.rm = FALSE)
  
  # Removing duplicate observation in Housing starts
  
  HouseStartRelease <- unique(HouseStartRelease)
  
# CPI
  
  # Changing columns
  
  CPIRelease$date <- NULL
  names(CPIRelease)[1] <- "date"
  CPIRelease$CPI_Past_DT <- CPIRelease$date
  CPIRelease$CPI_Next_DT <- CPIRelease$date
  
  # Realeses in holidays are moved to the following business day:
  
  CPIRelease$date <- adjust("UnitedStates/NYSE", CPIRelease$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  CPIRelease <- merge(DF[,c(1,2)], CPIRelease, by = "date", all.x = TRUE)
  CPIRelease <- CPIRelease[order(CPIRelease$date, decreasing = TRUE),]
  
  CPIRelease$Close_unadj <- NULL
  
  CPIRelease$CPI_Past_DT <- na.locf(CPIRelease$CPI_Past_DT, na.rm = FALSE, fromLast = TRUE)
  CPIRelease$CPI_Next_DT <- na.locf(CPIRelease$CPI_Next_DT, na.rm = FALSE)
  
# FOMC rate
  
  # Changing columns
  
  FOMCRateRelease$date <- NULL
  names(FOMCRateRelease)[1] <- "date"
  FOMCRateRelease$FED_Rate_Past_DT <- FOMCRateRelease$date
  FOMCRateRelease$FED_Rate_Next_DT <- FOMCRateRelease$date
  
  # Realeses in holidays are moved to the following business day:
  
  FOMCRateRelease$date <- adjust("UnitedStates/NYSE", FOMCRateRelease$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  FOMCRateRelease <- merge(DF[,c(1,2)], FOMCRateRelease, by = "date", all.x = TRUE)
  FOMCRateRelease <- FOMCRateRelease[order(FOMCRateRelease$date, decreasing = TRUE),]
  
  FOMCRateRelease$Close_unadj <- NULL
  
  FOMCRateRelease$FED_Rate_Past_DT <- na.locf(FOMCRateRelease$FED_Rate_Past_DT, na.rm = FALSE, fromLast = TRUE)
  FOMCRateRelease$FED_Rate_Next_DT <- na.locf(FOMCRateRelease$FED_Rate_Next_DT, na.rm = FALSE)
  
# PMI index
  
  # Changing columns
  
  PMI$ECO_RELEASE_DT <- as.Date(ifelse(is.na(PMI$ECO_RELEASE_DT) == TRUE, PMI$date + 1, PMI$ECO_RELEASE_DT))
  PMI$date <- NULL
  names(PMI)[1] <- "PMI"
  names(PMI)[2] <- "date"
  PMI$ChngPMI <- PMI$PMI - coredata(lag(zoo(PMI$PMI), -1, na.pad=TRUE))
  PMI$PMI_Past_DT <- PMI$date
  PMI$PMI_Next_DT <- PMI$date
  
  # Realeses in holidays are moved to the following business day:
  
  PMI$date <- adjust("UnitedStates/NYSE", PMI$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  PMI <- merge(DF[,c(1,2)], PMI, by = "date", all.x = TRUE)
  PMI <- PMI[order(PMI$date, decreasing = TRUE),]
  
  PMI$Close_unadj <- NULL
  
  PMI$PMI <- na.locf(PMI$PMI, na.rm = FALSE, fromLast = TRUE)
  PMI$ChngPMI <- na.locf(PMI$ChngPMI, na.rm = FALSE, fromLast = TRUE)
  PMI$PMI_Past_DT <- na.locf(PMI$PMI_Past_DT, na.rm = FALSE, fromLast = TRUE)
  PMI$PMI_Next_DT <- na.locf(PMI$PMI_Next_DT, na.rm = FALSE)
  
# NY Fed prob of recession in US twelve months ahead
  
  # Changing columns
  
  names(RecessionProb)[2] <- "RecessProb"
  RecessionProb$RecessProbCh <- RecessionProb$RecessProb - coredata(lag(zoo(RecessionProb$RecessProb), -1, na.pad=TRUE))

  # Realeses in holidays are moved to the following business day:
  
  RecessionProb$date <- adjust("UnitedStates/NYSE", RecessionProb$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  RecessionProb <- merge(DF[,c(1,2)], RecessionProb, by = "date", all.x = TRUE)
  RecessionProb <- RecessionProb[order(RecessionProb$date, decreasing = TRUE),]
  
  RecessionProb$Close_unadj <- NULL
  
  RecessionProb$RecessProb <- na.locf(RecessionProb$RecessProb, na.rm = FALSE, fromLast = TRUE)
  RecessionProb$RecessProbCh <- na.locf(RecessionProb$RecessProbCh, na.rm = FALSE, fromLast = TRUE)

# Sentix Economic US Aggregate overall Index (Sentix Behavioral Index)
  
  # Changing columns
  
  names(BehavIndex)[2] <- "Sentix"
  BehavIndex$SentixCh <- (BehavIndex$Sentix / coredata(lag(zoo(BehavIndex$Sentix), -1, na.pad=TRUE))) - 1
  BehavIndex$SentixCh_12m <- (BehavIndex$Sentix / coredata(lag(zoo(BehavIndex$Sentix), -12, na.pad=TRUE))) - 1
  
  # Realeses in holidays are moved to the following business day:
  
  BehavIndex$date <- adjust("UnitedStates/NYSE", BehavIndex$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  BehavIndex <- merge(DF[,c(1,2)], BehavIndex, by = "date", all.x = TRUE)
  BehavIndex <- BehavIndex[order(BehavIndex$date, decreasing = TRUE),]
  
  BehavIndex$Close_unadj <- NULL
  
  BehavIndex$Sentix <- na.locf(BehavIndex$Sentix, na.rm = FALSE, fromLast = TRUE)
  BehavIndex$SentixCh <- na.locf(BehavIndex$SentixCh, na.rm = FALSE, fromLast = TRUE)
  BehavIndex$SentixCh_12m <- na.locf(BehavIndex$SentixCh_12m, na.rm = FALSE, fromLast = TRUE)
  
# NYSE Margin Debt
  
  MarginDebt2 <- read.csv2("C:/Market Data/FINRA Margin Debt.csv")
  MarginDebt2$date <- as.Date(MarginDebt2$date)
  MarginDebt1 <- MarginDebt
  
  # Reordering the data frames
  
  MarginDebt1 <- MarginDebt1[order(MarginDebt1$date, decreasing = TRUE),]
  MarginDebt2 <- MarginDebt2[order(MarginDebt2$date, decreasing = TRUE),]
  
  # Changing column name
  
  names(MarginDebt1)[2] <- "Margin_Debt"
  
  # Monthly change:
  
  MarginDebt1$Chng <- (MarginDebt1$Margin_Debt/coredata(lag(zoo(MarginDebt1$Margin_Debt), +1, na.pad=TRUE)))-1
  MarginDebt2$Chng <- (MarginDebt2$Margin_Debt/coredata(lag(zoo(MarginDebt2$Margin_Debt), +1, na.pad=TRUE)))-1
  
  # Merging NYSE Margin Debt, FINRA Margin Debt dates and monthly changes:
  
  date <- c(MarginDebt2$date, MarginDebt1$date[95:383])
  Margin_Debt <- c(MarginDebt2$Margin_Debt, rep(NA, 289))
  Margin_Debt_Chng <- c(MarginDebt2$Chng[1:125], MarginDebt1$Chng[94:383])
  
  MarginDebt <- data.frame(date, Margin_Debt, Margin_Debt_Chng)
  
  # Estimating last Margin Debt:
  
  for (i in 1:nrow(MarginDebt)){
    if(is.na(MarginDebt$Margin_Debt[i]) == TRUE){
      MarginDebt$Margin_Debt[i] <- MarginDebt$Margin_Debt[i-1] / (1 + MarginDebt$Margin_Debt_Chng[i-1])
    }
  }
  
  MarginDebt$Margin_Debt <- round(MarginDebt$Margin_Debt, digits = 0)
  
  # Past 12 months Average Monthly change:
  
  MarginDebt$Avg_12m_Margin_Debt <- rowMeans(coredata(lag(zoo(MarginDebt$Margin_Debt_Chng), k=1:12, na.pad=TRUE)))
  
  # Monthly change - Past 12 months Average Monthly change spread:
  
  MarginDebt$Margin_Debt_Chng_12m_Spread <- MarginDebt$Margin_Debt_Chng - MarginDebt$Avg_12m_Margin_Debt
  
  # Yearly percent change - Margin Debt Acceleration:
  
  MarginDebt$Margin_Debt_Chng_1y <- (MarginDebt$Margin_Debt/coredata(lag(zoo(MarginDebt$Margin_Debt), +12, na.pad=TRUE)))-1
  
  # Realeses in holidays are moved to the following business day:
  
  MarginDebt$date <- adjust("UnitedStates/NYSE", MarginDebt$date)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  MarginDebt <- merge(DF[,c(1,2)], MarginDebt, by = "date", all.x = TRUE)
  MarginDebt <- MarginDebt[order(MarginDebt$date, decreasing = TRUE),]
  
  MarginDebt$Close_unadj <- NULL
  MarginDebt$Margin_Debt <- NULL
  MarginDebt$Avg_12m_Margin_Debt <- NULL
  
  MarginDebt$Margin_Debt_Chng <- na.locf(MarginDebt$Margin_Debt_Chng, na.rm = FALSE, fromLast = TRUE)
  MarginDebt$Margin_Debt_Chng_12m_Spread <- na.locf(MarginDebt$Margin_Debt_Chng_12m_Spread, na.rm = FALSE, fromLast = TRUE)
  MarginDebt$Margin_Debt_Chng_1y <- na.locf(MarginDebt$Margin_Debt_Chng_1y, na.rm = FALSE, fromLast = TRUE)
  
# Economic growth
  
  # Changing columns:
  
  names(RealGDP)[2] <- "Real_GDP_Growth"
  RealGDP$date <- RealGDP$date + 29
  
  # Recession dummy variable:
  # A recession is defined as two successive quarters of negative real GDP growth. 
  
  RealGDP$Recession_GDP <- ifelse(RealGDP$Real_GDP_Growth < 0 & coredata(lag(zoo(RealGDP$Real_GDP_Growth), -1, na.pad=TRUE)) < 0, 1, 0)
  
  # Past 2 Years Average Rate:
  
  RealGDP$Avg_2y_Real_GDP <- rowMeans(coredata(lag(zoo(RealGDP$Real_GDP_Growth), k=0:-7, na.pad=TRUE)))
  
  # GDP Growth Rate - Past 2 Years Average Rate spread:
  
  RealGDP$Real_GDP_2y_Spread <- RealGDP$Real_GDP_Growth - RealGDP$Avg_2y_Real_GDP
  
  # Realeses in holidays are moved to the preceding business day:
  
  RealGDP$date <- adjust("UnitedStates/NYSE", RealGDP$date, bdc = 2)
  
  # Full data frame since 1986-01-01:
  
  load("C:/IBM_US_D.RData")
  
  RealGDP <- merge(DF[,c(1,2)], RealGDP, by = "date", all.x = TRUE)
  RealGDP <- RealGDP[order(RealGDP$date, decreasing = TRUE),]
  
  RealGDP$Close_unadj <- NULL
  RealGDP$Avg_2y_Real_GDP <- NULL
  
  RealGDP$Real_GDP_Growth <- na.locf(RealGDP$Real_GDP_Growth, na.rm = FALSE, fromLast = TRUE)
  RealGDP$Real_GDP_2y_Spread <- na.locf(RealGDP$Real_GDP_2y_Spread, na.rm = FALSE, fromLast = TRUE)
  RealGDP$Recession_GDP <- na.locf(RealGDP$Recession_GDP, na.rm = FALSE, fromLast = TRUE)
  
# Saving indices

save(SP500, VIX, PutCallRatio, Sent, USRecession, NonfarmPayRelease, HouseStartRelease, 
     CPIRelease, FOMCRateRelease, PMI, RecessionProb, BehavIndex, MarginDebt, RealGDP, file="Market-Data.RData")

load(file="Market-Data.RData")
