# Disabling scientific notation

options(scipen=999)

# The stock lists

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
load(file="Final_Stock_List.RData")
load(file="Stock_List.RData")
load(file="Static_data.RData")

library(mgsub)

NStocks <- mgsub(NStocks, pattern=c("/"," "), replacement=c("-","_"))
Stocks <- mgsub(gsub('.{7}$', '', subset(Static, GICS_SECTOR_NAME != "")$Symbol), pattern=c("/"," "), replacement=c("-","_"))

# Libraries

# For roll_cor:

library(roll)

# For lag, zoo and na.locf:

library(zoo)

# For rowSds: 

library(matrixStats)

# For businessDaysBetween and adjust:

library(RQuantLib)

# Transformation function for Daily Equity Data

xFuncT <- function(data) {
  
  # Volume against the 30 days average volume
  
  data$Vol_Relatv_30D <- data$PX_VOLUME / data$AvgVol30D
  
  # 1 standard deviations over the month average variable
  
  data$Vol30D1 = data$AvgVol30D + data$SDVol30D
  
  # The volume is a standard deviation over the month average
  
  data$MVolume1 = as.integer(data$PX_VOLUME > data$Vol30D1)
  
  # Volume condition with 1 standard deviation
  
  data$Vol1SD = as.integer(rowSums(coredata(lag(zoo(data$MVolume1), k=0:4, na.pad=TRUE)), na.rm = TRUE) == 5)
  
  # 1.25 standard deviations over the month average variable
  
  data$Vol30D1.25 = data$AvgVol30D + 1.25 * data$SDVol30D
  
  # The volume is 1.25 standard deviations over the month average
  
  data$MVolume1.25 = as.integer(data$PX_VOLUME > data$Vol30D1.25)
  
  # Volume condition with 1.25 standard deviation
  
  data$Vol1.25SD = as.integer(rowSums(coredata(lag(zoo(data$MVolume1.25), k=0:4, na.pad=TRUE)), na.rm = TRUE) == 5)
  
  # 1.5 standard deviations over the month average variable
  
  data$Vol30D1.5 = data$AvgVol30D + 1.5 * data$SDVol30D
  
  # The volume is 1.5 standard deviations over the month average
  
  data$MVolume1.5 = as.integer(data$PX_VOLUME > data$Vol30D1.5)
  
  # Volume condition with 1.5 standard deviation
  
  data$Vol1.5SD = as.integer(rowSums(coredata(lag(zoo(data$MVolume1.5), k=0:4, na.pad=TRUE)), na.rm = TRUE) == 5)
  
  # Days in a trend (Strong): DT
  
  data$DT = as.integer(ifelse(data$Strategy == "Buy", ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:15, na.pad=TRUE)), na.rm = TRUE) == 11, 16,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:14, na.pad=TRUE)), na.rm = TRUE) == 10, 15,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:13, na.pad=TRUE)), na.rm = TRUE) == 9, 14,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:12, na.pad=TRUE)), na.rm = TRUE) == 8, 13,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:11, na.pad=TRUE)), na.rm = TRUE) == 7, 12,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:10, na.pad=TRUE)), na.rm = TRUE) == 6, 11,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:9, na.pad=TRUE)), na.rm = TRUE) == 5, 10,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:8, na.pad=TRUE)), na.rm = TRUE) == 4, 9,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:7, na.pad=TRUE)), na.rm = TRUE) == 3, 8,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MCloseDown), k=5:6, na.pad=TRUE)), na.rm = TRUE) == 2, 7,
                                                      ifelse(coredata(lag(zoo(data$MCloseDown), +5, na.pad=TRUE)) == 1, 6, 5))))))))))), 
                              ifelse(data$Strategy == "Sell", ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:17, na.pad=TRUE)), na.rm = TRUE) == 13, 18,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:16, na.pad=TRUE)), na.rm = TRUE) == 12, 17,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:15, na.pad=TRUE)), na.rm = TRUE) == 11, 16,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:14, na.pad=TRUE)), na.rm = TRUE) == 10, 15,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:13, na.pad=TRUE)), na.rm = TRUE) == 9, 14,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:12, na.pad=TRUE)), na.rm = TRUE) == 8, 13,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:11, na.pad=TRUE)), na.rm = TRUE) == 7, 12,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:10, na.pad=TRUE)), na.rm = TRUE) == 6, 11,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:9, na.pad=TRUE)), na.rm = TRUE) == 5, 10,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:8, na.pad=TRUE)), na.rm = TRUE) == 4, 9,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:7, na.pad=TRUE)), na.rm = TRUE) == 3, 8,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MCloseUp), k=5:6, na.pad=TRUE)), na.rm = TRUE) == 2, 7,
                                                              ifelse(coredata(lag(zoo(data$MCloseUp), +5, na.pad=TRUE)) == 1, 6, 5))))))))))))),0)))
  
  # % change in a trend (Strong): TCh
  
  data$TCh = ifelse(data$DT == 18, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +18, na.pad=TRUE)))-1,
                    ifelse(data$DT == 17, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +17, na.pad=TRUE)))-1,
                    ifelse(data$DT == 16, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +16, na.pad=TRUE)))-1,
                    ifelse(data$DT == 15, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +15, na.pad=TRUE)))-1, 
                    ifelse(data$DT == 14, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +14, na.pad=TRUE)))-1,
                    ifelse(data$DT == 13, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +13, na.pad=TRUE)))-1,
                    ifelse(data$DT == 12, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +12, na.pad=TRUE)))-1,
                    ifelse(data$DT == 11, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +11, na.pad=TRUE)))-1,
                    ifelse(data$DT == 10, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +10, na.pad=TRUE)))-1,
                    ifelse(data$DT == 9, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +9, na.pad=TRUE)))-1,
                    ifelse(data$DT == 8, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +8, na.pad=TRUE)))-1,
                    ifelse(data$DT == 7, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +7, na.pad=TRUE)))-1,
                    ifelse(data$DT == 6, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +6, na.pad=TRUE)))-1,
                    ifelse(data$DT == 5, (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +5, na.pad=TRUE)))-1,0))))))))))))))
  
  # Days in a momentum: DM
  
  data$MomVar = ifelse(data$Strategy == "Buy", 1, ifelse(data$Strategy == "Sell", -1, 0))
  
  data$DM = as.integer(ifelse(data$Strategy == "Buy", ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:8, na.pad=TRUE)), na.rm = TRUE) == 8, 13, 
                                                      ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:7, na.pad=TRUE)), na.rm = TRUE) == 7, 12, 
                                                      ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:6, na.pad=TRUE)), na.rm = TRUE) == 6, 11, 
                                                      ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:5, na.pad=TRUE)), na.rm = TRUE) == 5, 10, 
                                                      ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:4, na.pad=TRUE)), na.rm = TRUE) == 4, 9,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:3, na.pad=TRUE)), na.rm = TRUE) == 3, 8,
                                                      ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:2, na.pad=TRUE)), na.rm = TRUE) == 2, 7,
                                                      ifelse(coredata(lag(zoo(data$MomVar), +1 , na.pad=TRUE)) == 1, 6, 5)))))))),
                              ifelse(data$Strategy == "Sell", ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:8, na.pad=TRUE)), na.rm = TRUE) == -8, 13,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:7, na.pad=TRUE)), na.rm = TRUE) == -7, 12,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:6, na.pad=TRUE)), na.rm = TRUE) == -6, 11,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:5, na.pad=TRUE)), na.rm = TRUE) == -5, 10, 
                                                              ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:4, na.pad=TRUE)), na.rm = TRUE) == -4, 9,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:3, na.pad=TRUE)), na.rm = TRUE) == -3, 8,
                                                              ifelse(rowSums(coredata(lag(zoo(data$MomVar), k=1:2, na.pad=TRUE)), na.rm = TRUE) == -2, 7,
                                                              ifelse(coredata(lag(zoo(data$MomVar), +1 , na.pad=TRUE)) == -1, 6, 5)))))))),0)))
  
  # % change of the momentum pattern
  
  data$MomentumCh = ifelse(data$DM == 13,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +13, na.pad=TRUE)))-1,
                           ifelse(data$DM == 12,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +12, na.pad=TRUE)))-1,
                           ifelse(data$DM == 11,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +11, na.pad=TRUE)))-1,
                           ifelse(data$DM == 10,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +10, na.pad=TRUE)))-1,
                           ifelse(data$DM == 9,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +9, na.pad=TRUE)))-1,
                           ifelse(data$DM == 8,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +8, na.pad=TRUE)))-1,
                           ifelse(data$DM == 7,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +7, na.pad=TRUE)))-1,
                           ifelse(data$DM == 6,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +6, na.pad=TRUE)))-1,
                           ifelse(data$DM == 5,(data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +5, na.pad=TRUE)))-1,0)))))))))
  
  # The volume of today is greater than yesterday's volume: 
  
  data$VolUp_5D = as.integer(data$PX_VOLUME > coredata(lag(zoo(data$PX_VOLUME), +1, na.pad=TRUE)))
  
  # Days with volume up: 
  
  data$DVolUp = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$VolUp_5D), k=0:3, na.pad=TRUE))), 0))
  
  # 2° momentum day with volume up:
  
  data$VolUp_2D <- coredata(lag(zoo(data$VolUp_5D), k=3, na.pad=TRUE))
  
  # 3° momentum day with volume up:
  
  data$VolUp_3D <- coredata(lag(zoo(data$VolUp_5D), k=2, na.pad=TRUE))
  
  # 4° momentum day with volume up:
  
  data$VolUp_4D <- coredata(lag(zoo(data$VolUp_5D), k=1, na.pad=TRUE))
  
  # Max. price in 10 days:
  
  data$MaxPrice10D = apply(coredata(lag(zoo(data$PX_HIGH), k=-1:-10, na.pad=TRUE)),1,max)
  
  # Min. price in 10 days:
  
  data$MinPrice10D = apply(coredata(lag(zoo(data$PX_LOW), k=-1:-10, na.pad=TRUE)),1,min)
  
  # Date of Max. price in 10 days:
  # Counting forward from the investment signal

  data$Date_MaxPrice10D = as.integer(apply(coredata(lag(zoo(data$PX_HIGH), k=-1:-10, na.pad=TRUE)),1,which.max))

  # Date of Min. price in 10 days:
  # Counting forward from the investment signal
  
  data$Date_MinPrice10D = as.integer(apply(coredata(lag(zoo(data$PX_LOW), k=-1:-10, na.pad=TRUE)),1,which.min))
  
  # Max. price in 15 days:
  
  data$MaxPrice15D = apply(coredata(lag(zoo(data$PX_HIGH), k=-1:-15, na.pad=TRUE)),1,max)
  
  # Min. price in 15 days:
  
  data$MinPrice15D = apply(coredata(lag(zoo(data$PX_LOW), k=-1:-15, na.pad=TRUE)),1,min)
  
  # Date of Max. price in 15 days:
  # Counting forward from the investment signal
  
  data$Date_MaxPrice15D = as.integer(apply(coredata(lag(zoo(data$PX_HIGH), k=-1:-15, na.pad=TRUE)),1,which.max))
  
  # Date of Min. price in 15 days:
  # Counting forward from the investment signal
  
  data$Date_MinPrice15D = as.integer(apply(coredata(lag(zoo(data$PX_LOW), k=-1:-15, na.pad=TRUE)),1,which.min))
  
  # Average Bid-Ask Spread during the momentum pattern
  
  data$Mom_Avg_BID_ASK = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:12, na.pad=TRUE))),
                          ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:11, na.pad=TRUE))),
                          ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:10, na.pad=TRUE))),
                          ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:9, na.pad=TRUE))),
                          ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:8, na.pad=TRUE))),
                          ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:7, na.pad=TRUE))),
                          ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:6, na.pad=TRUE))),
                          ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:5, na.pad=TRUE))),
                          ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$BID_ASK), k=0:4, na.pad=TRUE))),0)))))))))

  # Dividend yield 12 months (260 working days) % change:
  
  data$DIV_YLD_12m_chng <- round(data$DIVIDEND_12_MONTH_YIELD / coredata(lag(zoo(data$DIVIDEND_12_MONTH_YIELD), +260, na.pad=TRUE)) - 1, digits = 2)
  
  # Past 12 months (260 working days) Average Dividend yield:
  
  data$Avg_12m_DIV_YLD = rowMeans(coredata(lag(zoo(data$DIVIDEND_12_MONTH_YIELD), k=0:259, na.pad=TRUE)))
  
  # 12 months Average Dividend yield: Filling NAs with the average Dividend yield
  
  data$Avg_12m_DIV_YLD[is.na(data$Avg_12m_DIV_YLD) == TRUE] <- mean(data$Avg_12m_DIV_YLD, na.rm = TRUE)
  
  # Average of the volume against the 30 days average volume during the momentum pattern
  
  data$Mom_Vol_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:12, na.pad=TRUE))),
                                ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:11, na.pad=TRUE))),
                                ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:10, na.pad=TRUE))),
                                ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:9, na.pad=TRUE))),
                                ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:8, na.pad=TRUE))),
                                ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:7, na.pad=TRUE))),
                                ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:6, na.pad=TRUE))),
                                ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:5, na.pad=TRUE))),
                                ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Max. ratio of the volume against the 30 days average volume during the momentum pattern

  data$Max_Mom_Vol_Relatv_30D = ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:12, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:11, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:10, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:9, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:8, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:7, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:6, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:5, na.pad=TRUE)),1,max),
                                   ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:4, na.pad=TRUE)),1,max),0)))))))))
  
  # Day of the Max. ratio between volume and the 30 days average volume in the momentum pattern
  # Counting backward from the most recent date

  data$Day_Max_Mom_Vol_Relatv_30D = as.integer(ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:12, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:11, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:10, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:9, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:8, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:7, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:6, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:5, na.pad=TRUE)),1,which.max),
                                       ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Vol_Relatv_30D), k=0:4, na.pad=TRUE)),1,which.max),0))))))))))

  # % change volume:
  
  data$Vol_Chng <- (data$PX_VOLUME/coredata(lag(zoo(data$PX_VOLUME), +1, na.pad=TRUE)))-1
  
  # Mean % change volume in the momentum pattern:
  
  data$Mom_Avg_Vol_Chng = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:11, na.pad=TRUE))),
                                ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:10, na.pad=TRUE))),
                                ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:9, na.pad=TRUE))),
                                ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:8, na.pad=TRUE))),
                                ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:7, na.pad=TRUE))),
                                ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:6, na.pad=TRUE))),
                                ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:5, na.pad=TRUE))),
                                ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:4, na.pad=TRUE))),
                                ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$Vol_Chng), k=0:3, na.pad=TRUE))),0)))))))))
  
  # 5-days moving average (Short term):
  
  data$Avg_Price_5D <- rowMeans(coredata(lag(zoo(data$PX_LAST), k=0:4, na.pad=TRUE)))
  
  # 30-days moving average (Long term):
  
  data$Avg_Price_30D <- rowMeans(coredata(lag(zoo(data$PX_LAST), k=0:29, na.pad=TRUE)))
  
  # 50-days moving average (Long term):
  
  data$Avg_Price_50D <- rowMeans(coredata(lag(zoo(data$PX_LAST), k=0:49, na.pad=TRUE)))

  # 200-days moving average (Long term):
  
  data$Avg_Price_200D <- rowMeans(coredata(lag(zoo(data$PX_LAST), k=0:199, na.pad=TRUE)))
  
  # Percentage change between the closing price and the lowest or maximum price 
  
  data$Chng_Max_Price_Mom = ifelse(data$Strategy == "Buy", ifelse(data$DM == 13,(apply(coredata(lag(zoo(data$PX_LOW), k=0:12, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 12,(apply(coredata(lag(zoo(data$PX_LOW), k=0:11, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 11,(apply(coredata(lag(zoo(data$PX_LOW), k=0:10, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 10,(apply(coredata(lag(zoo(data$PX_LOW), k=0:9, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 9,(apply(coredata(lag(zoo(data$PX_LOW), k=0:8, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 8,(apply(coredata(lag(zoo(data$PX_LOW), k=0:7, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 7,(apply(coredata(lag(zoo(data$PX_LOW), k=0:6, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 6,(apply(coredata(lag(zoo(data$PX_LOW), k=0:5, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,
                                                           ifelse(data$DM == 5,(apply(coredata(lag(zoo(data$PX_LOW), k=0:4, na.pad=TRUE)),1,min)/data$PX_LAST) - 1,0))))))))),
                                   ifelse(data$Strategy == "Sell", ifelse(data$DM == 13,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:12, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 12,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:11, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 11,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:10, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 10,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:9, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 9,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:8, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 8,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:7, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 7,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:6, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 6,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:5, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,
                                                                   ifelse(data$DM == 5,(apply(coredata(lag(zoo(data$PX_HIGH), k=0:4, na.pad=TRUE)),1,max)/data$PX_LAST) - 1,0))))))))),0))
  
  # Day of the lowest or maximum price in the momentum pattern
  # Counting backward from the most recent date

  data$Day_Max_Price_Mom = as.integer(ifelse(data$Strategy == "Buy", ifelse(data$DM == 13,apply(coredata(lag(zoo(data$PX_LOW), k=0:12, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 12,apply(coredata(lag(zoo(data$PX_LOW), k=0:11, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 11,apply(coredata(lag(zoo(data$PX_LOW), k=0:10, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 10,apply(coredata(lag(zoo(data$PX_LOW), k=0:9, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 9,apply(coredata(lag(zoo(data$PX_LOW), k=0:8, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 8,apply(coredata(lag(zoo(data$PX_LOW), k=0:7, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 7,apply(coredata(lag(zoo(data$PX_LOW), k=0:6, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 6,apply(coredata(lag(zoo(data$PX_LOW), k=0:5, na.pad=TRUE)),1,which.min),
                                                           ifelse(data$DM == 5,apply(coredata(lag(zoo(data$PX_LOW), k=0:4, na.pad=TRUE)),1,which.min),0))))))))),
                                   ifelse(data$Strategy == "Sell", ifelse(data$DM == 13,apply(coredata(lag(zoo(data$PX_HIGH), k=0:12, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 12,apply(coredata(lag(zoo(data$PX_HIGH), k=0:11, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 11,apply(coredata(lag(zoo(data$PX_HIGH), k=0:10, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 10,apply(coredata(lag(zoo(data$PX_HIGH), k=0:9, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 9,apply(coredata(lag(zoo(data$PX_HIGH), k=0:8, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 8,apply(coredata(lag(zoo(data$PX_HIGH), k=0:7, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 7,apply(coredata(lag(zoo(data$PX_HIGH), k=0:6, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 6,apply(coredata(lag(zoo(data$PX_HIGH), k=0:5, na.pad=TRUE)),1,which.max),
                                                                   ifelse(data$DM == 5,apply(coredata(lag(zoo(data$PX_HIGH), k=0:4, na.pad=TRUE)),1,which.max),0))))))))),0)))
  
  # Price returns:
  
  data$Pr_Rt <- (data$PX_LAST/coredata(lag(zoo(data$PX_LAST), +1, na.pad=TRUE)))-1
  
  # 12 months Risk (260 working days):
  
  data$Risk_12m <- rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:259, na.pad=TRUE)))
  
  # 5 year Risk (1300 working days):
  
  data$Risk_5y <- rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:1299, na.pad=TRUE)))
  
  # 12 months Average return (260 working days):
  
  data$Avg_12m_Pr_Rt = rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:259, na.pad=TRUE)))
  
  # 5 year Average return (1300 working days):
  
  data$Avg_5y_Pr_Rt = rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:1299, na.pad=TRUE)))
  
  # Momentum Average return:
  
  data$Mom_Avg_Rt = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE))),
                                   ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE))),
                                   ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE))),
                                   ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE))),
                                   ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE))),
                                   ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE))),
                                   ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE))),
                                   ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE))),
                                   ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Momentum risk:
  
  data$Mom_Risk = ifelse(data$DM == 13,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE))),
                                   ifelse(data$DM == 12,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE))),
                                   ifelse(data$DM == 11,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE))),
                                   ifelse(data$DM == 10,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE))),
                                   ifelse(data$DM == 9,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE))),
                                   ifelse(data$DM == 8,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE))),
                                   ifelse(data$DM == 7,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE))),
                                   ifelse(data$DM == 6,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE))),
                                   ifelse(data$DM == 5,rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE))),0)))))))))
  
  # 30 days volatility:
  
  data$Volat_30d <- rowSds(coredata(lag(zoo(data$Pr_Rt), k=0:29, na.pad=TRUE)))
  
  # Trend Average return:
  
  data$T_Avg_Rt = ifelse(data$DT == 18,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:17, na.pad=TRUE))),
                           ifelse(data$DT == 17,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:16, na.pad=TRUE))),
                           ifelse(data$DT == 16,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:15, na.pad=TRUE))),
                           ifelse(data$DT == 15,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:14, na.pad=TRUE))),
                           ifelse(data$DT == 14,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:13, na.pad=TRUE))),
                           ifelse(data$DT == 13,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE))),
                           ifelse(data$DT == 12,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE))),
                           ifelse(data$DT == 11,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE))),
                           ifelse(data$DT == 10,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE))),
                           ifelse(data$DT == 9,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE))),
                           ifelse(data$DT == 8,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE))),
                           ifelse(data$DT == 7,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE))),
                           ifelse(data$DT == 6,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE))),
                           ifelse(data$DT == 5,rowMeans(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE))),0))))))))))))))
  
  # Momentum Median return:
  
  data$Mom_Median_Rt = ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE)),1,median),
                              ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE)),1,median),0)))))))))
  
  # Trend Median return:
  
  data$T_Median_Rt = ifelse(data$DT == 18,apply(coredata(lag(zoo(data$Pr_Rt), k=0:17, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 17,apply(coredata(lag(zoo(data$Pr_Rt), k=0:16, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 16,apply(coredata(lag(zoo(data$Pr_Rt), k=0:15, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 15,apply(coredata(lag(zoo(data$Pr_Rt), k=0:14, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 14,apply(coredata(lag(zoo(data$Pr_Rt), k=0:13, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 13,apply(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 12,apply(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 11,apply(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 10,apply(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 9,apply(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 8,apply(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 7,apply(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 6,apply(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE)),1,median),
                              ifelse(data$DT == 5,apply(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE)),1,median),0))))))))))))))
  
  # Volume standard desviation from movile mean - High/Low volume trading
  
  data$VolSD_Relatv_30D <- (data$PX_VOLUME - data$AvgVol30D) / data$SDVol30D
  
  # Average of the volume standard desviation from movile mean during the momentum pattern
  
  data$Mom_VolSD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                   ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                   ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                   ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                   ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                   ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                   ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                   ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                   ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Max volume standard desviation from movile mean during the momentum pattern

  data$Max_Mom_VolSD_Relatv_30D = ifelse(data$DM == 13,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:12, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 12,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:11, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 11,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:10, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 10,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:9, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 9,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:8, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 8,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:7, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 7,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:6, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 6,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:5, na.pad=TRUE)),1,max),
                                       ifelse(data$DM == 5,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:4, na.pad=TRUE)),1,max),0)))))))))
  
  # Day of the Max volume standard desviation from movile mean in the momentum pattern
  # Counting backward from the most recent date

  data$Day_Max_Mom_VolSD_Relatv_30D = as.integer(ifelse(data$DM == 13,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:12, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 12,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:11, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 11,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:10, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 10,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:9, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 9,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:8, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 8,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:7, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 7,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:6, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 6,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:5, na.pad=TRUE)),1,which.max),
                                           ifelse(data$DM == 5,apply(coredata(lag(zoo(data$VolSD_Relatv_30D), k=0:4, na.pad=TRUE)),1,which.max),0))))))))))
  
  # Today's return is greater than or equal to yesterday's return: 
  
  data$Rt_Up_5D = as.integer(abs(data$Pr_Rt) >= abs(coredata(lag(zoo(data$Pr_Rt), +1, na.pad=TRUE))))
  
  # Days with greater return: 
  
  data$D_Rt_Up = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$Rt_Up_5D), k=0:3, na.pad=TRUE))), 0))
  
  # 2° momentum day with greater return:
  
  data$Rt_Up_2D <- coredata(lag(zoo(data$Rt_Up_5D), k=3, na.pad=TRUE))
  
  # 3° momentum day with greater return:
  
  data$Rt_Up_3D <- coredata(lag(zoo(data$Rt_Up_5D), k=2, na.pad=TRUE))
  
  # 4° momentum day with greater return:
  
  data$Rt_Up_4D <- coredata(lag(zoo(data$Rt_Up_5D), k=1, na.pad=TRUE))
  
  # Max % price change during the momentum pattern

  data$Max_Rt_Mom = ifelse(data$Strategy == "Buy", ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE)),1,min),
                                                          ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE)),1,min),0))))))))),
                                  ifelse(data$Strategy == "Sell", ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE)),1,max),
                                                          ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE)),1,max),0))))))))),0))
  
  
  # Day of the max % price change in the momentum pattern
  # Counting backward from the most recent date

  data$Day_Max_Rt_Mom = as.integer(ifelse(data$Strategy == "Buy", ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE)),1,which.min),
                                                       ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE)),1,which.min),0))))))))),
                                  ifelse(data$Strategy == "Sell", ifelse(data$DM == 13,apply(coredata(lag(zoo(data$Pr_Rt), k=0:12, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 12,apply(coredata(lag(zoo(data$Pr_Rt), k=0:11, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 11,apply(coredata(lag(zoo(data$Pr_Rt), k=0:10, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 10,apply(coredata(lag(zoo(data$Pr_Rt), k=0:9, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 9,apply(coredata(lag(zoo(data$Pr_Rt), k=0:8, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 8,apply(coredata(lag(zoo(data$Pr_Rt), k=0:7, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 7,apply(coredata(lag(zoo(data$Pr_Rt), k=0:6, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 6,apply(coredata(lag(zoo(data$Pr_Rt), k=0:5, na.pad=TRUE)),1,which.max),
                                                       ifelse(data$DM == 5,apply(coredata(lag(zoo(data$Pr_Rt), k=0:4, na.pad=TRUE)),1,which.max),0))))))))),0)))
  
  # % change between the close price and the mean price (27 weeks)
  
  data$PR_Chng_27wkAvg <- (data$PX_LAST/rowMeans(coredata(lag(zoo(data$PX_LAST), k=0:134, na.pad=TRUE))))-1
  
  # Changes in trading volume

  data$Chng_Trade_Volum = ifelse(data$DM == 13,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:12, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=13:42, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 12,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:11, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=12:41, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 11,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:10, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=11:40, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 10,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:9, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=10:39, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 9,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:8, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=9:38, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 8,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:7, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=8:37, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 7,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:6, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=7:36, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 6,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:5, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=6:35, na.pad=TRUE))))-1,
                                   ifelse(data$DM == 5,(rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=0:4, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # Total Open interest
  
  data$TOTAL_OPEN_INT <- data$OPEN_INT_TOTAL_CALL + data$OPEN_INT_TOTAL_PUT

  # Percentage change of the Total open interest:
  
  data$Chng_TOTAL_OPEN_INT <- ifelse(data$DM == 13,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +13, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 12,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +12, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 11,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +11, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 10,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +10, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 9,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +9, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 8,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +8, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 7,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +7, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 6,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +6, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 5,(data$TOTAL_OPEN_INT/coredata(lag(zoo(data$TOTAL_OPEN_INT), +5, na.pad=TRUE)))-1,0)))))))))
  
  # Average Total open interest over the last month
  
  data$Avg_TOT_OPEN_INT_30D <- rowMeans(coredata(lag(zoo(data$TOTAL_OPEN_INT), k=1:30, na.pad=TRUE)))
  
  # Total Open interest standard desviation from movile mean
  
  data$TOT_OPEN_INT_SD_Relatv_30D <- (data$TOTAL_OPEN_INT - data$Avg_TOT_OPEN_INT_30D) / rowSds(coredata(lag(zoo(data$TOTAL_OPEN_INT), k=1:30, na.pad=TRUE)))
  
  # Average of the Total Open interest standard desviation from movile mean during the momentum pattern
  
  data$Mom_TOT_OPEN_INT_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                     ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                     ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                     ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                     ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                     ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                     ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                     ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                     ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0))))))))) 
  
  # Percentage change of the Call open interest:
  
  data$Chng_CALL_OPEN_INT <- ifelse(data$DM == 13,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +13, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 12,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +12, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 11,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +11, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 10,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +10, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 9,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +9, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 8,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +8, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 7,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +7, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 6,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +6, na.pad=TRUE)))-1,
                                     ifelse(data$DM == 5,(data$OPEN_INT_TOTAL_CALL/coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), +5, na.pad=TRUE)))-1,0)))))))))
  
  # Average call open interest over the last month
  
  data$Avg_CALL_OPEN_INT_30D <- rowMeans(coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), k=1:30, na.pad=TRUE)))
  
  # Call Open interest standard desviation from movile mean
  
  data$CALL_OPEN_INT_SD_Relatv_30D <- (data$OPEN_INT_TOTAL_CALL - data$Avg_CALL_OPEN_INT_30D) / rowSds(coredata(lag(zoo(data$OPEN_INT_TOTAL_CALL), k=1:30, na.pad=TRUE)))
  
  # Average of the call Open interest standard desviation from movile mean during the momentum pattern
  
  data$Mom_CALL_OPEN_INT_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                               ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                               ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                               ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                               ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                               ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                               ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                               ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                               ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0))))))))) 
  
  # Percentage change of the put open interest:
  
  data$Chng_PUT_OPEN_INT <- ifelse(data$DM == 13,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +13, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 12,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +12, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 11,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +11, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 10,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +10, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 9,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +9, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 8,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +8, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 7,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +7, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 6,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +6, na.pad=TRUE)))-1,
                                    ifelse(data$DM == 5,(data$OPEN_INT_TOTAL_PUT/coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), +5, na.pad=TRUE)))-1,0)))))))))
  
  # Average put open interest over the last month
  
  data$Avg_PUT_OPEN_INT_30D <- rowMeans(coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), k=1:30, na.pad=TRUE)))
  
  # Put Open interest standard desviation from movile mean
  
  data$PUT_OPEN_INT_SD_Relatv_30D <- (data$OPEN_INT_TOTAL_PUT - data$Avg_PUT_OPEN_INT_30D) / rowSds(coredata(lag(zoo(data$OPEN_INT_TOTAL_PUT), k=1:30, na.pad=TRUE)))
  
  # Average of the put Open interest standard desviation from movile mean during the momentum pattern
  
  data$Mom_PUT_OPEN_INT_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                                ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                                ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                                ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                                ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                                ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                                ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                                ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                                ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0))))))))) 
  
  # Put / Call Open Int. Ratio
  
  data$PUT_CALL_OPEN_INTEREST_RATIO <- data$OPEN_INT_TOTAL_PUT / data$OPEN_INT_TOTAL_CALL
  
  # Percentage change of the the Put/Call Ratio during the momentum pattern:
  
  data$Chng_PUT_CALL_OPEN_INT_RATIO <- ifelse(data$DM == 13,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +13, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 12,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +12, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 11,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +11, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 10,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +10, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 9,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +9, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 8,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +8, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 7,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +7, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 6,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +6, na.pad=TRUE)))-1,
                                       ifelse(data$DM == 5,(data$PUT_CALL_OPEN_INTEREST_RATIO/coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), +5, na.pad=TRUE)))-1,0)))))))))
  
  # Average Put/Call Ratio over the last month
  
  data$Avg_PUT_CALL_OPEN_INT_RATIO_30D <- rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), k=1:30, na.pad=TRUE)))
  
  # Put/Call Ratio standard desviation from movile mean
  
  data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D <- (data$PUT_CALL_OPEN_INTEREST_RATIO - data$Avg_PUT_CALL_OPEN_INT_RATIO_30D) / rowSds(coredata(lag(zoo(data$PUT_CALL_OPEN_INTEREST_RATIO), k=1:30, na.pad=TRUE)))
  
  # Average of the Put/Call Ratio standard desviation from movile mean during the momentum pattern
  
  data$Mom_PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                                   ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                                   ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                                   ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                                   ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                                   ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                                   ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                                   ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                                   ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0))))))))) 
  
  # Filling Option Volume NAs with zero
  
  data$VOLUME_TOTAL_CALL[is.na(data$VOLUME_TOTAL_CALL) == TRUE] <- 0
  data$VOLUME_TOTAL_PUT[is.na(data$VOLUME_TOTAL_PUT) == TRUE] <- 0
  
  # Total Option Volume
  
  data$TOT_OPT_VOLUME <- data$VOLUME_TOTAL_CALL + data$VOLUME_TOTAL_PUT

  # Month average - Total Option volume
  
  data$Avg_TOT_OPT_VOLUME_30D = rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=1:30, na.pad=TRUE)))
  
  # Month standard deviation - Total Option volume
  
  data$SD_TOT_OPT_VOLUME_30D = rowSds(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=1:30, na.pad=TRUE))) 
  
  # Total Option Volume against the 30 days average volume
  
  data$TOT_OPT_VOL_Relatv_30D <- data$TOT_OPT_VOLUME / data$Avg_TOT_OPT_VOLUME_30D
  
  # Average of the Total Option Volume against the 30 days average volume during the momentum pattern
  
  data$Mom_TOT_OPT_VOL_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:12, na.pad=TRUE))),
                                   ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:11, na.pad=TRUE))),
                                   ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:10, na.pad=TRUE))),
                                   ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:9, na.pad=TRUE))),
                                   ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:8, na.pad=TRUE))),
                                   ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:7, na.pad=TRUE))),
                                   ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:6, na.pad=TRUE))),
                                   ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:5, na.pad=TRUE))),
                                   ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # % change Total Option Volume:
  
  data$TOT_OPT_VOL_Chng <- (data$TOT_OPT_VOLUME/coredata(lag(zoo(data$TOT_OPT_VOLUME), +1, na.pad=TRUE)))-1
  
  # Mean % change Total Option Volume in the momentum pattern:
  
  data$Mom_Avg_TOT_OPT_VOL_Chng = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:11, na.pad=TRUE))),
                                 ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:10, na.pad=TRUE))),
                                 ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:9, na.pad=TRUE))),
                                 ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:8, na.pad=TRUE))),
                                 ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:7, na.pad=TRUE))),
                                 ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:6, na.pad=TRUE))),
                                 ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:5, na.pad=TRUE))),
                                 ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:4, na.pad=TRUE))),
                                 ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_Chng), k=0:3, na.pad=TRUE))),0)))))))))
  
  # Total Option Volume standard desviation from movile mean
  
  data$TOT_OPT_VOL_SD_Relatv_30D <- (data$TOT_OPT_VOLUME - data$Avg_TOT_OPT_VOLUME_30D) / data$SD_TOT_OPT_VOLUME_30D
  
  # Average of the Total Option Volume standard desviation from movile mean during the momentum pattern
  
  data$Mom_TOT_OPT_VOL_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                     ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                     ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                     ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                     ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                     ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                     ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                     ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                     ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$TOT_OPT_VOL_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Changes in Total Option Volume

  data$Chng_TOT_OPT_VOL = ifelse(data$DM == 13,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:12, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=13:42, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 12,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:11, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=12:41, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 11,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:10, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=11:40, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 10,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:9, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=10:39, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 9,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:8, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=9:38, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 8,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:7, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=8:37, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 7,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:6, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=7:36, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 6,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:5, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=6:35, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 5,(rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=0:4, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$TOT_OPT_VOLUME), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # The Total Option Volume of today is greater than yesterday's Total Option Volume: 
  
  data$TOT_OPT_VOL_Up_5D = as.integer(data$TOT_OPT_VOLUME > coredata(lag(zoo(data$TOT_OPT_VOLUME), +1, na.pad=TRUE)))
  
  # Days with Total Option Volume up: 
  
  data$D_TOT_OPT_VOL_Up = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$TOT_OPT_VOL_Up_5D), k=0:4, na.pad=TRUE))), 0))
  
  # Month average - Call Option volume
  
  data$Avg_VOLUME_TOTAL_CALL_30D = rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=1:30, na.pad=TRUE)))
  
  # Month standard deviation - Call Option volume
  
  data$SD_VOLUME_TOTAL_CALL_30D = rowSds(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=1:30, na.pad=TRUE))) 
  
  # Call Option volume against the 30 days average volume
  
  data$CALL_VOL_Relatv_30D <- data$VOLUME_TOTAL_CALL / data$Avg_VOLUME_TOTAL_CALL_30D
  
  # Average of the Call Option volume against the 30 days average volume during the momentum pattern
  
  data$Mom_CALL_VOL_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:12, na.pad=TRUE))),
                                           ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:11, na.pad=TRUE))),
                                           ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:10, na.pad=TRUE))),
                                           ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:9, na.pad=TRUE))),
                                           ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:8, na.pad=TRUE))),
                                           ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:7, na.pad=TRUE))),
                                           ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:6, na.pad=TRUE))),
                                           ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:5, na.pad=TRUE))),
                                           ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$CALL_VOL_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # % change Call Option volume:
  
  data$CALL_VOL_Chng <- (data$VOLUME_TOTAL_CALL/coredata(lag(zoo(data$VOLUME_TOTAL_CALL), +1, na.pad=TRUE)))-1
  
  # Mean % change Call Option volume in the momentum pattern:
  
  data$Mom_Avg_CALL_VOL_Chng = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:11, na.pad=TRUE))),
                                         ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:10, na.pad=TRUE))),
                                         ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:9, na.pad=TRUE))),
                                         ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:8, na.pad=TRUE))),
                                         ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:7, na.pad=TRUE))),
                                         ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:6, na.pad=TRUE))),
                                         ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:5, na.pad=TRUE))),
                                         ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:4, na.pad=TRUE))),
                                         ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$CALL_VOL_Chng), k=0:3, na.pad=TRUE))),0)))))))))
  
  # Call Option volume standard desviation from movile mean
  
  data$CALL_VOL_SD_Relatv_30D <- (data$VOLUME_TOTAL_CALL - data$Avg_VOLUME_TOTAL_CALL_30D) / data$SD_VOLUME_TOTAL_CALL_30D
  
  # Average of the Call Option volume standard desviation from movile mean during the momentum pattern
  
  data$Mom_CALL_VOL_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                              ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                              ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                              ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                              ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                              ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                              ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                              ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                              ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$CALL_VOL_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Changes in Call Option volume

  data$Chng_CALL_VOL = ifelse(data$DM == 13,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:12, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=13:42, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 12,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:11, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=12:41, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 11,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:10, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=11:40, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 10,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:9, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=10:39, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 9,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:8, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=9:38, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 8,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:7, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=8:37, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 7,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:6, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=7:36, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 6,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:5, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=6:35, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 5,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=0:4, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_CALL), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # The Call Option volume of today is greater than yesterday's Call Option volume: 
  
  data$CALL_VOL_Up_5D = as.integer(data$VOLUME_TOTAL_CALL > coredata(lag(zoo(data$VOLUME_TOTAL_CALL), +1, na.pad=TRUE)))
  
  # Days with Call Option volume up: 
  
  data$D_CALL_VOL_Up = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$CALL_VOL_Up_5D), k=0:4, na.pad=TRUE))), 0))
  
  # Month average - Put Option volume
  
  data$Avg_VOLUME_TOTAL_PUT_30D = rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=1:30, na.pad=TRUE)))
  
  # Month standard deviation - Put Option volume
  
  data$SD_VOLUME_TOTAL_PUT_30D = rowSds(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=1:30, na.pad=TRUE))) 
  
  # Put Option volume against the 30 days average volume
  
  data$PUT_VOL_Relatv_30D <- data$VOLUME_TOTAL_PUT / data$Avg_VOLUME_TOTAL_PUT_30D
  
  # Average of the Put Option volume against the 30 days average volume during the momentum pattern
  
  data$Mom_PUT_VOL_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:12, na.pad=TRUE))),
                                        ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:11, na.pad=TRUE))),
                                        ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:10, na.pad=TRUE))),
                                        ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:9, na.pad=TRUE))),
                                        ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:8, na.pad=TRUE))),
                                        ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:7, na.pad=TRUE))),
                                        ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:6, na.pad=TRUE))),
                                        ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:5, na.pad=TRUE))),
                                        ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_VOL_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # % change Put Option volume:
  
  data$PUT_VOL_Chng <- (data$VOLUME_TOTAL_PUT/coredata(lag(zoo(data$VOLUME_TOTAL_PUT), +1, na.pad=TRUE)))-1
  
  # Mean % change Put Option volume in the momentum pattern:
  
  data$Mom_Avg_PUT_VOL_Chng = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:11, na.pad=TRUE))),
                                      ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:10, na.pad=TRUE))),
                                      ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:9, na.pad=TRUE))),
                                      ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:8, na.pad=TRUE))),
                                      ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:7, na.pad=TRUE))),
                                      ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:6, na.pad=TRUE))),
                                      ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:5, na.pad=TRUE))),
                                      ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:4, na.pad=TRUE))),
                                      ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_VOL_Chng), k=0:3, na.pad=TRUE))),0)))))))))
  
  # Put Option volume standard desviation from movile mean
  
  data$PUT_VOL_SD_Relatv_30D <- (data$VOLUME_TOTAL_PUT - data$Avg_VOLUME_TOTAL_PUT_30D) / data$SD_VOLUME_TOTAL_PUT_30D
  
  # Average of the Put Option volume standard desviation from movile mean during the momentum pattern
  
  data$Mom_PUT_VOL_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                           ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                           ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                           ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                           ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                           ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                           ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                           ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                           ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_VOL_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Changes in Put Option volume

  data$Chng_PUT_VOL = ifelse(data$DM == 13,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:12, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=13:42, na.pad=TRUE))))-1,
                              ifelse(data$DM == 12,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:11, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=12:41, na.pad=TRUE))))-1,
                              ifelse(data$DM == 11,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:10, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=11:40, na.pad=TRUE))))-1,
                              ifelse(data$DM == 10,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:9, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=10:39, na.pad=TRUE))))-1,
                              ifelse(data$DM == 9,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:8, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=9:38, na.pad=TRUE))))-1,
                              ifelse(data$DM == 8,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:7, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=8:37, na.pad=TRUE))))-1,
                              ifelse(data$DM == 7,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:6, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=7:36, na.pad=TRUE))))-1,
                              ifelse(data$DM == 6,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:5, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=6:35, na.pad=TRUE))))-1,
                              ifelse(data$DM == 5,(rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=0:4, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$VOLUME_TOTAL_PUT), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # The Put Option volume of today is greater than yesterday's Put Option volume: 
  
  data$PUT_VOL_Up_5D = as.integer(data$VOLUME_TOTAL_PUT > coredata(lag(zoo(data$VOLUME_TOTAL_PUT), +1, na.pad=TRUE)))
  
  # Days with Put Option volume up: 
  
  data$D_PUT_VOL_Up = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$PUT_VOL_Up_5D), k=0:4, na.pad=TRUE))), 0))
  
  # Total open interest relative to the Total Option Volume of contracts traded
  
  data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL <- data$TOTAL_OPEN_INT / data$TOT_OPT_VOLUME
  
  # Average of the Total open interest relative to the Total Option Volume during the momentum pattern
  
  data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:12, na.pad=TRUE))),
                                          ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:11, na.pad=TRUE))),
                                          ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:10, na.pad=TRUE))),
                                          ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:9, na.pad=TRUE))),
                                          ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:8, na.pad=TRUE))),
                                          ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:7, na.pad=TRUE))),
                                          ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:6, na.pad=TRUE))),
                                          ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:5, na.pad=TRUE))),
                                          ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Changes in the Total open interest relative to the Total Option Volume

  data$Chng_T_OPEN_INT_Relatv_T_OPT_VOL = ifelse(data$DM == 13,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=13:42, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 12,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=12:41, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 11,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=11:40, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 10,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=10:39, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 9,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=9:38, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 8,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=8:37, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 7,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=7:36, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 6,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=6:35, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 5,(data$Mom_T_OPEN_INT_Relatv_T_OPT_VOL/rowMeans(coredata(lag(zoo(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # The total Option volume exceeds the existing total open interest 
  
  data$HIGH_TOT_OPT_VOL = as.integer(data$TOT_OPEN_INT_Relatv_TOT_OPT_VOL <= 1)
  
  # Days with high total Option volume: 
  
  data$D_HIGH_TOT_OPT_VOL  = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$HIGH_TOT_OPT_VOL), k=0:4, na.pad=TRUE))), 0))
  
  # Call open interest relative to the Call Option Volume of contracts traded
  
  data$CALL_OPEN_INT_Relatv_CALL_VOL <- data$OPEN_INT_TOTAL_CALL / data$VOLUME_TOTAL_CALL
  
  # Average of the Call open interest relative to the Call Option Volume during the momentum pattern
  
  data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:12, na.pad=TRUE))),
                                                ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:11, na.pad=TRUE))),
                                                ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:10, na.pad=TRUE))),
                                                ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:9, na.pad=TRUE))),
                                                ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:8, na.pad=TRUE))),
                                                ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:7, na.pad=TRUE))),
                                                ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:6, na.pad=TRUE))),
                                                ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:5, na.pad=TRUE))),
                                                ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Changes in the Call open interest relative to the Call Option Volume

  data$Chng_CALL_OPEN_INT_Relatv_CALL_VOL = ifelse(data$DM == 13,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=13:42, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 12,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=12:41, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 11,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=11:40, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 10,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=10:39, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 9,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=9:38, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 8,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=8:37, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 7,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=7:36, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 6,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=6:35, na.pad=TRUE))))-1,
                                          ifelse(data$DM == 5,(data$Mom_CALL_OPEN_INT_Relatv_CALL_VOL/rowMeans(coredata(lag(zoo(data$CALL_OPEN_INT_Relatv_CALL_VOL), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # The Call Option volume exceeds the existing Call open interest 
  
  data$HIGH_CALL_VOL = as.integer(data$CALL_OPEN_INT_Relatv_CALL_VOL <= 1)
  
  # Days with high Call Option volume: 
  
  data$D_HIGH_CALL_VOL  = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$HIGH_CALL_VOL), k=0:4, na.pad=TRUE))), 0))

  # Put open interest relative to the Put Option Volume of contracts traded
  
  data$PUT_OPEN_INT_Relatv_PUT_VOL <- data$OPEN_INT_TOTAL_PUT / data$VOLUME_TOTAL_PUT
  
  # Average of the Put open interest relative to the Put Option Volume during the momentum pattern
  
  data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:12, na.pad=TRUE))),
                                                  ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:11, na.pad=TRUE))),
                                                  ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:10, na.pad=TRUE))),
                                                  ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:9, na.pad=TRUE))),
                                                  ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:8, na.pad=TRUE))),
                                                  ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:7, na.pad=TRUE))),
                                                  ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:6, na.pad=TRUE))),
                                                  ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:5, na.pad=TRUE))),
                                                  ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Changes in the Put open interest relative to the Put Option Volume

  data$Chng_PUT_OPEN_INT_Relatv_PUT_VOL = ifelse(data$DM == 13,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=13:42, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 12,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=12:41, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 11,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=11:40, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 10,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=10:39, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 9,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=9:38, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 8,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=8:37, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 7,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=7:36, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 6,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=6:35, na.pad=TRUE))))-1,
                                            ifelse(data$DM == 5,(data$Mom_PUT_OPEN_INT_Relatv_PUT_VOL/rowMeans(coredata(lag(zoo(data$PUT_OPEN_INT_Relatv_PUT_VOL), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  # The put Option volume exceeds the existing put open interest 
  
  data$HIGH_PUT_VOL = as.integer(data$PUT_OPEN_INT_Relatv_PUT_VOL <= 1)
  
  # Days with high put Option volume: 
  
  data$D_HIGH_PUT_VOL  = as.integer(ifelse(data$Strategy == "Buy" | data$Strategy == "Sell", rowSums(coredata(lag(zoo(data$HIGH_PUT_VOL), k=0:4, na.pad=TRUE))), 0))

  # Put / Call Volume Ratio
  
  data$PUT_CALL_VOLUME_RATIO <- data$VOLUME_TOTAL_PUT / data$VOLUME_TOTAL_CALL
  
  # Percentage change of the the Put / Call Volume Ratio during the momentum pattern:
  
  data$PUT_CALL_VOL_RATIO_Chng <- ifelse(data$DM == 13,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +13, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 12,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +12, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 11,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +11, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 10,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +10, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 9,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +9, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 8,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +8, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 7,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +7, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 6,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +6, na.pad=TRUE)))-1,
                                              ifelse(data$DM == 5,(data$PUT_CALL_VOLUME_RATIO/coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), +5, na.pad=TRUE)))-1,0)))))))))
  
  # Average Put / Call Volume Ratio over the last month
  
  data$Avg_PUT_CALL_VOL_RATIO_30D <- rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=1:30, na.pad=TRUE)))
  
  # Put/Call Volume Ratio relative to the average Put/Call Volume Ratio 
  
  data$PUT_CALL_VOL_RATIO_Relatv_30D <- data$PUT_CALL_VOLUME_RATIO / data$Avg_PUT_CALL_VOL_RATIO_30D
  
  # Average of the Put/Call Volume Ratio against the 30 days average volume during the momentum pattern
  
  data$Mom_PUT_CALL_VOL_RATIO_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:12, na.pad=TRUE))),
                                        ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:11, na.pad=TRUE))),
                                        ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:10, na.pad=TRUE))),
                                        ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:9, na.pad=TRUE))),
                                        ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:8, na.pad=TRUE))),
                                        ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:7, na.pad=TRUE))),
                                        ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:6, na.pad=TRUE))),
                                        ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:5, na.pad=TRUE))),
                                        ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_Relatv_30D), k=0:4, na.pad=TRUE))),0)))))))))
  
  # Put/Call volume Ratio standard desviation from movile mean
  
  data$PUT_CALL_VOL_RATIO_SD_Relatv_30D <- (data$PUT_CALL_VOLUME_RATIO - data$Avg_PUT_CALL_VOL_RATIO_30D) / rowSds(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=1:30, na.pad=TRUE)))
  
  # Average of the Put/Call volume Ratio standard desviation from movile mean during the momentum pattern
  
  data$Mom_PUT_CALL_VOL_RATIO_SD_Relatv_30D = ifelse(data$DM == 13,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:12, na.pad=TRUE))),
                                                  ifelse(data$DM == 12,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:11, na.pad=TRUE))),
                                                  ifelse(data$DM == 11,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:10, na.pad=TRUE))),
                                                  ifelse(data$DM == 10,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:9, na.pad=TRUE))),
                                                  ifelse(data$DM == 9,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:8, na.pad=TRUE))),
                                                  ifelse(data$DM == 8,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:7, na.pad=TRUE))),
                                                  ifelse(data$DM == 7,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:6, na.pad=TRUE))),
                                                  ifelse(data$DM == 6,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:5, na.pad=TRUE))),
                                                  ifelse(data$DM == 5,rowMeans(coredata(lag(zoo(data$PUT_CALL_VOL_RATIO_SD_Relatv_30D), k=0:4, na.pad=TRUE))),0))))))))) 
  
  # Changes in Put/Call volume Ratio

  data$Chng_PUT_CALL_VOL_RATIO = ifelse(data$DM == 13,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:12, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=13:42, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 12,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:11, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=12:41, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 11,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:10, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=11:40, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 10,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:9, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=10:39, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 9,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:8, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=9:38, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 8,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:7, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=8:37, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 7,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:6, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=7:36, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 6,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:5, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=6:35, na.pad=TRUE))))-1,
                                 ifelse(data$DM == 5,(rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=0:4, na.pad=TRUE)))/rowMeans(coredata(lag(zoo(data$PUT_CALL_VOLUME_RATIO), k=5:34, na.pad=TRUE))))-1,0)))))))))
  
  data
  
}

# Data transformation

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
RFile1 <- ""

for (i in 1:length(NStocks)){
  RFile1 <- sprintf('%s.RData', NStocks[i])
  load(file = RFile1)
  DF <- xFuncT(DF)
  save(DF, file = RFile1)
  print(i)
  print(NStocks[i])
}

# P/E ratio transformation function

PE_Function <- function(data) {
  
  # To fill NAs data:
  
  data$LATEST_ANNOUNCEMENT_DT2 <- na.locf(data$LATEST_ANNOUNCEMENT_DT2, na.rm = FALSE, fromLast = TRUE)
  data$EPS_TTM <- na.locf(data$EPS_TTM, na.rm = FALSE, fromLast = TRUE)
  data$EPS_Surprise <- na.locf(data$EPS_Surprise, na.rm = FALSE, fromLast = TRUE)
  data$Positv_EPS_Surp <- na.locf(data$Positv_EPS_Surp, na.rm = FALSE, fromLast = TRUE)
  data$EPS_YoY_Chng <- na.locf(data$EPS_YoY_Chng, na.rm = FALSE, fromLast = TRUE)
  data$EPS_PoP_Chng <- na.locf(data$EPS_PoP_Chng, na.rm = FALSE, fromLast = TRUE)
  data$EPS_TTM_DT <- na.locf(data$EPS_TTM_DT, na.rm = FALSE, fromLast = TRUE)
  data$EPS_Surprise_DT <- na.locf(data$EPS_Surprise_DT, na.rm = FALSE, fromLast = TRUE)
  data$Positv_EPS_Surp_DT <- na.locf(data$Positv_EPS_Surp_DT, na.rm = FALSE, fromLast = TRUE)
  data$EPS_YoY_Chng_DT <- na.locf(data$EPS_YoY_Chng_DT, na.rm = FALSE, fromLast = TRUE)
  data$EPS_PoP_Chng_DT <- na.locf(data$EPS_PoP_Chng_DT, na.rm = FALSE, fromLast = TRUE)
  
  # EPS Trailing 12 Months (TTM) equals NA if the last available date is 151 days ago or more (a quarter):
  
  data$EPS_TTM[as.numeric(data$date - data$EPS_TTM_DT) > 150] <- NA

  # P/E ratio:
  
  data$PE_RATIO <- ifelse(is.na(data$PE_RATIO) == TRUE, data$PX_LAST / data$EPS_TTM, data$PE_RATIO)

  # A dummy variable for negative P/E ratio:
  
  data$NEG_PE_RATIO <- as.integer(ifelse(data$PE_RATIO < 0, 1, ifelse(is.na(data$PE_RATIO) == TRUE, NA, 0)))
  
  # Deleting negative and infinite P/E ratios:
  
  data$PE_RATIO[data$PE_RATIO < 0] <- NA
  data$PE_RATIO[data$PE_RATIO == Inf] <- NA
  
  # Deleting P/E ratios greater than 1000000:
  
  data$PE_RATIO[data$PE_RATIO > 1000000] <- NA
  
  # P/E ratio 12 months (260 working days) % change:
  
  data$PE_12m_chng <- round(data$PE_RATIO / coredata(lag(zoo(data$PE_RATIO), +260, na.pad=TRUE)) - 1, digits = 2)
  
  # Past 12 months (260 working days) Average P/E ratio:
  
  data$Avg_12m_PE <- rowMeans(coredata(lag(zoo(data$PE_RATIO), k=0:259, na.pad=TRUE)))
  
  # 12 months Average P/E ratio: Filling NAs with the average P/E ratio
  
  data$Avg_12m_PE[is.na(data$Avg_12m_PE) == TRUE] <- mean(data$Avg_12m_PE, na.rm = TRUE)
  
  data
  
}

# Merging with EPS Data

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
ADF <- ""
ADF2 <- ""
DFQ <- ""
RFile1 <- ""
RFile2 <- ""
Dir <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings with EPS/'

for (i in 1:length(NStocks)){
  RFile2 <- paste0(Dir, sprintf('%s_Q_Earn.RData', NStocks[i]))
  load(file = RFile2)
  ADF <- DF
  if (dim(ADF)[2] == 1) {
    RFile1 <- sprintf('%s.RData', NStocks[i])
    load(file = RFile1)
    DF$LATEST_ANNOUNCEMENT_DT2 <- as.Date(NA)
    DF$EPS_TTM <- as.numeric(NA)
    DF$EPS_Surprise <- as.numeric(NA)
    DF$Positv_EPS_Surp <- as.integer(NA)
    DF$EPS_YoY_Chng <- as.numeric(NA)
    DF$EPS_PoP_Chng <- as.numeric(NA)
    DF$EPS_TTM_DT <- as.Date(NA)
    DF$EPS_Surprise_DT <- as.Date(NA)
    DF$Positv_EPS_Surp_DT <- as.Date(NA)
    DF$EPS_YoY_Chng_DT <- as.Date(NA)
    DF$EPS_PoP_Chng_DT <- as.Date(NA)
    DF$NEG_PE_RATIO <- as.integer(NA)
    DF$PE_12m_chng <- as.numeric(NA)
    DF$Avg_12m_PE <- as.numeric(NA)
    save(DF, file = RFile1)
    print(i)
    print(NStocks[i])
  } else {
    names(ADF)[1] <- "date"
    names(ADF)[3] <- "Comparable_EPS"
    names(ADF)[4] <- "EPS"
    names(ADF)[5] <- "Estimate_EPS"
    names(ADF)[6] <- "Year_Period"
    ADF$date <- adjust("UnitedStates/NYSE", ADF$date)
    ADF$LATEST_ANNOUNCEMENT_DT2 <- ADF$date
    ADF2 <- ADF
    ADF2$Comparable_EPS[ADF2$Comparable_EPS == -0.000000000000024245362661989844] <- NA
    ADF2$EPS[ADF2$EPS == -0.000000000000024245362661989844] <- NA
    ADF2$Estimate_EPS[ADF2$Estimate_EPS == -0.000000000000024245362661989844] <- NA
    if (dim(ADF2)[1] < 4) {
      ADF$EPS_TTM <- as.numeric(NA)
      ADF$EPS_Surprise <- ifelse(is.na(ADF2$Comparable_EPS) == TRUE, round(ADF2$EPS / ADF2$Estimate_EPS - 1, digits = 3),
                                 round(ADF2$Comparable_EPS / ADF2$Estimate_EPS - 1, digits = 3))
      ADF$EPS_Surprise <- ifelse(ADF2$Estimate_EPS < 0, ADF$EPS_Surprise * -1, ADF$EPS_Surprise)
      ADF$Positv_EPS_Surp <- as.integer(NA)
      ADF$EPS_YoY_Chng <- as.numeric(NA)
      if (dim(ADF2)[1] == 1) {
        ADF$EPS_PoP_Chng <- NA
      } else {
        ADF$EPS_PoP_Chng <- round((ADF2$EPS / coredata(lag(zoo(ADF2$EPS), +1, na.pad=TRUE))) - 1, digits = 3)
        ADF$EPS_PoP_Chng <- ifelse(coredata(lag(zoo(ADF2$EPS), +1, na.pad=TRUE)) < 0, ADF$EPS_PoP_Chng * -1, ADF$EPS_PoP_Chng)
      }
      ADF$EPS_TTM_DT <- as.Date(NA)
      ADF$EPS_Surprise_DT <- as.Date(ifelse(is.na(ADF$EPS_Surprise) == TRUE, NA, ADF$date))
      ADF$Positv_EPS_Surp_DT <- as.Date(NA)
      ADF$EPS_YoY_Chng_DT <- as.Date(NA)
      ADF$EPS_PoP_Chng_DT <- as.Date(ifelse(is.na(ADF$EPS_PoP_Chng) == TRUE, NA, ADF$date))
      ADF <- ADF[,c(6, 1:5, 7:17)]
    } else {
      ADF2$YEAR <- as.numeric(substr(ADF2$Year_Period, 1, 4))
      DFQ <- data.frame("Year_Period" = paste0(rep(min(ADF2$YEAR):max(ADF2$YEAR), each = 4),":Q",rep(1:4)))
      DFQ <- merge(DFQ, ADF2[,c(3:6)], all.x = TRUE)
      DFQ$EPS_TTM <- rowSums(coredata(lag(zoo(DFQ$EPS), k=0:-3, na.pad=TRUE)))
      DFQ$EPS_Surprise <- ifelse(is.na(DFQ$Comparable_EPS) == TRUE, round(DFQ$EPS / DFQ$Estimate_EPS - 1, digits = 3),
                                round(DFQ$Comparable_EPS / DFQ$Estimate_EPS - 1, digits = 3))
      DFQ$EPS_Surprise <- ifelse(DFQ$Estimate_EPS < 0, DFQ$EPS_Surprise * -1, DFQ$EPS_Surprise)
      DFQ$Positv_EPS_Surp <- as.integer(rowSums(coredata(lag(zoo(DFQ$EPS_Surprise > 0), k=0:-3, na.pad=TRUE))))
      if (dim(DFQ)[1] == 4) {
        DFQ$EPS_YoY_Chng <- as.numeric(NA)
      } else {
        DFQ$EPS_YoY_Chng <- round((DFQ$EPS / coredata(lag(zoo(DFQ$EPS), -4, na.pad=TRUE))) - 1, digits = 3)
        DFQ$EPS_YoY_Chng <- ifelse(coredata(lag(zoo(DFQ$EPS), -4, na.pad=TRUE)) < 0, DFQ$EPS_YoY_Chng * -1, DFQ$EPS_YoY_Chng)
      }
      DFQ$EPS_PoP_Chng <- round((DFQ$EPS / coredata(lag(zoo(DFQ$EPS), -1, na.pad=TRUE))) - 1, digits = 3)
      DFQ$EPS_PoP_Chng <- ifelse(coredata(lag(zoo(DFQ$EPS), -1, na.pad=TRUE)) < 0, DFQ$EPS_PoP_Chng * -1, DFQ$EPS_PoP_Chng)
      ADF <- merge(ADF, DFQ[,c(-2:-4)], all.x = TRUE)
      ADF <- ADF[order(ADF$date, decreasing = TRUE),]
      ADF$EPS_TTM_DT <- as.Date(ifelse(is.na(ADF$EPS_TTM) == TRUE, NA, ADF$date))
      ADF$EPS_Surprise_DT <- as.Date(ifelse(is.na(ADF$EPS_Surprise) == TRUE, NA, ADF$date))
      ADF$Positv_EPS_Surp_DT <- as.Date(ifelse(is.na(ADF$Positv_EPS_Surp) == TRUE, NA, ADF$date))
      ADF$EPS_YoY_Chng_DT <- as.Date(ifelse(is.na(ADF$EPS_YoY_Chng) == TRUE, NA, ADF$date))
      ADF$EPS_PoP_Chng_DT <- as.Date(ifelse(is.na(ADF$EPS_PoP_Chng) == TRUE, NA, ADF$date))
    }
    RFile1 <- sprintf('%s.RData', NStocks[i])
    load(file = RFile1)
    DF <- merge(DF, ADF[,c(-1, -3:-6)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF <- PE_Function(DF)
    save(DF, file = RFile1)
    print(i)
    print(NStocks[i])
  }
}

# Transformation function for Twice per Month Equity Data

TxM_FuncT <- function(data) {
  
  # Date for each available value:
  
  data$SHORT_INT_DT <- as.Date(ifelse(is.na(data$SHORT_INT) == TRUE, NA, data$date))
  data$SI_PERCENT_EQUITY_FLOAT_DT <- as.Date(ifelse(is.na(data$SI_PERCENT_EQUITY_FLOAT) == TRUE, NA, data$date))
  data$SHORT_INT_RATIO_DT <- as.Date(ifelse(is.na(data$SHORT_INT_RATIO) == TRUE, NA, data$date))
  
  # % Changes in the short interest:

  if (dim(data)[1] == 1) {
    data$SHORT_INT_Chng <- NA
  } else {
    data$SHORT_INT_Chng <- round(data$SHORT_INT / coredata(lag(zoo(data$SHORT_INT), +1, na.pad=TRUE)) - 1, digits = 2)
  }

  # Date for the last available value:
  
  data$SHORT_INT_Chng_DT <- as.Date(ifelse(is.na(data$SHORT_INT_Chng) == TRUE, NA, coredata(lag(zoo(data$date), +1, na.pad=TRUE))))

  # Extreme low and high Short interest / Equity Float value:
  
  data$SI_EQUITY_FLOAT_LOW <- as.integer(data$SI_PERCENT_EQUITY_FLOAT <= quantile(data$SI_PERCENT_EQUITY_FLOAT, 0.25, na.rm = TRUE))
  data$SI_EQUITY_FLOAT_HIGH <- as.integer(data$SI_PERCENT_EQUITY_FLOAT >= quantile(data$SI_PERCENT_EQUITY_FLOAT, 0.75, na.rm = TRUE))
  data$SI_EQUITY_FLOAT_EXTRM1 <- as.integer(data$SI_PERCENT_EQUITY_FLOAT >= (mean(data$SI_PERCENT_EQUITY_FLOAT, na.rm = TRUE) + 1.5 * sd(data$SI_PERCENT_EQUITY_FLOAT, na.rm = TRUE)))
  data$SI_EQUITY_FLOAT_EXTRM2 <- as.integer(data$SI_PERCENT_EQUITY_FLOAT >= (quantile(data$SI_PERCENT_EQUITY_FLOAT, 0.75, na.rm = TRUE) + 1.5 * (quantile(data$SI_PERCENT_EQUITY_FLOAT, 0.75, na.rm = TRUE) - quantile(data$SI_PERCENT_EQUITY_FLOAT, 0.25, na.rm = TRUE))))

  data 
}

# Adjusting Twice per month data to fill NAs data

NA_FuncT <- function(data) {
  
  data$SHORT_INT <- na.locf(data$SHORT_INT, na.rm = FALSE, fromLast = TRUE)
  data$SI_PERCENT_EQUITY_FLOAT <- na.locf(data$SI_PERCENT_EQUITY_FLOAT, na.rm = FALSE, fromLast = TRUE)
  data$SHORT_INT_RATIO <- na.locf(data$SHORT_INT_RATIO, na.rm = FALSE, fromLast = TRUE)
  data$SHORT_INT_DT <- na.locf(data$SHORT_INT_DT, na.rm = FALSE, fromLast = TRUE)
  data$SI_PERCENT_EQUITY_FLOAT_DT <- na.locf(data$SI_PERCENT_EQUITY_FLOAT_DT, na.rm = FALSE, fromLast = TRUE)
  data$SHORT_INT_RATIO_DT <- na.locf(data$SHORT_INT_RATIO_DT, na.rm = FALSE, fromLast = TRUE)
  data$SHORT_INT_Chng <- na.locf(data$SHORT_INT_Chng, na.rm = FALSE, fromLast = TRUE)
  data$SHORT_INT_Chng_DT <- na.locf(data$SHORT_INT_Chng_DT, na.rm = FALSE, fromLast = TRUE)
  data$SI_EQUITY_FLOAT_LOW <- na.locf(data$SI_EQUITY_FLOAT_LOW, na.rm = FALSE, fromLast = TRUE)
  data$SI_EQUITY_FLOAT_HIGH <- na.locf(data$SI_EQUITY_FLOAT_HIGH, na.rm = FALSE, fromLast = TRUE)
  data$SI_EQUITY_FLOAT_EXTRM1 <- na.locf(data$SI_EQUITY_FLOAT_EXTRM1, na.rm = FALSE, fromLast = TRUE)
  data$SI_EQUITY_FLOAT_EXTRM2 <- na.locf(data$SI_EQUITY_FLOAT_EXTRM2, na.rm = FALSE, fromLast = TRUE)
  
  data 
}

# Merging with Twice per month data
cor(na.omit(DF[,c(2,4)]))

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
ADF <- ""
RFile1 <- ""
RFile2 <- ""
Dir <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Twice per Month Equity Data/'

for (i in 1:length(NStocks)){
  RFile2 <- paste0(Dir, sprintf('%s_2XM.RData', NStocks[i]))
  load(file = RFile2)
  DF$date <- adjust("UnitedStates/NYSE", DF$date)
  ADF <- TxM_FuncT(DF)
  RFile1 <- sprintf('%s.RData', NStocks[i])
  load(file = RFile1)
  DF <- merge(DF, ADF, all.x = TRUE)
  DF <- DF[order(DF$date, decreasing = TRUE),]
  DF <- NA_FuncT(DF)
  save(DF, file = RFile1)
  print(i)
  print(NStocks[i])
}

# Transformation function for Quarterly data

Q_FuncT <- function(data) {
  
  # Actual Date of each earnings announcement:
  
  data$date <- as.Date(ifelse(is.na(data$date) == TRUE, data$Quarter_Ended, data$date))
  data$LATEST_ANNOUNCEMENT_DT <- data$date
  
  # Date for each available value:
  
  data$NUM_EMPLOYEES_DT <- as.Date(ifelse(is.na(data$NUM_OF_EMPLOYEES) == TRUE, NA, data$LATEST_ANNOUNCEMENT_DT))
  data$DEBT_TO_EQY_DT <- as.Date(ifelse(is.na(data$TOT_DEBT_TO_TOT_EQY) == TRUE, NA, data$LATEST_ANNOUNCEMENT_DT))
  data$REV_SURP_DT <- as.Date(ifelse(is.na(data$Revenue_Surprise) == TRUE, NA, data$LATEST_ANNOUNCEMENT_DT))
  
  # Periods with positive revenue surprises:
  
  data$Positv_Rev_Surp <- as.integer(rowSums(coredata(lag(zoo(data$Revenue_Surprise > 0), k=0:3, na.pad=TRUE))))
  data$Positv_Rev_Surp_DT <- as.Date(ifelse(is.na(data$Positv_Rev_Surp) == TRUE, NA, data$date))
  
  # Revenue YoY % Growth:
  
  if (dim(data)[1] <= 4) {
    data$Rev_YoY_Chng <- as.numeric(NA)
  } else {
    data$Rev_YoY_Chng <- round((data$IS_COMP_SALES / coredata(lag(zoo(data$IS_COMP_SALES), +4, na.pad=TRUE))) - 1, digits = 3)
  }
  
  # Revenue PoP % Growth:
  
  data$Rev_PoP_Chng <- round((data$IS_COMP_SALES / coredata(lag(zoo(data$IS_COMP_SALES), +1, na.pad=TRUE))) - 1, digits = 3)
  
  # Date for each available value:
  
  data$Rev_YoY_Chng_DT <- as.Date(ifelse(is.na(data$Rev_YoY_Chng) == TRUE, NA, data$date))
  data$Rev_PoP_Chng_DT <- as.Date(ifelse(is.na(data$Rev_PoP_Chng) == TRUE, NA, data$date))
  
  # Adjusting date to the following business day:
  
  data$date <- adjust("UnitedStates/NYSE", data$date)
  
  data 
}


# Transformation function of Quarterly data in a daily basis

QD_FuncT <- function(data) {
  
  # To fill NAs data:
  
  data$LATEST_ANNOUNCEMENT_DT <- na.locf(data$LATEST_ANNOUNCEMENT_DT, na.rm = FALSE, fromLast = TRUE)
  data$NUM_OF_EMPLOYEES <- na.locf(data$NUM_OF_EMPLOYEES, na.rm = FALSE, fromLast = TRUE)
  data$NUM_OF_EMPLOYEES <- ifelse(is.na(data$NUM_OF_EMPLOYEES) == TRUE, na.locf(data$NUM_OF_EMPLOYEES, na.rm = FALSE), data$NUM_OF_EMPLOYEES)
  data$NUM_EMPLOYEES_DT <- na.locf(data$NUM_EMPLOYEES_DT, na.rm = FALSE, fromLast = TRUE)
  data$NUM_EMPLOYEES_DT <- as.Date(ifelse(is.na(data$NUM_EMPLOYEES_DT) == TRUE, na.locf(data$NUM_EMPLOYEES_DT, na.rm = FALSE), data$NUM_EMPLOYEES_DT))
  data$TOT_DEBT_TO_TOT_EQY <- na.locf(data$TOT_DEBT_TO_TOT_EQY, na.rm = FALSE, fromLast = TRUE)
  data$DEBT_TO_EQY_DT <- na.locf(data$DEBT_TO_EQY_DT, na.rm = FALSE, fromLast = TRUE)
  data$Revenue_Surprise <- na.locf(data$Revenue_Surprise, na.rm = FALSE, fromLast = TRUE)
  data$REV_SURP_DT <- na.locf(data$REV_SURP_DT, na.rm = FALSE, fromLast = TRUE)
  data$Positv_Rev_Surp <- na.locf(data$Positv_Rev_Surp, na.rm = FALSE, fromLast = TRUE)
  data$Positv_Rev_Surp_DT <- na.locf(data$Positv_Rev_Surp_DT, na.rm = FALSE, fromLast = TRUE)
  data$Rev_YoY_Chng <- na.locf(data$Rev_YoY_Chng, na.rm = FALSE, fromLast = TRUE)
  data$Rev_PoP_Chng <- na.locf(data$Rev_PoP_Chng, na.rm = FALSE, fromLast = TRUE)
  data$Rev_YoY_Chng_DT <- na.locf(data$Rev_YoY_Chng_DT, na.rm = FALSE, fromLast = TRUE)
  data$Rev_PoP_Chng_DT <- na.locf(data$Rev_PoP_Chng_DT, na.rm = FALSE, fromLast = TRUE)
  
  # Debt-To-Equity Ratio equals NA if the last available date is 151 days ago or more (a quarter):
  
  data$TOT_DEBT_TO_TOT_EQY[as.numeric(data$date - data$DEBT_TO_EQY_DT) > 150] <- NA
  
  # Debt-To-Equity Ratio 12 months (260 working days) % change:
  data$DEBT_TO_EQY_12mChng <- round(data$TOT_DEBT_TO_TOT_EQY / coredata(lag(zoo(data$TOT_DEBT_TO_TOT_EQY), +260, na.pad=TRUE)) - 1, digits = 2)
  
  data 
}

# Merging with quarterly data and Earnings dates announcements

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
QQ <- ""
ADF <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/'
RFile3 <- ""
Dir3 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings Dates/'

  # New stock list without "2761917Q US"
  NStocks2 <- NStocks[-130]

for (i in 1:length(NStocks2)){
  RFile2 <- paste0(Dir2, sprintf('%s_Q.RData', NStocks2[i]))
  load(file = RFile2)
  if (dim(DF)[1] == 0) {
    RFile1 <- sprintf('%s.RData', NStocks2[i])
    load(file = RFile1)
    DF$NUM_OF_EMPLOYEES <- as.integer(NA)
    DF$TOT_DEBT_TO_TOT_EQY <- as.numeric(NA)
    DF$Revenue_Surprise <- as.numeric(NA)
    DF$LATEST_ANNOUNCEMENT_DT <- as.Date(NA)
    DF$NUM_EMPLOYEES_DT <- as.Date(NA)
    DF$DEBT_TO_EQY_DT <- as.Date(NA)
    DF$REV_SURP_DT <- as.Date(NA)
    DF$Positv_Rev_Surp <- as.integer(NA)
    DF$Positv_Rev_Surp_DT <- as.Date(NA)
    DF$Rev_YoY_Chng <- as.numeric(NA)
    DF$Rev_PoP_Chng <- as.numeric(NA)
    DF$Rev_YoY_Chng_DT <- as.Date(NA)
    DF$Rev_PoP_Chng_DT <- as.Date(NA)
    DF$DEBT_TO_EQY_12mChng <- as.numeric(NA)
    save(DF, file = RFile1)
    print(i)
    print(NStocks2[i])
  } else {
    QQ <- DF
    QQ$TANG_BOOK_VAL_PER_SH <- NULL
    QQ$Revenue_Surprise <- round((QQ$IS_COMP_SALES / QQ$BEST_SALES) - 1, digits = 3)
    RFile3 <- paste0(Dir3, sprintf('%s_Q_EDate.RData', NStocks2[i]))
    load(file = RFile3)
    ADF <- merge(QQ, DF, all.x = TRUE)
    ADF <- ADF[order(ADF$date, decreasing = TRUE),]
    names(ADF)[1] <- "Quarter_Ended"
    names(ADF)[7] <- "date"
    ADF <- Q_FuncT(ADF)
    ADF$Quarter_Ended <- NULL
    RFile1 <- sprintf('%s.RData', NStocks2[i])
    load(file = RFile1)
    DF <- merge(DF, ADF[,c(-3,-4)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF <- QD_FuncT(DF)
    save(DF, file = RFile1)
    print(i)
    print(NStocks2[i])
  }
}

  # Merging with quarterly data and Earnings dates announcements for "2761917Q US

  load(file = 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/2761917Q_US_Q.RData')
  ADF <- DF
  ADF$TANG_BOOK_VAL_PER_SH <- NULL
  ADF$Revenue_Surprise <- round((ADF$IS_COMP_SALES / ADF$BEST_SALES) - 1, digits = 3)
  ADF <- Q_FuncT(ADF)
  RFile1 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data/2761917Q_US.RData'
  load(file = RFile1)
  DF <- merge(DF, ADF[,c(-4,-5)], all.x = TRUE)
  DF <- DF[order(DF$date, decreasing = TRUE),]
  DF <- QD_FuncT(DF)
  save(DF, file = RFile1)
  
# Correlation with commodities:
  
setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')
  
load("C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Market Data/Daily-market-Data.RData")
Gold$GoldChng <- (Gold$PX_LAST / coredata(lag(zoo(Gold$PX_LAST), -1, na.pad=TRUE))) - 1
Oil$OilChng <- (Oil$PX_LAST / coredata(lag(zoo(Oil$PX_LAST), -1, na.pad=TRUE))) - 1
NatuGas$GasChng <- (NatuGas$PX_LAST / coredata(lag(zoo(NatuGas$PX_LAST), -1, na.pad=TRUE))) - 1
Copper$CopperChng <- (Copper$PX_LAST / coredata(lag(zoo(Copper$PX_LAST), -1, na.pad=TRUE))) - 1
Silver$SilverChng <- (Silver$PX_LAST / coredata(lag(zoo(Silver$PX_LAST), -1, na.pad=TRUE))) - 1
Gold$PX_LAST <- NULL
Oil$PX_LAST <- NULL
NatuGas$PX_LAST <- NULL
Copper$PX_LAST <- NULL
Silver$PX_LAST <- NULL
  
DF <- ""
RFile1 <- ""
  
for (i in 1:length(NStocks)){
  RFile1 <- sprintf('%s.RData', NStocks[i])
  load(file = RFile1)
  DF <- merge(DF, Gold, all.x = TRUE)
  DF <- merge(DF, Oil, all.x = TRUE)
  DF <- merge(DF, NatuGas, all.x = TRUE)
  DF <- merge(DF, Copper, all.x = TRUE)
  DF <- merge(DF, Silver, all.x = TRUE)
  # Rolling Correlations
  DF$Corr30d_Gold <- roll_cor(DF$Pr_Rt, DF$GoldChng, width = 30, min_obs = 20)
  DF$Corr30d_Oil <- roll_cor(DF$Pr_Rt, DF$OilChng, width = 30, min_obs = 20)
  DF$Corr30d_Gas <- roll_cor(DF$Pr_Rt, DF$GasChng, width = 30, min_obs = 20)
  DF$Corr30d_Copper <- roll_cor(DF$Pr_Rt, DF$CopperChng, width = 30, min_obs = 20)
  DF$Corr30d_Silver <- roll_cor(DF$Pr_Rt, DF$SilverChng, width = 30, min_obs = 20)
  DF$Corr150d_Gold <- roll_cor(DF$Pr_Rt, DF$GoldChng, width = 150, min_obs = 100)
  DF$Corr150d_Oil <- roll_cor(DF$Pr_Rt, DF$OilChng, width = 150, min_obs = 100)
  DF$Corr150d_Gas <- roll_cor(DF$Pr_Rt, DF$GasChng, width = 150, min_obs = 100)
  DF$Corr150d_Copper <- roll_cor(DF$Pr_Rt, DF$CopperChng, width = 150, min_obs = 100)
  DF$Corr150d_Silver <- roll_cor(DF$Pr_Rt, DF$SilverChng, width = 150, min_obs = 100)
  DF <- DF[order(DF$date, decreasing = TRUE),]
  save(DF, file = RFile1)
  print(i)
  print(NStocks[i])
}

# Merging with Dividends Data

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
DV <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Dividends/'

for (i in 1:length(NStocks)){
  RFile2 <- paste0(Dir2, sprintf('%s_Q_Div.RData', NStocks[i]))
  load(file = RFile2)
  if (class(DF) == "NULL")  {
    RFile1 <- sprintf('%s.RData', NStocks[i])
    load(file = RFile1)
    DF$Ex_Div <- as.Date(NA)
    save(DF, file = RFile1)
    print(i)
    print(NStocks[i])
  } else {
    DV <- subset(DF, `Dividend Type` %in% c("Regular Cash", "Capital Gains", "Income"))
    names(DV)[5] <- "Ex_Div"
    DV$Ex_Div <- adjust("UnitedStates/NYSE", DV$Ex_Div)
    DV$date <- DV$Ex_Div
    RFile1 <- sprintf('%s.RData', NStocks[i])
    load(file = RFile1)
    DF <- merge(DF, DV[,c(5, 8)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF$Ex_Div <- na.locf(DF$Ex_Div, na.rm = FALSE)
    save(DF, file = RFile1)
    print(i)
    print(NStocks[i])
  }
}

# Merging with Mergers & acquisitions Data Sets:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
MA1 <- ""
MA2 <- ""
MAX <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Mergers & Acquisitions/'

for (i in 1:length(NStocks)){
  RFile2 <- paste0(Dir2, sprintf('%s_M&A.RData', NStocks[i]))
  load(file = RFile2)
  if (class(DF) == "NULL")  {
    RFile1 <- sprintf('%s.RData', NStocks[i])
    load(file = RFile1)
    DF$Merg_Acq_Date <- as.Date(NA)
    DF$Merg_Acq_Deal_Status <- as.character(NA)
    DF$Merg_Acq_Payment_Type <- as.character(NA)
    DF$Next_Merg_Acq_Date <- as.Date(NA)
    DF$Next_Merg_Acq_Payment_Type <- as.character(NA)
    DF$Last_Merg_Acq <- as.numeric(NA)
    DF$Inv_JV_Date <- as.Date(NA)
    DF$Inv_JV_Deal_Status <- as.character(NA)
    DF$Inv_JV_Deal_Type <- as.character(NA)
    DF$Inv_JV_Payment_Type <- as.character(NA)
    DF$Next_Inv_JV_Date <- as.Date(NA)
    DF$Next_Inv_JV_Deal_Type <- as.character(NA)
    DF$Next_Inv_JV_Payment_Type <- as.character(NA)
    save(DF, file = RFile1)
    print(i)
    print(NStocks[i])
  }  else {
    MA1 <- subset(DF, `Deal Type` == "M&A")
    MA2 <- subset(DF, `Deal Type` %in% c("INV", "JV"))
    names(MA1)[3] <- "Merg_Acq_Date"
    names(MA1)[5] <- "Merg_Acq_Deal_Status"
    names(MA1)[7] <- "Merg_Acq_Payment_Type"
    MA1$Merg_Acq_Date <- adjust("UnitedStates/NYSE", MA1$Merg_Acq_Date)
    MA1$date <- MA1$Merg_Acq_Date
    MA1$Next_Merg_Acq_Date <- MA1$Merg_Acq_Date
    MA1$Next_Merg_Acq_Payment_Type <- MA1$Merg_Acq_Payment_Type
    RFile1 <- sprintf('%s.RData', NStocks[i])
    load(file = RFile1)
    DF <- merge(DF, MA1[,c(3, 5, 7:10)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF$Merg_Acq_Date <- na.locf(DF$Merg_Acq_Date, na.rm = FALSE, fromLast = TRUE)
    DF$Merg_Acq_Deal_Status <- na.locf(DF$Merg_Acq_Deal_Status, na.rm = FALSE, fromLast = TRUE)
    DF$Merg_Acq_Payment_Type <- na.locf(DF$Merg_Acq_Payment_Type, na.rm = FALSE, fromLast = TRUE)
    DF$Next_Merg_Acq_Date <- na.locf(DF$Next_Merg_Acq_Date, na.rm = FALSE)
    DF$Next_Merg_Acq_Payment_Type <- na.locf(DF$Next_Merg_Acq_Payment_Type, na.rm = FALSE)
    # Probably a merger or acquisition by another company (A time lapse of 365 days or less until the last trading date):
    MAX <- max(DF$date)
    if (MAX == "2019-12-31") {
      DF$Last_Merg_Acq <- 0
    } else {
      DF$Last_Merg_Acq <- ifelse(as.numeric(MAX - DF$Merg_Acq_Date) <= 365, 1, 0)
    }
    names(MA2)[3] <- "Inv_JV_Date"
    names(MA2)[5] <- "Inv_JV_Deal_Status"
    names(MA2)[6] <- "Inv_JV_Deal_Type"
    names(MA2)[7] <- "Inv_JV_Payment_Type"
    MA2$Inv_JV_Date <- adjust("UnitedStates/NYSE", MA2$Inv_JV_Date)
    MA2$date <- MA2$Inv_JV_Date
    MA2$Next_Inv_JV_Date <- MA2$Inv_JV_Date
    MA2$Next_Inv_JV_Deal_Type <- MA2$Inv_JV_Deal_Type
    MA2$Next_Inv_JV_Payment_Type <- MA2$Inv_JV_Payment_Type
    DF <- merge(DF, MA2[,c(3, 5:11)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF$Inv_JV_Date <- na.locf(DF$Inv_JV_Date, na.rm = FALSE, fromLast = TRUE)
    DF$Inv_JV_Deal_Status <- na.locf(DF$Inv_JV_Deal_Status, na.rm = FALSE, fromLast = TRUE)
    DF$Inv_JV_Deal_Type <- na.locf(DF$Inv_JV_Deal_Type, na.rm = FALSE, fromLast = TRUE)
    DF$Inv_JV_Payment_Type <- na.locf(DF$Inv_JV_Payment_Type, na.rm = FALSE, fromLast = TRUE)
    DF$Next_Inv_JV_Date <- na.locf(DF$Next_Inv_JV_Date, na.rm = FALSE)
    DF$Next_Inv_JV_Deal_Type <- na.locf(DF$Next_Inv_JV_Deal_Type, na.rm = FALSE)
    DF$Next_Inv_JV_Payment_Type <- na.locf(DF$Next_Inv_JV_Payment_Type, na.rm = FALSE)
    save(DF, file = RFile1)
    print(i)
    print(NStocks[i])
  }
}

# Subseting the data with investment signal (IS):

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

IS <- data.frame()
RFile <- ""
datfr <- ""

for (i in 1:length(NStocks)){
  RFile <- sprintf('%s.RData', NStocks[i])
  load(file = RFile)
  datfr <- subset(DF, Strategy == "Buy" | Strategy == "Sell")
  IS <- rbind(IS, datfr)
  print(i)
  print(NStocks[i])
}

# Checking data frame

str(IS)
summary(IS)
head(IS)

# Removing variables:

IS$AvgVol30D <- NULL
IS$SDVol30D <- NULL
IS$Vol30D0.75 <- NULL
IS$MCloseUp <- NULL
IS$MLow <- NULL
IS$MCloseDown <- NULL
IS$MVolume <- NULL
IS$Up <- NULL
IS$Down <- NULL
IS$MHigh <- NULL
IS$MomVar <- NULL
IS$BID <- NULL
IS$ASK <- NULL
IS$Pr_Rt <- NULL
IS$Vol_Chng <- NULL
IS$TOTAL_OPEN_INT <- NULL
IS$OPEN_INT_TOTAL_CALL <- NULL
IS$OPEN_INT_TOTAL_PUT <- NULL
IS$Avg_TOT_OPEN_INT_30D <- NULL
IS$Avg_CALL_OPEN_INT_30D <- NULL
IS$Avg_PUT_OPEN_INT_30D <- NULL
IS$Avg_PUT_CALL_OPEN_INT_RATIO_30D <- NULL
IS$TOT_OPT_VOLUME <- NULL
IS$VOLUME_TOTAL_CALL <- NULL
IS$VOLUME_TOTAL_PUT <- NULL
IS$Avg_TOT_OPT_VOLUME_30D <- NULL
IS$SD_TOT_OPT_VOLUME_30D <- NULL
IS$TOT_OPT_VOL_Chng <- NULL
IS$TOT_OPT_VOL_Up_5D <- NULL
IS$Avg_VOLUME_TOTAL_CALL_30D <- NULL
IS$SD_VOLUME_TOTAL_CALL_30D <- NULL
IS$CALL_VOL_Chng <- NULL
IS$CALL_VOL_Up_5D <- NULL
IS$Avg_VOLUME_TOTAL_PUT_30D <- NULL
IS$SD_VOLUME_TOTAL_PUT_30D <- NULL
IS$PUT_VOL_Chng <- NULL
IS$PUT_VOL_Up_5D <- NULL
IS$Avg_PUT_CALL_VOL_RATIO_30D <- NULL
IS$Vol30D1 <- NULL
IS$MVolume1 <- NULL
IS$Vol30D1.25 <- NULL
IS$MVolume1.25 <- NULL
IS$Vol30D1.5 <- NULL
IS$MVolume1.5 <- NULL
IS$EPS_TTM <- NULL
IS$EPS_TTM_DT <- NULL
IS$IS_COMP_SALES <- NULL
IS$BEST_SALES <- NULL
IS$DEBT_TO_EQY_DT <- NULL
IS$GoldChng <- NULL
IS$OilChng <- NULL
IS$GasChng <- NULL
IS$CopperChng <- NULL
IS$SilverChng <- NULL

# Removing duplicate observation in "FNMA US" and "WRE US"

which(IS$date == "2011-08-08" & IS$Symbol == "WRE US")

IS <- IS[-c(4892, 4893, 2089),]

# Merging with the static data:

Static$Symbol <- gsub('.{7}$', '', Static$Symbol)
IS <- merge(IS, Static[,c(1:4, 7)], all.x = TRUE)

# Sector variables:

IS$GICS_SECTOR_NAME <- as.factor(IS$GICS_SECTOR_NAME)
IS$GICS_INDUSTRY_GROUP_NAME <- as.factor(IS$GICS_INDUSTRY_GROUP_NAME)
IS$GICS_INDUSTRY_NAME <- as.factor(IS$GICS_INDUSTRY_NAME)
IS$GICS_SUB_INDUSTRY_NAME <- as.factor(IS$GICS_SUB_INDUSTRY_NAME)

IS$GICS_SECTOR_NAME <- relevel(IS$GICS_SECTOR_NAME, "")
IS$GICS_INDUSTRY_GROUP_NAME <- relevel(IS$GICS_INDUSTRY_GROUP_NAME, "")
IS$GICS_INDUSTRY_NAME <- relevel(IS$GICS_INDUSTRY_NAME, "")
IS$GICS_SUB_INDUSTRY_NAME <- relevel(IS$GICS_SUB_INDUSTRY_NAME, "")

# Year:

library(lubridate)
IS$Year <- year(IS$date)

# Market Cap Classification:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Russell Market Cap')

Cap <- data.frame()
YY <- c(1995:2019)
RFile <- ""

for (i in 1:length(YY)){
  RFile <- sprintf('Large_Cap_%s.RData', YY[i])
  load(file = RFile)
  Index$SymbYr <- paste(Index$Symbol, YY[i], sep = " ")
  Index$Market_Cap <- "Large-Cap"
  Cap <- rbind(Cap, Index)
  print(YY[i])
}

for (i in 1:length(YY)){
  RFile <- sprintf('Mid_Cap_%s.RData', YY[i])
  load(file = RFile)
  Index$SymbYr <- paste(Index$Symbol, YY[i], sep = " ")
  Index$Market_Cap <- "Mid-Cap"
  Cap <- rbind(Cap, Index)
  print(YY[i])
}

for (i in 1:length(YY)){
  RFile <- sprintf('Samll_Cap_%s.RData', YY[i])
  load(file = RFile)
  Index$SymbYr <- paste(Index$Symbol, YY[i], sep = " ")
  Index$Market_Cap <- "Small-Cap"
  Cap <- rbind(Cap, Index)
  print(YY[i])
}

Cap$Symbol <- NULL
IS$SymbYr <- paste(IS$Symbol, IS$Year, sep = " ")
IS <- merge(IS, Cap, by ="SymbYr", all.x = TRUE)
IS$Market_Cap <- as.factor(IS$Market_Cap)
IS$SymbYr <- NULL

# Filtered by acceptable investment signals:

  # Prices greater or equall to 15:

  IS <- subset(IS, Close_unadj >= 15)
  
  # Do not invest in small-medium biotechnology and pharmaceuticals companies
  
  IS <- subset(IS, !(GICS_INDUSTRY_GROUP_NAME == "Pharmaceuticals, Biotechnology" & Market_Cap == "Small-Cap"))
  IS <- subset(IS, !(GICS_INDUSTRY_GROUP_NAME == "Pharmaceuticals, Biotechnology" & Market_Cap == "Mid-Cap"))
  
  # A time lapse of more than 10 business days between the investment signal and the Earnings release
  
  IS$Next_Earnings[is.na(IS$Next_Earnings) == TRUE] <- as.Date("2020-01-03")
  
  library(RQuantLib) # For businessDaysBetween
  
  IS$Days_Next_Earn <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Next_Earnings, includeFirst = FALSE, includeLast = TRUE)
  
  IS <- subset(IS, Days_Next_Earn > 10 | Days_Next_Earn == 0)
  
  # Only after-market announcements are accepted in cases of 11 business days between the investment signal and the Earnings release
  
  D11 <- subset(IS, Days_Next_Earn == 11)
  
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "Bef-mkt"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "07:00"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "07:15"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "07:30"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:00"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:30"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:31"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:45"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "09:30"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "10:00"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "11:00"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "12:00"))
  IS <- subset(IS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "13:00"))
  
  # Only announcements before mid-day session are accepted in cases of 0 business days between the investment signal and the Earnings release
  # It means, before 12:45 AM
  
  D11 <- subset(IS, Days_Next_Earn == 0)
  
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "Aft-mkt"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:00"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:01"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:02"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:05"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:09"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:10"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:20"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:23"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:30"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "17:00"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "17:05"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "18:30"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "18:32"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "18:34"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "19:00"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "20:00"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "20:01"))
  IS <- subset(IS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "20:05"))
  
  # A time lapse of more than 11 business days between the investment signal and corporate actions
  
  IS$Ex_Date[is.na(IS$Ex_Date) == TRUE] <- as.Date("2020-01-03")
  
  IS$Days_Next_CorpAct <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Ex_Date, includeFirst = FALSE, includeLast = TRUE)
  
  IS <- subset(IS, Days_Next_CorpAct > 11)
  
  # Do not invest after news about corporate actions

  IS$Declared_Date[is.na(IS$Declared_Date) == TRUE] <- as.Date("1985-12-31")

  IS$Days_Last_Corp_Act <- businessDaysBetween("UnitedStates/NYSE", IS$Declared_Date, IS$date, includeFirst = TRUE, includeLast = TRUE)

  IS <- subset(IS, Days_Last_Corp_Act > (IS$DT + 1))

  # Do not invest after news about Spinoff and buybacks

  IS$Spin_Buy_Date[is.na(IS$Spin_Buy_Date) == TRUE] <- as.Date("1985-12-31")

  IS$Days_Last_Spin_Buy <- businessDaysBetween("UnitedStates/NYSE", IS$Spin_Buy_Date, IS$date, includeFirst = TRUE, includeLast = TRUE)

  IS <- subset(IS, Days_Last_Spin_Buy > (IS$DT + 1))

  # Do not invest in cases of 1 business day between the Merger or acquisition and the investment sign:

  IS$Merg_Acq_Date[is.na(IS$Merg_Acq_Date) == TRUE] <- as.Date("1985-12-31")

  IS$Days_Past_Merg_Acq <- businessDaysBetween("UnitedStates/NYSE", IS$Merg_Acq_Date, IS$date, includeFirst = TRUE, includeLast = TRUE)

  IS <- subset(IS, Days_Past_Merg_Acq > 1)
  # 3752 Obs
  
  # Do not invest in stocks correlated with commodities:
  # Just for those companies classified in the Gold, Oil, Gas, Silver or Copper GICS Sub-Industry

  SUB <- subset(IS, GICS_INDUSTRY_NAME == "Energy Equipment & Services", select = c(Symbol, Corr30d_Oil, Corr150d_Oil))
  SUB <- subset(IS, GICS_INDUSTRY_NAME == "Energy Equipment & Services", select = c(Symbol, Corr30d_Gas, Corr150d_Gas))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Integrated Oil & Gas", select = c(Symbol, Corr30d_Oil, Corr150d_Oil))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Integrated Oil & Gas", select = c(Symbol, Corr30d_Gas, Corr150d_Gas))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Oil & Gas Exploration & Produc", select = c(Symbol, Corr30d_Oil, Corr150d_Oil))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Oil & Gas Exploration & Produc", select = c(Symbol, Corr30d_Gas, Corr150d_Gas))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Oil & Gas Refining & Marketing", select = c(Symbol, Corr30d_Oil, Corr150d_Oil))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Oil & Gas Refining & Marketing", select = c(Symbol, Corr30d_Gas, Corr150d_Gas))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Oil & Gas Storage & Transporta", select = c(Symbol, Corr30d_Oil, Corr150d_Oil))
  SUB <- subset(IS, GICS_SUB_INDUSTRY_NAME == "Oil & Gas Storage & Transporta", select = c(Symbol, Corr30d_Gas, Corr150d_Gas))
  SUB <- IS[IS$GICS_INDUSTRY_NAME == "Gas Utilities", c("Symbol", "Corr30d_Gas", "Corr150d_Gas")]
  SUB <- IS[IS$GICS_SUB_INDUSTRY_NAME == "Gold", c("Symbol", "Corr30d_Gold", "Corr150d_Gold")]
  SUB <- IS[IS$GICS_SUB_INDUSTRY_NAME == "Copper", c("Symbol", "Corr30d_Copper", "Corr150d_Copper")]
  SUB <- IS[IS$GICS_SUB_INDUSTRY_NAME == "Silver", c("Symbol", "Corr30d_Silver", "Corr150d_Silver")]
  
  IS <- subset(IS, !(GICS_INDUSTRY_NAME == "Energy Equipment & Services" & Corr30d_Oil >= 0.7))
  IS <- subset(IS, !(GICS_INDUSTRY_NAME == "Energy Equipment & Services" & Corr30d_Gas >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Integrated Oil & Gas" & Corr30d_Oil >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Integrated Oil & Gas" & Corr30d_Gas >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Oil & Gas Exploration & Produc" & Corr30d_Oil >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Oil & Gas Exploration & Produc" & Corr30d_Gas >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Oil & Gas Refining & Marketing" & Corr30d_Oil >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Oil & Gas Refining & Marketing" & Corr30d_Gas >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Oil & Gas Storage & Transporta" & Corr30d_Oil >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Oil & Gas Storage & Transporta" & Corr30d_Gas >= 0.7))
  IS <- subset(IS, !(GICS_INDUSTRY_NAME == "Gas Utilities" & Corr30d_Gas >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Gold" & Corr30d_Gold >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Copper" & Corr30d_Copper >= 0.7))
  IS <- subset(IS, !(GICS_SUB_INDUSTRY_NAME == "Silver" & Corr30d_Silver >= 0.7))
  # 3723 Obs
  
  # Do not invest after news of mergers or acquisitions by another company:
  
  LST <- subset(IS, Last_Merg_Acq == 1)
  View(LST[,c(1,2,182:184)])
  
    # Companies with news about mergers or acquisitions by another company:
    # 0231874D US / 2006-07-07
    # 0618828D US / 2008-05-01
    # 1030770Q US / 2002-05-23
    # 127929Q US / 1998-03-24
    # 1500785D US / 2016-02-19
    # 1763171D US / 1995-07-28
    # 202550Q US / 1996-12-10
    # 2211Q US / 1997-09-30
    # 2250091Q US / 2002-05-01
    # 2899Q US / 1998-11-24
    # 3011Q US / 2000-01-24
    # 3011Q US / 2000-01-21 
    # 3073Q US / 1999-03-05
    # 3077Q US / 1998-08-04
    # 3190818Q US / 2008-08-04
    # 739306Q US / 2000-11-03
    # 754895Q US / 2000-10-05
    # 754895Q US / 2000-10-04
    # BRL US / 2008-07-21
    # CEPH US / 2011-08-08
    # ILN US / 2000-01-13
    # ILN US / 2000-01-12
    # IPCM US / 2015-08-05
    # JNC US / 2007-06-27
    # KOMG US / 2007-06-29
    # KRON US / 2007-03-26
    # KRON US / 2007-03-22
    # LVLT US / 2017-06-22
    # NCH US / 2001-12-28
    # NCH US / 2001-12-27
    # NMK US / 2002-01-09
    # RBK US / 2006-01-24
    # RYK US / 1997-10-31
    # TCOMA US / 1998-12-10
    # TFCFA US / 2018-06-15
    # TMC US / 2000-03-17
    # UDS US / 2001-05-11
    # VLSI US / 1999-03-04
    # XYLN US / 1999-03-03

  which(IS$date == "1998-03-24" & IS$Symbol == "127929Q US")
  
  IS <- IS[-c(7,17,50,72,110,171,188,196,198,241,265,266,272,273,290,409,411,412,838,953,1894,
              1895,1922,1989,2097,2110,2111,2220,2491,2492,2545,2882,3004,3245,3278,3320,3407,
              3520,3703),]

  # 3684 Obs

# Mergers and acquisitions during the momentum trend:

IS$Event_Driven_Merg_Acq <- as.integer(IS$Days_Past_Merg_Acq <= (IS$DT + 1))

# Investments and joint ventures during the momentum trend:

IS$Inv_JV_Date[is.na(IS$Inv_JV_Date) == TRUE] <- as.Date("1985-12-31")

IS$Days_Past_Inv_JV <- businessDaysBetween("UnitedStates/NYSE", IS$Inv_JV_Date, IS$date, includeFirst = TRUE, includeLast = TRUE)

IS$Event_Driven_Inv_JV <- as.integer(IS$Days_Past_Inv_JV <= (IS$DT + 1))

# Refreshing the factor variable "Strategy", to remove the unnecessary category:

IS$Strategy <- factor(IS$Strategy)

# Max. % change momentum correction in 10 days:

IS$MaxCorr10D <- ifelse(IS$Strategy == "Buy", (IS$MaxPrice10D/IS$PX_LAST)-1, (IS$MinPrice10D/IS$PX_LAST)-1)

# Date of Max. % change momentum correction in 10 days:
# Counting forward from the investment signal

IS$Date_MaxCorr10D <- ifelse(IS$Strategy == "Buy", IS$Date_MaxPrice10D, IS$Date_MinPrice10D)

# Max. % change in the continuity of the momentum in 10 days:

IS$MaxMoment10D <- ifelse(IS$Strategy == "Buy", (IS$MinPrice10D/IS$PX_LAST)-1,(IS$MaxPrice10D/IS$PX_LAST)-1)

# Date of Max. % change in the continuity of the momentum in 10 days:
# Counting forward from the investment signal

IS$Date_MaxMoment10D <- ifelse(IS$Strategy == "Buy", IS$Date_MinPrice10D, IS$Date_MaxPrice10D)

# Max. % change momentum correction in 15 days:

IS$MaxCorr15D <- ifelse(IS$Strategy == "Buy", (IS$MaxPrice15D/IS$PX_LAST)-1, (IS$MinPrice15D/IS$PX_LAST)-1)

# Date of Max. % change momentum correction in 15 days:
# Counting forward from the investment signal

IS$Date_MaxCorr15D <- ifelse(IS$Strategy == "Buy", IS$Date_MaxPrice15D, IS$Date_MinPrice15D)

# Max. % change in the continuity of the momentum in 15 days:

IS$MaxMoment15D <- ifelse(IS$Strategy == "Buy", (IS$MinPrice15D/IS$PX_LAST)-1, (IS$MaxPrice15D/IS$PX_LAST)-1)

# Date of Max. % change in the continuity of the momentum in 15 days:
# Counting forward from the investment signal

IS$Date_MaxMoment15D <- ifelse(IS$Strategy == "Buy", IS$Date_MinPrice15D, IS$Date_MaxPrice15D)

# Log transformation of the dependent variables:

IS$LogMaxCorr10D <- log(IS$MaxCorr10D + 1)
IS$LogMaxCorr15D <- log(IS$MaxCorr15D + 1)
IS$LogMaxMoment10D <- log(IS$MaxMoment10D + 1)
IS$LogMaxMoment15D <- log(IS$MaxMoment15D + 1)

# Short interest data is equal to NA if the last available date is 20/36 days ago or more:

IS$SHORT_INT <- ifelse(IS$date >= "2007-09-14" & as.numeric(IS$date - IS$SHORT_INT_DT) > 19, NA,
                         ifelse(IS$date < "2007-09-14" & as.numeric(IS$date - IS$SHORT_INT_DT) > 35, NA, IS$SHORT_INT))
IS$SI_PERCENT_EQUITY_FLOAT <- ifelse(IS$date >= "2007-09-14" & as.numeric(IS$date - IS$SI_PERCENT_EQUITY_FLOAT_DT) > 19, NA,
                                       ifelse(IS$date < "2007-09-14" & as.numeric(IS$date - IS$SI_PERCENT_EQUITY_FLOAT_DT) > 35, NA, IS$SI_PERCENT_EQUITY_FLOAT))
IS$SHORT_INT_RATIO <- ifelse(IS$date >= "2007-09-14" & as.numeric(IS$date - IS$SHORT_INT_RATIO_DT) > 19, NA,
                               ifelse(IS$date < "2007-09-14" & as.numeric(IS$date - IS$SHORT_INT_RATIO_DT) > 35, NA, IS$SHORT_INT_RATIO))
IS$SHORT_INT_Chng <- ifelse(is.na(IS$SHORT_INT) == TRUE, NA, 
                              ifelse(IS$date >= "2007-09-14" & as.numeric(IS$date - IS$SHORT_INT_Chng_DT) > 38, NA,
                              ifelse(IS$date < "2007-09-14" & as.numeric(IS$date - IS$SHORT_INT_Chng_DT) > 70, NA, IS$SHORT_INT_Chng)))
IS$SI_EQUITY_FLOAT_LOW <- ifelse(is.na(IS$SI_PERCENT_EQUITY_FLOAT) == TRUE, NA, IS$SI_EQUITY_FLOAT_LOW)
IS$SI_EQUITY_FLOAT_HIGH <- ifelse(is.na(IS$SI_PERCENT_EQUITY_FLOAT) == TRUE, NA, IS$SI_EQUITY_FLOAT_HIGH)
IS$SI_EQUITY_FLOAT_EXTRM1 <- ifelse(is.na(IS$SI_PERCENT_EQUITY_FLOAT) == TRUE, NA, IS$SI_EQUITY_FLOAT_EXTRM1)
IS$SI_EQUITY_FLOAT_EXTRM2 <- ifelse(is.na(IS$SI_PERCENT_EQUITY_FLOAT) == TRUE, NA, IS$SI_EQUITY_FLOAT_EXTRM2)

# Short interest ratio > 8:

IS$SHORT_INT_RATIO_8 <- as.integer(IS$SHORT_INT_RATIO >= 8)

# Removing 12 months P/E Average equals to infinite or greater than 1000000

IS$Avg_12m_PE[IS$Avg_12m_PE > 1000000] <- NA
IS$Avg_12m_PE[IS$Avg_12m_PE == Inf] <- NA

# P/E against the 12 months Average:

IS$PE_Relatv_12m <- IS$PE_RATIO / IS$Avg_12m_PE
  
# Changing the locale for my current R session for English language

Sys.setlocale("LC_ALL", "C")

# Creating month's, the day of the week and quarter's variables:

IS$Month <- as.factor(months(IS$date))
IS$Weekday <- as.factor(weekdays(IS$date))
IS$Quarter <- as.factor(quarters(IS$date))

IS$Month <- factor(IS$Month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
IS$Weekday <- factor(IS$Weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
IS$Quarter <- factor(IS$Quarter, c("Q1", "Q2", "Q3", "Q4"))

IS$MonthN <- as.integer(IS$Month)
IS$WeekdayN <- as.integer(IS$Weekday)
IS$QuarterN <- as.integer(IS$Quarter)

# P/E per Industry, sub-industry, year and stock:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

library(lubridate)

PE <- data.frame()
DT <- ""
RFile1 <- ""
t <- ""

for (i in 1:length(Stocks)){
  RFile1 <- sprintf('%s.RData', Stocks[i])
  load(file = RFile1)
  t <- class(try(aggregate(PE_RATIO ~ year(date), data = DF, mean)))
  if(t == "try-error") next
  DF$PE_RATIO[DF$PE_RATIO > 1000000] <- NA
  DF$PE_RATIO[DF$PE_RATIO == Inf] <- NA
  DT <- aggregate(PE_RATIO ~ year(date), data = DF, mean)
  DT$Symbol <- Stocks[i]
  PE <- rbind(PE, DT)
  print(i)
  print(Stocks[i])
}

PE$Symbol <- paste0(mgsub(PE$Symbol, pattern=c("-","_"), replacement=c("/"," "))," Equity")
names(PE)[1] <- "Year"

PE <- merge(PE, Static[,c(3, 4, 7)], all.x = TRUE)

Ind <- aggregate(PE_RATIO ~ Year + GICS_INDUSTRY_NAME, data = PE, mean)
names(Ind)[3] <- "PE_INDUSTRY"
SubInd <- aggregate(PE_RATIO ~ Year + GICS_SUB_INDUSTRY_NAME, data = PE, mean)
names(SubInd)[3] <- "PE_COMPETITORS"

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(PE, Ind, SubInd, file="PE_Ind.RData")

YY <- ""
IND <- ""
COMP <- ""
IS$PE_INDUSTRY <- as.numeric(0)
IS$PE_COMPETITORS <- as.numeric(0)

for (i in 1:nrow(IS)){
  if (IS$GICS_SECTOR_NAME[i] == "") {
    IS$PE_INDUSTRY[i] = NA
    IS$PE_COMPETITORS[i] = NA
  } else {
    if (IS$MonthN[i] >= 7) {
      YY = IS$Year[i]
      IND = as.character(IS$GICS_INDUSTRY_NAME[i])
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(Ind$PE_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]) == 0){
        IS$PE_INDUSTRY[i] = NA
      } else {
        IS$PE_INDUSTRY[i] = Ind$PE_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]
      }
      if (length(SubInd$PE_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0){
        IS$PE_COMPETITORS[i] = NA
      } else {
        IS$PE_COMPETITORS[i] = SubInd$PE_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    } else {
      YY = IS$Year[i] - 1
      IND = as.character(IS$GICS_INDUSTRY_NAME[i])
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(Ind$PE_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]) == 0) {
        IS$PE_INDUSTRY[i] = NA
      } else {
        IS$PE_INDUSTRY[i] = Ind$PE_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]
      }
      if (length(SubInd$PE_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0) {
        IS$PE_COMPETITORS[i] = NA
      } else {
        IS$PE_COMPETITORS[i] = SubInd$PE_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    }
  }
}

# P/E against the sector Average:

IS$PE_Relatv_IND <- ifelse(IS$PE_INDUSTRY == 0, 0, IS$PE_RATIO / IS$PE_INDUSTRY)

# P/E against competitors Average:

IS$PE_Relatv_COMP <- ifelse(IS$PE_COMPETITORS == 0, 0, IS$PE_RATIO / IS$PE_COMPETITORS)

# EPS Surprises are equal to NA if the last available date is 151 days ago or more (a quarter):

IS$EPS_Surprise[as.numeric(IS$date - IS$EPS_Surprise_DT) > 150] <- NA

# Positive EPS Surprises are equal to NA if the last available date is 151 days ago or more (a quarter):

IS$Positv_EPS_Surp[as.numeric(IS$date - IS$Positv_EPS_Surp_DT) > 150] <- NA

# EPS Year over year % change is equal to NA if the last available date is 151 days ago or more (a quarter):

IS$EPS_YoY_Chng[as.numeric(IS$date - IS$EPS_YoY_Chng_DT) > 150] <- NA

# EPS Period over period % change is equal to NA if the last available date is 151 days ago or more (a quarter):

IS$EPS_PoP_Chng[as.numeric(IS$date - IS$EPS_PoP_Chng_DT) > 150] <- NA

# A dummy variable for sector missing data:
# (Only if the sector variable is significant)

IS$Sector_Miss <- as.integer(ifelse(IS$GICS_SECTOR_NAME == "", 1, 0))

# Number of investment signals per day (buy, sell or total)

IS$TotIS <- as.integer(table(IS$date)[as.character(IS$date)])

IS$BuyIS <- table(subset(IS, Strategy == "Buy")$date)[as.character(IS$date)]
IS$BuyIS[is.na(IS$BuyIS) == TRUE] <- 0
IS$BuyIS <- as.integer(IS$BuyIS)

IS$SellIS <- table(subset(IS, Strategy == "Sell")$date)[as.character(IS$date)]
IS$SellIS[is.na(IS$SellIS) == TRUE] <- 0
IS$SellIS <- as.integer(IS$SellIS)

# Dividend yield against the 12 months Average:

IS$DIV_YLD_Relatv_1y <- IS$DIVIDEND_12_MONTH_YIELD / IS$Avg_12m_DIV_YLD

# Dividend yield per Industry, sub-industry, year and stock:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

library(lubridate)

DY <- data.frame()
DT <- ""
RFile1 <- ""
t <- ""

for (i in 1:length(Stocks)){
  RFile1 <- sprintf('%s.RData', Stocks[i])
  load(file = RFile1)
  t <- class(try(aggregate(DIVIDEND_12_MONTH_YIELD ~ year(date), data = DF, mean)))
  if(t == "try-error") next
  DT <- aggregate(DIVIDEND_12_MONTH_YIELD ~ year(date), data = DF, mean)
  DT$Symbol <- Stocks[i]
  DY <- rbind(DY, DT)
  print(i)
  print(Stocks[i])
}

DY$Symbol <- paste0(mgsub(DY$Symbol, pattern=c("-","_"), replacement=c("/"," "))," Equity")
names(DY)[1] <- "Year"

DY <- merge(DY, Static[,c(3, 4, 7)], all.x = TRUE)

Ind <- aggregate(DIVIDEND_12_MONTH_YIELD ~ Year + GICS_INDUSTRY_NAME, data = DY, mean)
names(Ind)[3] <- "DIV_YLD_INDUSTRY"
SubInd <- aggregate(DIVIDEND_12_MONTH_YIELD ~ Year + GICS_SUB_INDUSTRY_NAME, data = DY, mean)
names(SubInd)[3] <- "DIV_YLD_COMPETITORS"

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(DY, Ind, SubInd, file="DIV_YLD_Ind.RData")

YY <- ""
IND <- ""
COMP <- ""
IS$DIV_YLD_INDUSTRY <- as.numeric(0)
IS$DIV_YLD_COMPETITORS <- as.numeric(0)

for (i in 1:nrow(IS)){
  if (IS$GICS_SECTOR_NAME[i] == "") {
    IS$DIV_YLD_INDUSTRY[i] = NA
    IS$DIV_YLD_COMPETITORS[i] = NA
  } else {
    if (IS$MonthN[i] >= 7) {
      YY = IS$Year[i]
      IND = as.character(IS$GICS_INDUSTRY_NAME[i])
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(Ind$DIV_YLD_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]) == 0){
        IS$DIV_YLD_INDUSTRY[i] = NA
      } else {
        IS$DIV_YLD_INDUSTRY[i] = Ind$DIV_YLD_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]
      }
      if (length(SubInd$DIV_YLD_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0){
        IS$DIV_YLD_COMPETITORS[i] = NA
      } else {
        IS$DIV_YLD_COMPETITORS[i] = SubInd$DIV_YLD_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    } else {
      YY = IS$Year[i] - 1
      IND = as.character(IS$GICS_INDUSTRY_NAME[i])
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(Ind$DIV_YLD_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]) == 0) {
        IS$DIV_YLD_INDUSTRY[i] = NA
      } else {
        IS$DIV_YLD_INDUSTRY[i] = Ind$DIV_YLD_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]
      }
      if (length(SubInd$DIV_YLD_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0) {
        IS$DIV_YLD_COMPETITORS[i] = NA
      } else {
        IS$DIV_YLD_COMPETITORS[i] = SubInd$DIV_YLD_COMPETITORS[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    }
  }
}

# Dividend yield against the sector Average:

IS$DIV_YLD_Relatv_IND <- ifelse(IS$DIV_YLD_INDUSTRY == 0, 0, IS$DIVIDEND_12_MONTH_YIELD / IS$DIV_YLD_INDUSTRY)

# Dividend yield against competitors Average:

IS$DIV_YLD_Relatv_COMP <- ifelse(IS$DIV_YLD_COMPETITORS == 0, 0, IS$DIVIDEND_12_MONTH_YIELD / IS$DIV_YLD_COMPETITORS)

# Earnings announcement during the momentum trend - Changes in fundamental data:

IS$LATEST_ANNOUNCEMENT_DT[is.na(IS$LATEST_ANNOUNCEMENT_DT) == TRUE] <- as.Date("1985-12-31")
IS$LATEST_ANNOUNCEMENT_DT2[is.na(IS$LATEST_ANNOUNCEMENT_DT2) == TRUE] <- as.Date("1985-12-31")

IS$Days_Past_Earn <- businessDaysBetween("UnitedStates/NYSE", IS$LATEST_ANNOUNCEMENT_DT, IS$date, includeFirst = TRUE, includeLast = TRUE)
IS$Days_Past_Earn2 <- businessDaysBetween("UnitedStates/NYSE", IS$LATEST_ANNOUNCEMENT_DT2, IS$date, includeFirst = TRUE, includeLast = TRUE)

IS$Event_Driven_Earn <- as.integer(IS$Days_Past_Earn <= (IS$DT + 1))
IS$Event_Driven_Earn2 <- as.integer(IS$Days_Past_Earn2 <= (IS$DT + 1))

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
load(file="Earnings_Dates_Diff.RData")

Eq_Diff <- row.names(subset(Diff, Difference < -1))
Eq_Diff <- mgsub(Eq_Diff, pattern=c("-","_"), replacement=c("/"," "))

IS$Event_Driven_Earn[IS$Symbol %in% Eq_Diff & IS$Event_Driven_Earn == 0] <- IS$Event_Driven_Earn2[IS$Symbol %in% Eq_Diff & IS$Event_Driven_Earn == 0]
IS$Event_Driven_Earn[IS$Symbol == "2761917Q US"] <- IS$Event_Driven_Earn2[IS$Symbol == "2761917Q US"]

# Market-to-Book ratio per sub-industry, year and stock:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

library(lubridate)

MCVB <- data.frame()
DT <- ""
RFile1 <- ""
t <- ""

for (i in 1:length(Stocks)){
  RFile1 <- sprintf('%s.RData', Stocks[i])
  load(file = RFile1)
  t <- class(try(aggregate(MARKET_CAPITALIZATION_TO_BV ~ year(date), data = DF, mean)))
  if(t == "try-error") next
  DT <- aggregate(MARKET_CAPITALIZATION_TO_BV ~ year(date), data = DF, mean)
  DT$Symbol <- Stocks[i]
  MCVB <- rbind(MCVB, DT)
  print(i)
  print(Stocks[i])
}

MCVB$Symbol <- paste0(mgsub(MCVB$Symbol, pattern=c("-","_"), replacement=c("/"," "))," Equity")
names(MCVB)[1] <- "Year"

MCVB <- merge(MCVB, Static[,c(4, 7)], all.x = TRUE)

SubInd <- aggregate(MARKET_CAPITALIZATION_TO_BV ~ Year + GICS_SUB_INDUSTRY_NAME, data = MCVB, mean)
names(SubInd)[3] <- "MARKETCAP_TO_BV_COMP"

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(MCVB, SubInd, file="MARKETCAP_TO_BV_Ind.RData")

YY <- ""
COMP <- ""
IS$MARKETCAP_TO_BV_COMP <- as.numeric(0)

for (i in 1:nrow(IS)){
  if (IS$GICS_SECTOR_NAME[i] == "") {
    IS$MARKETCAP_TO_BV_COMP[i] = NA
  } else {
    if (IS$MonthN[i] >= 7) {
      YY = IS$Year[i]
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(SubInd$MARKETCAP_TO_BV_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0){
        IS$MARKETCAP_TO_BV_COMP[i] = NA
      } else {
        IS$MARKETCAP_TO_BV_COMP[i] = SubInd$MARKETCAP_TO_BV_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    } else {
      YY = IS$Year[i] - 1
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(SubInd$MARKETCAP_TO_BV_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0) {
        IS$MARKETCAP_TO_BV_COMP[i] = NA
      } else {
        IS$MARKETCAP_TO_BV_COMP[i] = SubInd$MARKETCAP_TO_BV_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    }
  }
}

# Market-to-Book ratio against competitors Average:

IS$MARKETCAP_TO_BV_Relatv_COMP <- ifelse(IS$MARKETCAP_TO_BV_COMP == 0, 0, IS$MARKET_CAPITALIZATION_TO_BV / IS$MARKETCAP_TO_BV_COMP)

# Classification of  companies size according to their number of employees (OECD):

IS$COMPANY_SIZE <- as.factor(ifelse(IS$NUM_OF_EMPLOYEES >= 250, "Large",
                                ifelse(IS$NUM_OF_EMPLOYEES <= 49, "Small", "Medium")))

IS$COMPANY_SIZE <- factor(IS$COMPANY_SIZE, c("Small", "Medium", "Large"))

# Natural log of total number of employees:

hist(IS$NUM_OF_EMPLOYEES, xlim = c(0,500000), breaks = 100)

IS$LOG_NUM_EMPLOYEES <- log(IS$NUM_OF_EMPLOYEES)

# Revenue SurpriseS are equal to NA if the last available date is 151 days ago or more (a quarter):

IS$Revenue_Surprise[as.numeric(IS$date - IS$REV_SURP_DT) > 150] <- NA

# Positive revenue SurpriseS are equal to NA if the last available date is 151 days ago or more (a quarter):

IS$Positv_Rev_Surp[as.numeric(IS$date - IS$REV_SURP_DT) > 150] <- NA 

# Revenue Year over year % change is equal to NA if the last available date is 151 days ago or more (a quarter):

IS$Rev_YoY_Chng[as.numeric(IS$date - IS$Rev_YoY_Chng_DT) > 150] <- NA

# Revenue Period over period % change is equal to NA if the last available date is 151 days ago or more (a quarter):

IS$Rev_PoP_Chng[as.numeric(IS$date - IS$Rev_PoP_Chng_DT) > 150] <- NA

# Debt-To-Equity Ratio per Industry, sub-industry, year and stock:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data')

library(lubridate)

DE <- data.frame()
DT <- ""
QQ <- ""
ADF <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings Dates/'
t <- ""

for (i in 1:length(Stocks)){
  RFile1 <- sprintf('%s_Q.RData', Stocks[i])
  load(file = RFile1)
  if (dim(DF)[1] == 0) next
  QQ <- DF
  RFile2 <- paste0(Dir2, sprintf('%s_Q_EDate.RData', Stocks[i]))
  load(file = RFile2)
  ADF <- merge(QQ, DF, all.x = TRUE)
  ADF <- ADF[order(ADF$date, decreasing = TRUE),]
  names(ADF)[1] <- "Quarter_Ended"
  names(ADF)[7] <- "date"
  ADF$date <- as.Date(ifelse(is.na(ADF$date) == TRUE, ADF$Quarter_Ended, ADF$date), origin = lubridate::origin)
  t <- class(try(aggregate(TOT_DEBT_TO_TOT_EQY ~ year(date), data = ADF, mean)))
  if(t == "try-error") next
  DT <- aggregate(TOT_DEBT_TO_TOT_EQY ~ year(date), data = ADF, mean)
  DT$Symbol <- Stocks[i]
  DE <- rbind(DE, DT)
  print(i)
  print(Stocks[i])
}

DE$Symbol <- paste0(mgsub(DE$Symbol, pattern=c("-","_"), replacement=c("/"," "))," Equity")
names(DE)[1] <- "Year"

DE <- merge(DE, Static[,c(3, 4, 7)], all.x = TRUE)

Ind <- aggregate(TOT_DEBT_TO_TOT_EQY ~ Year + GICS_INDUSTRY_NAME, data = DE, mean)
names(Ind)[3] <- "DEBT_TO_EQY_INDUSTRY"
SubInd <- aggregate(TOT_DEBT_TO_TOT_EQY ~ Year + GICS_SUB_INDUSTRY_NAME, data = DE, mean)
names(SubInd)[3] <- "DEBT_TO_EQY_COMP"

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(DE, Ind, SubInd, file="DEBT_TO_EQY_Ind.RData")

YY <- ""
IND <- ""
COMP <- ""
IS$DEBT_TO_EQY_INDUSTRY <- as.numeric(0)
IS$DEBT_TO_EQY_COMP <- as.numeric(0)

for (i in 1:nrow(IS)){
  if (IS$GICS_SECTOR_NAME[i] == "") {
    IS$DEBT_TO_EQY_INDUSTRY[i] = NA
    IS$DEBT_TO_EQY_COMP[i] = NA
  } else {
    if (IS$MonthN[i] >= 7) {
      YY = IS$Year[i]
      IND = as.character(IS$GICS_INDUSTRY_NAME[i])
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(Ind$DEBT_TO_EQY_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]) == 0){
        IS$DEBT_TO_EQY_INDUSTRY[i] = NA
      } else {
        IS$DEBT_TO_EQY_INDUSTRY[i] = Ind$DEBT_TO_EQY_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]
      }
      if (length(SubInd$DEBT_TO_EQY_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0){
        IS$DEBT_TO_EQY_COMP[i] = NA
      } else {
        IS$DEBT_TO_EQY_COMP[i] = SubInd$DEBT_TO_EQY_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    } else {
      YY = IS$Year[i] - 1
      IND = as.character(IS$GICS_INDUSTRY_NAME[i])
      COMP = as.character(IS$GICS_SUB_INDUSTRY_NAME[i])
      if (length(Ind$DEBT_TO_EQY_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]) == 0) {
        IS$DEBT_TO_EQY_INDUSTRY[i] = NA
      } else {
        IS$DEBT_TO_EQY_INDUSTRY[i] = Ind$DEBT_TO_EQY_INDUSTRY[which(Ind$Year == YY & Ind$GICS_INDUSTRY_NAME == IND)]
      }
      if (length(SubInd$DEBT_TO_EQY_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]) == 0) {
        IS$DEBT_TO_EQY_COMP[i] = NA
      } else {
        IS$DEBT_TO_EQY_COMP[i] = SubInd$DEBT_TO_EQY_COMP[which(SubInd$Year == YY & SubInd$GICS_SUB_INDUSTRY_NAME == COMP)]
      }
    }
  }
}

# Debt-To-Equity Ratio against the sector Average:

IS$DEBT_TO_EQY_Relatv_IND <- ifelse(IS$DEBT_TO_EQY_INDUSTRY == 0, 0, IS$TOT_DEBT_TO_TOT_EQY / IS$DEBT_TO_EQY_INDUSTRY)

# Debt-To-Equity Ratio against competitors Average:

IS$DEBT_TO_EQY_Relatv_COMP <- ifelse(IS$DEBT_TO_EQY_COMP == 0, 0, IS$TOT_DEBT_TO_TOT_EQY / IS$DEBT_TO_EQY_COMP)

# Ex-dividend Date during the next 10 business days:

IS$Ex_Div[is.na(IS$Ex_Div) == TRUE] <- as.Date("2020-01-03")

IS$Days_Next_Ex_DIV <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Ex_Div, includeFirst = FALSE, includeLast = TRUE)

IS$CorrectPer_DIV_EX = as.integer(IS$Days_Next_Ex_DIV <= 10 & IS$Days_Next_Ex_DIV > 0)

# Value stocks / growth stocks Classification:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Russell Value - Growth')

VG <- data.frame()
YY <- c(1995:2019)
RFile <- ""

for (i in 1:length(YY)){
  RFile <- sprintf('Value_%s.RData', YY[i])
  load(file = RFile)
  Index$Year <- YY[i]
  Index$Val_Grow <- "Value"
  VG <- rbind(VG, Index)
  print(YY[i])
}

for (i in 1:length(YY)){
  RFile <- sprintf('Growth_%s.RData', YY[i])
  load(file = RFile)
  Index$Year <- YY[i]
  Index$Val_Grow <- "Growth"
  VG <- rbind(VG, Index)
  print(YY[i])
}

IS <- merge(IS, VG, by = c("Symbol", "Year"), all.x = TRUE)
IS$Val_Grow <- as.factor(IS$Val_Grow)

# Momentum pattern slope
  
IS$Moment_Slope <- IS$MomentumCh / IS$DM
  
# Momentum trend slope
  
IS$Moment_Slope <- IS$TCh / IS$DT

# Days with volume up as factor variable: 

IS$DVolUp_F <- as.factor(IS$DVolUp)

# Pattern of days with volume up:

IS$DVolUp_Pattern <- as.factor(paste(IS$VolUp_2D, IS$VolUp_3D, IS$VolUp_4D, IS$VolUp_5D, sep = ""))

# Difference between de Max. and the last ratio of the volume against the 30 days average volume

IS$Diff_Max_Last_Vol_Relatv_30D <- IS$Max_Mom_Vol_Relatv_30D - IS$Vol_Relatv_30D

# Difference between de Max. and the momentum pattern average of the volume against the 30 days average volume

IS$Diff_Max_Avg_Vol_Relatv_30D <- IS$Max_Mom_Vol_Relatv_30D - IS$Mom_Vol_Relatv_30D

# Price momentum indicator - 5-days moving average against a 30-days moving average

IS$Price_Mom_5D_30D <- IS$Avg_Price_5D / IS$Avg_Price_30D

# Price momentum indicator - 5-days moving average against a 50-days moving average

IS$Price_Mom_5D_50D <- IS$Avg_Price_5D / IS$Avg_Price_50D

# 50-days moving average against a 200-days moving average

IS$Price_50D_Relatv_200D <- IS$Avg_Price_50D / IS$Avg_Price_200D

# Time since the event-driven

IS$Merg_Acq_Time <- IS$Event_Driven_Merg_Acq * IS$Days_Past_Merg_Acq
IS$Inv_JV_Time <- IS$Event_Driven_Inv_JV * IS$Days_Past_Inv_JV
IS$Earn_Time <- IS$Event_Driven_Earn * IS$Days_Past_Earn
IS$Earn_Time[IS$Symbol %in% Eq_Diff & IS$Event_Driven_Earn2 == 1] <- IS$Event_Driven_Earn[IS$Symbol %in% Eq_Diff & IS$Event_Driven_Earn2 == 1] * IS$Days_Past_Earn2[IS$Symbol %in% Eq_Diff & IS$Event_Driven_Earn2 == 1]
IS$Earn_Time[IS$Symbol == "2761917Q US"] <- IS$Event_Driven_Earn[IS$Symbol == "2761917Q US"] * IS$Days_Past_Earn2[IS$Symbol == "2761917Q US"]

# Corporate actions during the correction period:

IS$Next_Declared_Date[is.na(IS$Next_Declared_Date) == TRUE] <- as.Date("2020-01-03")

IS$Days_Next_Corp_Act <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Next_Declared_Date, includeFirst = FALSE, includeLast = TRUE)

IS$CorrectPer_Corp_Act <- as.integer(IS$Days_Next_Corp_Act <= 10 & IS$Days_Next_Corp_Act > 0)

# Spinoff and buybacks during the correction period:

IS$Next_Spin_Buy_Date[is.na(IS$Next_Spin_Buy_Date) == TRUE] <- as.Date("2020-01-03")

IS$Days_Next_Spin_Buy <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Next_Spin_Buy_Date, includeFirst = FALSE, includeLast = TRUE)

IS$CorrectPer_Spin_Buy <- as.integer(IS$Days_Next_Spin_Buy <= 10 & IS$Days_Next_Spin_Buy > 0)

# Merger or acquisitions during the correction period:

IS$Next_Merg_Acq_Date[is.na(IS$Next_Merg_Acq_Date) == TRUE] <- as.Date("2020-01-03")

IS$Days_Next_Merg_Acq <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Next_Merg_Acq_Date, includeFirst = FALSE, includeLast = TRUE)

IS$CorrectPer_Merg_Acq <- as.integer(IS$Days_Next_Merg_Acq <= 10 & IS$Days_Next_Merg_Acq > 0)

# Investments and joint ventures during the correction period:

IS$Next_Inv_JV_Date[is.na(IS$Next_Inv_JV_Date) == TRUE] <- as.Date("2020-01-03")

IS$Days_Next_Inv_JV <- businessDaysBetween("UnitedStates/NYSE", IS$date, IS$Next_Inv_JV_Date, includeFirst = FALSE, includeLast = TRUE)

IS$CorrectPer_Inv_JV <- as.integer(IS$Days_Next_Inv_JV <= 10 & IS$Days_Next_Inv_JV > 0)

# Return-risk ratio 12 months:

IS$Return_risk_12m <- IS$Avg_12m_Pr_Rt / IS$Risk_12m

# Return-risk ratio 5 years:

IS$Return_risk_5y <- IS$Avg_5y_Pr_Rt / IS$Risk_5y

# Momentum Return-risk ratio:

IS$Mom_Return_risk <- IS$Mom_Avg_Rt / IS$Mom_Risk

# 30 days volatility against the 12 months volatility:

IS$Volat_30d_Relatv_12m <- IS$Volat_30d / IS$Risk_12m

# 30 days volatility against the 5 year volatility:

IS$Volat_30d_Relatv_5y <- IS$Volat_30d / IS$Risk_5y

# MSCI Cyclical and Defensive Sectors:

IS$MSCI_Cyclical <- as.integer(ifelse(IS$GICS_SECTOR_NAME == "", NA, ifelse(IS$GICS_SECTOR_NAME == "Real Estate" | IS$GICS_SECTOR_NAME == "Materials" | IS$GICS_SECTOR_NAME == "Information Technology" |
                           IS$GICS_SECTOR_NAME == "Industrials" | IS$GICS_SECTOR_NAME == "Financials" | IS$GICS_SECTOR_NAME == "Consumer Discretionary",
                            1, 0)))

# Morningstar Cyclical, Defensive and Sensitive Sectors:

IS$Morningstar_Sector_Class <- as.factor(ifelse(IS$GICS_SECTOR_NAME == "", NA, ifelse(IS$GICS_SECTOR_NAME == "Real Estate" | IS$GICS_SECTOR_NAME == "Materials" | IS$GICS_SECTOR_NAME == "Consumer Discretionary" |
                                      IS$GICS_SECTOR_NAME == "Financials", "Cyclical", ifelse(IS$GICS_SECTOR_NAME == "Information Technology" | IS$GICS_SECTOR_NAME == "Industrials" |
                                                                                              IS$GICS_SECTOR_NAME == "Communication Services" | IS$GICS_SECTOR_NAME == "Energy",
                                                                                              "Sensitive", "Defensive"))))

# Days with greater return as factor variable: 

IS$D_Rt_Up_F <- as.factor(IS$D_Rt_Up)

# Pattern in the % change of each day close price:

IS$Rt_Up_Pattern <- as.factor(paste(IS$Rt_Up_2D, IS$Rt_Up_3D, IS$Rt_Up_4D, IS$Rt_Up_5D, sep = ""))

# Max % price vs Momentum Average return:

IS$Max_Rt_Relatv_Avg <- IS$Max_Rt_Mom / IS$Mom_Avg_Rt

# Max % price vs Momentum Median return:

IS$Max_Rt_Relatv_Median <- IS$Max_Rt_Mom / IS$Mom_Median_Rt

# Days with Total Option Volume up as factor variable: 

IS$D_TOT_OPT_VOL_Up_F <- as.factor(IS$D_TOT_OPT_VOL_Up)

# Days with Call Option Volume up as factor variable: 

IS$D_CALL_VOL_Up_F <- as.factor(IS$D_CALL_VOL_Up)

# Days with Put Option Volume up as factor variable: 

IS$D_PUT_VOL_Up_F <- as.factor(IS$D_PUT_VOL_Up)

# Days with high total Option Volume up as factor variable: 

IS$D_HIGH_TOT_OPT_VOL_F <- as.factor(IS$D_HIGH_TOT_OPT_VOL)

# Days with high call Option Volume up as factor variable: 

IS$D_HIGH_CALL_VOL_F <- as.factor(IS$D_HIGH_CALL_VOL)

# Days with high Put Option Volume up as factor variable: 

IS$D_HIGH_PUT_VOL_F <- as.factor(IS$D_HIGH_PUT_VOL)

# Liquidity: An average volume over 1 million shares traded

IS$Vol_Liquid <- as.integer(IS$AvgVol_6m >= 1000000)

hist(subset(IS, Vol_Liquid == 1)$date, freq = TRUE, breaks = 30)
hist(subset(IS, Vol_Liquid == 0)$date, freq = TRUE, breaks = 30)
table(IS$Vol_Liquid)

# Liquidity: A lower average monthly Bid-Ask Spread
# It will be used only to filter

IS$Liquid <- as.integer(IS$Avg_BID_ASK  < 1)
table(IS$Liquid)

IS$High_Liquid <- as.integer(IS$Avg_BID_ASK  < 0.5)
table(IS$High_Liquid)

# Removing variables:

IS$VolUp_2D <- NULL
IS$VolUp_3D <- NULL
IS$VolUp_4D <- NULL
IS$VolUp_5D <- NULL
IS$MaxPrice10D <- NULL
IS$MinPrice10D <- NULL
IS$MaxPrice15D <- NULL
IS$MinPrice15D <- NULL
IS$Avg_12m_DIV_YLD <- NULL
IS$Risk_5y <- NULL
IS$Avg_12m_Pr_Rt <- NULL
IS$Avg_5y_Pr_Rt <- NULL
IS$Rt_Up_2D <- NULL
IS$Rt_Up_3D <- NULL
IS$Rt_Up_4D <- NULL
IS$Rt_Up_5D <- NULL
IS$Avg_Price_5D <- NULL
IS$Avg_Price_30D <- NULL
IS$Avg_Price_50D <- NULL
IS$Avg_Price_200D <- NULL
IS$Avg_12m_PE <- NULL
IS$SHORT_INT <- NULL
IS$SHORT_INT_DT <- NULL
IS$SI_PERCENT_EQUITY_FLOAT_DT <- NULL
IS$SHORT_INT_RATIO_DT <- NULL
IS$SHORT_INT_Chng_DT <- NULL
IS$NUM_OF_EMPLOYEES <- NULL
IS$Ex_Div <- NULL
IS$Merg_Acq_Date <- NULL
IS$Next_Merg_Acq_Date <- NULL
IS$Inv_JV_Date <- NULL
IS$Next_Inv_JV_Date <- NULL
IS$Next_Earnings <- NULL
IS$Next_Earnings_Time <- NULL
IS$Ex_Date <- NULL
IS$Declared_Date <- NULL
IS$Days_Last_Corp_Act <- NULL
IS$Spin_Buy_Date <- NULL
IS$Days_Last_Spin_Buy <- NULL
IS$Days_Past_Merg_Acq <- NULL
IS$Corr30d_Gold <- NULL
IS$Corr30d_Oil <- NULL
IS$Corr30d_Gas <- NULL
IS$Corr30d_Copper <- NULL
IS$Corr30d_Silver <- NULL
IS$Corr150d_Gold <- NULL
IS$Corr150d_Oil <- NULL
IS$Corr150d_Gas <- NULL
IS$Corr150d_Copper <- NULL
IS$Corr150d_Silver <- NULL
IS$Days_Past_Inv_JV <- NULL
IS$REV_SURP_DT <- NULL
IS$EPS_Surprise_DT <- NULL
IS$Positv_EPS_Surp_DT <- NULL
IS$EPS_YoY_Chng_DT <- NULL
IS$EPS_PoP_Chng_DT <- NULL
IS$Rev_YoY_Chng_DT <- NULL
IS$Rev_PoP_Chng_DT <- NULL
IS$MARKET_CAPITALIZATION_TO_BV <- NULL
IS$DIVIDEND_12_MONTH_YIELD <- NULL
IS$Next_Declared_Date <- NULL
IS$Next_Spin_Buy_Date <- NULL
IS$LATEST_ANNOUNCEMENT_DT <- NULL
IS$LATEST_ANNOUNCEMENT_DT2 <- NULL
IS$PE_INDUSTRY <- NULL
IS$PE_COMPETITORS <- NULL
IS$DIV_YLD_INDUSTRY <- NULL
IS$DIV_YLD_COMPETITORS <- NULL
IS$Days_Past_Earn <- NULL
IS$Days_Past_Earn2 <- NULL
IS$Event_Driven_Earn2 <- NULL
IS$MARKETCAP_TO_BV_COMP <- NULL
IS$DEBT_TO_EQY_INDUSTRY <- NULL
IS$DEBT_TO_EQY_COMP <- NULL
IS$Dividend_Type <- NULL
IS$Spin_Buy_Type <- NULL

IS$BID_ASK <- NULL
IS$Date_MinPrice10D <- NULL
IS$Date_MaxPrice10D <- NULL
IS$Date_MaxPrice15D <- NULL
IS$Date_MinPrice15D <- NULL
IS$TOT_OPEN_INT_SD_Relatv_30D <- NULL
IS$CALL_OPEN_INT_SD_Relatv_30D <- NULL
IS$PUT_OPEN_INT_SD_Relatv_30D <- NULL
IS$PUT_CALL_OPEN_INT_RATIO_SD_Relatv_30D <- NULL

# 3684 Obs

# Saving IS

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(IS, file="Stock-Investment-Signals.RData")

# Market data files

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Market Data')
load(file="Daily-market-Data.RData")
load(file="Market-Data.RData")

# Merging with S&P 500

PD <- merge(IS, SP500, all.x = TRUE)

# S&P 500 Change during the momentum pattern

PD$ChngSP500 <- ifelse(PD$DM == 13, PD$ChngSP500D13,
                        ifelse(PD$DM == 12, PD$ChngSP500D12,
                        ifelse(PD$DM == 11, PD$ChngSP500D11,
                        ifelse(PD$DM == 10, PD$ChngSP500D10, 
                        ifelse(PD$DM == 9, PD$ChngSP500D9,
                        ifelse(PD$DM == 8, PD$ChngSP500D8,
                        ifelse(PD$DM == 7, PD$ChngSP500D7,
                        ifelse(PD$DM == 6, PD$ChngSP500D6, PD$ChngSP500D5))))))))

PD$ChngSP500D13 <- NULL
PD$ChngSP500D12 <- NULL
PD$ChngSP500D11 <- NULL
PD$ChngSP500D10 <- NULL
PD$ChngSP500D9 <- NULL
PD$ChngSP500D8 <- NULL
PD$ChngSP500D7 <- NULL
PD$ChngSP500D6 <- NULL

# Merging with VIX Index

PD <- merge(PD, VIX, all.x = TRUE)

# VIX Change during the momentum pattern

PD$VIX_LAST <- ifelse(PD$DM == 13, PD$VIX_LASTD13, 
                     ifelse(PD$DM == 12, PD$VIX_LASTD12, 
                      ifelse(PD$DM == 11, PD$VIX_LASTD11, 
                      ifelse(PD$DM == 10, PD$VIX_LASTD10, 
                      ifelse(PD$DM == 9, PD$VIX_LASTD9,
                      ifelse(PD$DM == 8, PD$VIX_LASTD8,
                      ifelse(PD$DM == 7, PD$VIX_LASTD7,
                      ifelse(PD$DM == 6, PD$VIX_LASTD6, PD$VIX_LASTD5))))))))

PD$VIX_LASTD13 <- NULL
PD$VIX_LASTD12 <- NULL
PD$VIX_LASTD11 <- NULL
PD$VIX_LASTD10 <- NULL
PD$VIX_LASTD9 <- NULL
PD$VIX_LASTD8 <- NULL
PD$VIX_LASTD7 <- NULL
PD$VIX_LASTD6 <- NULL

# VIX Change during the momentum pattern

PD$ChngVIX <- (PD$VIX/PD$VIX_LAST)-1

# VIX momentum indicator - 5-days moving average against a 30-days moving average

PD$VIX_Mom_5D_30D <- PD$Avg_VIX_5D / PD$Avg_VIX_30D

# VIX momentum indicator - 5-days moving average against a 50-days moving average

PD$VIX_Mom_5D_50D <- PD$Avg_VIX_5D / PD$Avg_VIX_50D

# 50-days moving average against a 200-days moving average

PD$VIX_50D_relatv_200D <- PD$Avg_VIX_50D / PD$Avg_VIX_200D

# VIX classification:

PD$VIX_Class <- as.factor(ifelse(PD$VIX >= 30, "VIX>30", ifelse(PD$VIX <=20, "VIX<20", "20<VIX<30")))

# VIX change between classifications:

PD$VIX_Class_Chng <- as.factor(ifelse(PD$VIX_LAST <=20 & PD$VIX <= 20, "Btwn Low zone", 
                                      ifelse(PD$VIX_LAST >= 30 & PD$VIX >= 30, "Btwn High zone",
                                      ifelse(PD$VIX_LAST <= 20 & PD$VIX >= 30, "Low zone - High zone",
                                      ifelse(PD$VIX_LAST >= 30 & PD$VIX <= 20, "High zone - Low zone",
                                      ifelse(PD$VIX_LAST < 30 & PD$VIX >= 30, "Middle zone - High zone",
                                      ifelse(PD$VIX_LAST >= 30 & PD$VIX < 30, "High zone - Middle zone",
                                      ifelse(PD$VIX_LAST < 30 & PD$VIX <= 20, "Middle zone - Low zone",
                                      ifelse(PD$VIX_LAST <= 20 & PD$VIX < 30, "Low zone - Middle zone", "Btwn Middle zone")))))))))

PD$VIX_LAST <- NULL
PD$Avg_VIX_5D <- NULL
PD$Avg_VIX_30D <- NULL
PD$Avg_VIX_50D <- NULL
PD$Avg_VIX_200D <- NULL

# Merging with Citigroup Economic Surprise Index

names(CitiIndex)[2] <- "Citi"
PD <- merge(PD, CitiIndex, all.x = TRUE)

# Merging with Equity Put/Call Ratio

PD <- merge(PD, PutCallRatio, all.x = TRUE)

# Equity Put/Call Ratio Change during the momentum pattern

PD$ChngEq_PutCall <- ifelse(PD$DM == 13, PD$Chng_Eq_PutCalD13,
                       ifelse(PD$DM == 12, PD$Chng_Eq_PutCalD12,
                        ifelse(PD$DM == 11, PD$Chng_Eq_PutCalD11,
                        ifelse(PD$DM == 10, PD$Chng_Eq_PutCalD10, 
                        ifelse(PD$DM == 9, PD$Chng_Eq_PutCalD9,
                        ifelse(PD$DM == 8, PD$Chng_Eq_PutCalD8,
                        ifelse(PD$DM == 7, PD$Chng_Eq_PutCalD7,
                        ifelse(PD$DM == 6, PD$Chng_Eq_PutCalD6, PD$Chng_Eq_PutCalD5))))))))

PD$Chng_Eq_PutCalD13 <- NULL
PD$Chng_Eq_PutCalD12 <- NULL
PD$Chng_Eq_PutCalD11 <- NULL
PD$Chng_Eq_PutCalD10 <- NULL
PD$Chng_Eq_PutCalD9 <- NULL
PD$Chng_Eq_PutCalD8 <- NULL
PD$Chng_Eq_PutCalD7 <- NULL
PD$Chng_Eq_PutCalD6 <- NULL

# Merging with US Investor sentiment

PD <- merge(PD, Sent, all.x = TRUE)

  # market sentiment indicator

  PD$Market_Sent <- as.factor(ifelse(PD$Bull_Bear_R <= 0.5, "Pessimism", ifelse(PD$Bull_Bear_R >= 1.5 , "Optimism", "")))

# Merging with U.S. Recession Indicator

PD <- merge(PD, USRecession, all.x = TRUE)

# Merging with Nonfarm payrolls

PD <- merge(PD, NonfarmPayRelease, all.x = TRUE)

  # Nonfarm payrolls release during the momentum trend:

  PD$Nonfarm_Past_DT[is.na(PD$Nonfarm_Past_DT) == TRUE] <- as.Date("1985-12-31")

  PD$Days_Past_Nonfarm <- businessDaysBetween("UnitedStates/NYSE", PD$Nonfarm_Past_DT, PD$date, includeFirst = TRUE, includeLast = TRUE)

  PD$Past_Nonfarm <- as.integer(PD$Days_Past_Nonfarm <= (PD$DT + 1))

  # Nonfarm payrolls release during the next 10 business days:

  PD$Nonfarm_Next_DT[is.na(PD$Nonfarm_Next_DT) == TRUE] <- as.Date("2020-01-03")

  PD$Days_Next_Nonfarm <- businessDaysBetween("UnitedStates/NYSE", PD$date, PD$Nonfarm_Next_DT, includeFirst = FALSE, includeLast = TRUE)

  PD$Next_Nonfarm = as.integer(PD$Days_Next_Nonfarm <= 10 & PD$Days_Next_Nonfarm > 0)

PD$Nonfarm_Past_DT <- NULL
PD$Days_Past_Nonfarm <- NULL
PD$Nonfarm_Next_DT <- NULL

# Removing duplicate observation in Housing starts

HouseStartRelease <- unique(HouseStartRelease)
  
# Merging with Housing starts
  
  PD <- merge(PD, HouseStartRelease, all.x = TRUE)
  
  # Housing starts release during the momentum trend:
  
  PD$HouseStart_Past_DT[is.na(PD$HouseStart_Past_DT) == TRUE] <- as.Date("1985-12-31")
  
  PD$Days_Past_HouseStart <- businessDaysBetween("UnitedStates/NYSE", PD$HouseStart_Past_DT, PD$date, includeFirst = TRUE, includeLast = TRUE)
  
  PD$Past_HouseStart <- as.integer(PD$Days_Past_HouseStart <= (PD$DT + 1))
  
  # Housing starts release during the next 10 business days:
  
  PD$HouseStart_Next_DT[is.na(PD$HouseStart_Next_DT) == TRUE] <- as.Date("2020-01-03")
  
  PD$Days_Next_HouseStart <- businessDaysBetween("UnitedStates/NYSE", PD$date, PD$HouseStart_Next_DT, includeFirst = FALSE, includeLast = TRUE)
  
  PD$Next_HouseStart = as.integer(PD$Days_Next_HouseStart <= 10 & PD$Days_Next_HouseStart > 0)

PD$HouseStart_Past_DT <- NULL
PD$Days_Past_HouseStart <- NULL
PD$HouseStart_Next_DT <- NULL

# Merging with CPI
  
  PD <- merge(PD, CPIRelease, all.x = TRUE)
  
  # CPI release during the momentum trend:
  
  PD$CPI_Past_DT[is.na(PD$CPI_Past_DT) == TRUE] <- as.Date("1985-12-31")
  
  PD$Days_Past_CPI <- businessDaysBetween("UnitedStates/NYSE", PD$CPI_Past_DT, PD$date, includeFirst = TRUE, includeLast = TRUE)
  
  PD$Past_CPI <- as.integer(PD$Days_Past_CPI <= (PD$DT + 1))
  
  # CPI release during the next 10 business days:
  
  PD$CPI_Next_DT[is.na(PD$CPI_Next_DT) == TRUE] <- as.Date("2020-01-03")
  
  PD$Days_Next_CPI <- businessDaysBetween("UnitedStates/NYSE", PD$date, PD$CPI_Next_DT, includeFirst = FALSE, includeLast = TRUE)
  
  PD$Next_CPI = as.integer(PD$Days_Next_CPI <= 10 & PD$Days_Next_CPI > 0)
  
PD$CPI_Past_DT <- NULL
PD$Days_Past_CPI <- NULL
PD$CPI_Next_DT <- NULL

# Merging with FOMC rate
  
  PD <- merge(PD, FOMCRateRelease, all.x = TRUE)
  
  # FOMC rate release during the momentum trend:
  
  PD$FED_Rate_Past_DT[is.na(PD$FED_Rate_Past_DT) == TRUE] <- as.Date("1985-12-31")
  
  PD$Days_Past_FED_Rate <- businessDaysBetween("UnitedStates/NYSE", PD$FED_Rate_Past_DT, PD$date, includeFirst = TRUE, includeLast = TRUE)
  
  PD$Past_FED_Rate <- as.integer(PD$Days_Past_FED_Rate <= (PD$DT + 1))
  
  # FOMC rate release during the next 10 business days:
  
  PD$FED_Rate_Next_DT[is.na(PD$FED_Rate_Next_DT) == TRUE] <- as.Date("2020-01-03")
  
  PD$Days_Next_FED_Rate <- businessDaysBetween("UnitedStates/NYSE", PD$date, PD$FED_Rate_Next_DT, includeFirst = FALSE, includeLast = TRUE)
  
  PD$Next_FED_Rate = as.integer(PD$Days_Next_FED_Rate <= 10 & PD$Days_Next_FED_Rate > 0)
  
PD$FED_Rate_Past_DT <- NULL
PD$Days_Past_FED_Rate <- NULL
PD$FED_Rate_Next_DT <- NULL

# Merging with PMI
  
  PD <- merge(PD, PMI, all.x = TRUE)
  
  # PMI release during the momentum trend:
  
  PD$PMI_Past_DT[is.na(PD$PMI_Past_DT) == TRUE] <- as.Date("1985-12-31")
  
  PD$Days_Past_PMI <- businessDaysBetween("UnitedStates/NYSE", PD$PMI_Past_DT, PD$date, includeFirst = TRUE, includeLast = TRUE)
  
  PD$Past_PMI <- as.integer(PD$Days_Past_PMI <= (PD$DT + 1))
  
  # PMI release during the next 10 business days:
  
  PD$PMI_Next_DT[is.na(PD$PMI_Next_DT) == TRUE] <- as.Date("2020-01-03")
  
  PD$Days_Next_PMI <- businessDaysBetween("UnitedStates/NYSE", PD$date, PD$PMI_Next_DT, includeFirst = FALSE, includeLast = TRUE)
  
  PD$Next_PMI = as.integer(PD$Days_Next_PMI <= 10 & PD$Days_Next_PMI > 0)

PD$PMI_Past_DT <- NULL
PD$Days_Past_PMI <- NULL
PD$PMI_Next_DT <- NULL

# Merging with NY Fed prob of recession in US twelve months ahead
  
  PD <- merge(PD, RecessionProb, all.x = TRUE)
  
# Merging with Sentix Economic US Aggregate overall Index
  
  PD <- merge(PD, BehavIndex, all.x = TRUE)
  
# Merging with NYSE Margin Debt
  
  PD <- merge(PD, MarginDebt, all.x = TRUE)
  
# Merging with Economic growth
  
  PD <- merge(PD, RealGDP, all.x = TRUE)
  
# 3684 Obs
  
# Row names:

rownames(PD) <- paste(PD$Symbol, PD$date, sep = " / ")

# Saving

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(PD, file="Panel-Data.RData")













