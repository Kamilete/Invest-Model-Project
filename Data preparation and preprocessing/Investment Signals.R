# Disabling scientific notation

options(scipen=999)

# The stock list

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
load(file="Stock_List.RData")

  # Removing New York Registry Shares: "RDPL US" Row: 3015 / "UN US" Row: 3526

  Stocks <- Stocks[-c(3015, 3526),]
  
  # Removing shares listed on foreign exchanges:
  # "AMCR US" Row: 893 / "CCEP US" Row: 1219 / "FTI US" Row: 1826 / "JHG US" Row: 2211 / "LIN US" Row: 2350 / "PRGO US" Row:2929
  
  Stocks <- Stocks[-c(893, 1219, 1826, 2211, 2350, 2929),]

  save(Stocks, file="Stock_List.RData")

library(mgsub)

Stocks <- mgsub(Stocks[1], pattern=c("/"," "), replacement=c("-","_"))

# Libraries

# For lag, zoo and na.locf:

library(zoo)

# For rowSds: 

library(matrixStats)

# For adjust:

library(RQuantLib)

# Transformation function for Daily Equity Data

xFuncT <- function(data) {
  
  # Month volume average
  
  data$AvgVol30D = rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=1:30, na.pad=TRUE)))
  
  # Month volume standard deviation
  
  data$SDVol30D = rowSds(coredata(lag(zoo(data$PX_VOLUME), k=1:30, na.pad=TRUE)))
  
  # 0.75 standard deviations over the month average variable
  
  data$Vol30D0.75 = data$AvgVol30D + 0.75 * data$SDVol30D
  
  # Close price of today is greater than yesterday's close price
  
  data$MCloseUp = as.integer(data$PX_LAST > coredata(lag(zoo(data$PX_LAST), +1, na.pad=TRUE)))
  
  # Minimum price of today is greater than yesterday's minimum price 
  
  data$MLow = as.integer(data$PX_LOW > coredata(lag(zoo(data$PX_LOW), +1, na.pad=TRUE)))
  
  # Close price of today is less than yesterday's close price
  
  data$MCloseDown = as.integer(data$PX_LAST < coredata(lag(zoo(data$PX_LAST), +1, na.pad=TRUE)))
  
  # Maximum price of today is less than yesterday's maximum price 
  
  data$MHigh = as.integer(data$PX_HIGH < coredata(lag(zoo(data$PX_HIGH), +1, na.pad=TRUE)))
  
  # The volume must be 0.75  standard deviations over the month  average
  
  data$MVolume = as.integer(data$PX_VOLUME > data$Vol30D0.75)
  
  # The Up variable
  
  data$Up = as.integer(data$MCloseUp + data$MLow + data$MVolume == 3)
  
  # The Down variable
  
  data$Down = as.integer(data$MCloseDown + data$MHigh + data$MVolume == 3)
  
  # Momentum Strategy Buy/Sell
  
  data$Strategy = as.factor(ifelse(rowSums(coredata(lag(zoo(data$Up), k=0:4, na.pad=TRUE)), na.rm = TRUE) == 5, "Sell", 
                                   ifelse(rowSums(coredata(lag(zoo(data$Down), k=0:4, na.pad=TRUE)), na.rm = TRUE) == 5, "Buy", "")))
  
  # Bid-Ask Spread:
  
  data$BID_ASK = round((data$ASK - data$BID) / rowMeans(data[,c("ASK","BID")]) * 100, digits = 3)
  
  # Month Bid-Ask Spread average
  
  data$Avg_BID_ASK = rowMeans(coredata(lag(zoo(data$BID_ASK), k=1:30, na.pad=TRUE)))
  
  # 6-Month (130 working days) volume average
  
  data$AvgVol_6m = rowMeans(coredata(lag(zoo(data$PX_VOLUME), k=1:130, na.pad=TRUE)))
  
  data
  
}

# Data transformation

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Daily Equity Data')

DF <- ""
RFile1 <- ""
RFile2 <- ""
Dir <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data/'

for (i in 1:nrow(Stocks)){
  RFile1 <- sprintf('%s_D.RData', Stocks[i,1])
  load(file = RFile1)
  DF <- xFuncT(DF)
  RFile2 <- paste0(Dir ,sprintf('%s.RData', Stocks[i,1]))
  save(DF, file = RFile2)
  print(i)
  print(Stocks[i,1])
}

# Comparing Earnings dates announcements between files

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
load(file="Stock_List.RData")

which(Stocks$Symbol == "2761917Q_US")
Stocks2 <- Stocks[-402,]

library(mgsub)
Stocks2 <- mgsub(Stocks2[1], pattern=c("/"," "), replacement=c("-","_"))

Difference <- vector(mode = "integer", length = nrow(Stocks2))
Diff <- data.frame(Difference, "Dim_Earn_Dt" = numeric(length = 3782), "Dim_Earn_EPS" = numeric(length = 3782))
rownames(Diff) <- Stocks2$Symbol

RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings Dates/'
RFile3 <- ""
Dir3 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings with EPS/'

for (i in 1:nrow(Stocks2)){
  RFile2 <- paste0(Dir2, sprintf('%s_Q_EDate.RData', Stocks2[i,1]))
  load(file = RFile2)
  ErnDT <- DF
  ErnDT <- subset(ErnDT, LATEST_ANNOUNCEMENT_DT < "2020-01-01")
  Diff[i, 2] <- dim(ErnDT)[1]
  RFile3 <- paste0(Dir3, sprintf('%s_Q_Earn.RData', Stocks2[i,1]))
  load(file = RFile3)
  if (dim(DF)[2] == 1) {
    Diff[i, 1] <- dim(ErnDT)[1] - 0
  } else {
    DF <- DF[DF[,1] < "2020-01-01",]
    Diff[i, 1] <- dim(ErnDT)[1] - dim(DF)[1]
    Diff[i, 3] <- dim(DF)[1]
  }
  print(i)
  print(Stocks2[i,1])
}

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')

save(Diff, file="Earnings_Dates_Diff.RData")

# Merging with Earnings dates announcements

# Delete erroneous Earnings dates (1899-12-31) from "1690Q_US" and "DSCP_US"

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
load(file="Stock_List.RData")

Stocks2 <- Stocks[-402,]

library(mgsub)
Stocks2 <- mgsub(Stocks2[1], pattern=c("/"," "), replacement=c("-","_"))

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
ErnDT <- ""
ADF <- ""
dim2 <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings Dates/'
RFile3 <- ""
Dir3 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings with EPS/'

for (i in 1:nrow(Stocks2)){
  RFile2 <- paste0(Dir2, sprintf('%s_Q_EDate.RData', Stocks2[i,1]))
  load(file = RFile2)
  ErnDT <- DF
  RFile3 <- paste0(Dir3, sprintf('%s_Q_Earn.RData', Stocks2[i,1]))
  load(file = RFile3)
  dim2 <- ifelse(dim(DF)[2] == 1, 0, dim(DF[DF[,1] < "2020-01-01",])[1])
  if (dim(subset(ErnDT, LATEST_ANNOUNCEMENT_DT < "2020-01-01"))[1] - dim2 >= -1) {
    ErnDT$LATEST_ANNOUNCEMENT_DT <- adjust("UnitedStates/NYSE", ErnDT$LATEST_ANNOUNCEMENT_DT)
    ErnDT$Next_Earnings <- ErnDT$LATEST_ANNOUNCEMENT_DT
    ErnDT$date <- NULL
    names(ErnDT)[1] <- "date"
    if (dim(DF)[2] == 1)  {
      ADF <- ErnDT
      if (dim(ADF)[1] == 0) {
        ADF$Next_Earnings_Time <- as.character()
      } else {
        ADF$Next_Earnings_Time <- as.character("")
      }
    }  else {
      names(DF)[1] <- "date"
      DF$date <- adjust("UnitedStates/NYSE", DF$date)
      ADF <- merge(ErnDT, DF[,c(1, 2)], all.x = TRUE)
      ADF <- ADF[order(ADF$date, decreasing = TRUE),]
      names(ADF)[3] <- "Next_Earnings_Time"
    }
  } else {
    ADF <- DF[,c(1, 2)]
    names(ADF)[1] <- "Next_Earnings"
    names(ADF)[2] <- "Next_Earnings_Time"
    ADF$Next_Earnings <- adjust("UnitedStates/NYSE", ADF$Next_Earnings)
    ADF$date <- ADF$Next_Earnings
  }
  RFile1 <- sprintf('%s.RData', Stocks2[i,1])
  load(file = RFile1)
  DF <- merge(DF, ADF, all.x = TRUE)
  DF <- DF[order(DF$date, decreasing = TRUE),]
  DF$Next_Earnings <- na.locf(DF$Next_Earnings, na.rm = FALSE)
  DF$Next_Earnings_Time <- na.locf(DF$Next_Earnings_Time, na.rm = FALSE)
  save(DF, file = RFile1)
  print(i)
  print(Stocks2[i,1])
}

load(file = 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Earnings with EPS/2761917Q_US_Q_Earn.RData')
ADF <- DF[,c(1, 2)]
names(ADF)[1] <- "Next_Earnings"
names(ADF)[2] <- "Next_Earnings_Time"
ADF$Next_Earnings <- adjust("UnitedStates/NYSE", ADF$Next_Earnings)
ADF$date <- ADF$Next_Earnings
RFile <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data/2761917Q_US.RData'
load(file = RFile)
DF <- merge(DF, ADF, all.x = TRUE)
DF <- DF[order(DF$date, decreasing = TRUE),]
DF$Next_Earnings <- na.locf(DF$Next_Earnings, na.rm = FALSE)
DF$Next_Earnings_Time <- na.locf(DF$Next_Earnings_Time, na.rm = FALSE)
save(DF, file = RFile)

# Merging with Dividends Data
# Fill the missing Ex Date for "BVSN_US"

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
DV1 <- ""
DV2 <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Quarterly Equity Data/Dividends/'

for (i in 1162:nrow(Stocks)){
  RFile2 <- paste0(Dir2, sprintf('%s_Q_Div.RData', Stocks[i,1]))
  load(file = RFile2)
  if (class(DF) == "NULL")  {
    RFile1 <- sprintf('%s.RData', Stocks[i,1])
    load(file = RFile1)
    DF$Ex_Date <- as.Date(NA)
    DF$Declared_Date <- as.Date(NA)
    DF$Dividend_Type <- as.character(NA)
    DF$Next_Declared_Date <- as.Date(NA)
    DF$Next_Dividend_Type <- as.character(NA)
    save(DF, file = RFile1)
    print(i)
    print(Stocks[i,1])
  }  else {
    DV1 <- subset(DF, `Dividend Type` %in% c("Spinoff", "Stock Split", "Poison Pill Rights", "Return of Capital", 
                                           "Pfd Rights Redemption", "Rights Issue", "Stock Dividend", "Special Cash", "Bonus",
                                            "Long Term Cap Gain", "Preferred Right", "Discontinued", "Liquidation", 
                                           "In-specie", "Rights Redemption", "Pro Rata", "Misc"))
    DV2 <- subset(DF, `Dividend Type` %in% c("Spinoff", "Poison Pill Rights", "Rights Issue", "Pfd Rights Redemption", "Stock Dividend",
                                           "Special Cash", "Return of Capital", "Bonus", "Long Term Cap Gain", 
                                           "Discontinued", "Preferred Right", "Liquidation", "In-specie", "Rights Redemption",
                                            "Pro Rata"))
    names(DV1)[5] <- "Ex_Date"
    DV1$Ex_Date <- adjust("UnitedStates/NYSE", DV1$Ex_Date)
    DV1$date <- DV1$Ex_Date
    RFile1 <- sprintf('%s.RData', Stocks[i,1])
    load(file = RFile1)
    DF <- merge(DF, DV1[,c(5, 8)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF$Ex_Date <- na.locf(DF$Ex_Date, na.rm = FALSE)
    names(DV2)[1] <- "Declared_Date"
    names(DV2)[4] <- "Dividend_Type"
    DV2$Declared_Date <- adjust("UnitedStates/NYSE", DV2$Declared_Date)
    DV2$date <- DV2$Declared_Date
    DV2$Next_Declared_Date <- DV2$Declared_Date
    DV2$Next_Dividend_Type <- DV2$Dividend_Type
    DF <- merge(DF, DV2[,c(1, 4, 8:10)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF$Declared_Date <- na.locf(DF$Declared_Date, na.rm = FALSE, fromLast = TRUE)
    DF$Dividend_Type <- na.locf(DF$Dividend_Type, na.rm = FALSE, fromLast = TRUE)
    DF$Next_Declared_Date <- na.locf(DF$Next_Declared_Date, na.rm = FALSE)
    DF$Next_Dividend_Type <- na.locf(DF$Next_Dividend_Type, na.rm = FALSE)
    save(DF, file = RFile1)
    print(i)
    print(Stocks[i,1])
  }
}

# Merging with Mergers & acquisitions Data Sets:

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

DF <- ""
MA <- ""
RFile1 <- ""
RFile2 <- ""
Dir2 <- 'C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Mergers & Acquisitions/'

for (i in 1:nrow(Stocks)){
  RFile2 <- paste0(Dir2, sprintf('%s_M&A.RData', Stocks[i,1]))
  load(file = RFile2)
  if (class(DF) == "NULL")  {
    RFile1 <- sprintf('%s.RData', Stocks[i,1])
    load(file = RFile1)
    DF$Spin_Buy_Date <- as.Date(NA)
    DF$Spin_Buy_Type <- as.character(NA)
    DF$Next_Spin_Buy_Date <- as.Date(NA)
    DF$Next_Spin_Buy_Type <- as.character(NA)
    save(DF, file = RFile1)
    print(i)
    print(Stocks[i,1])
  }  else {
    MA <- subset(DF, `Deal Type` %in% c("SPIN", "BUY"))
    names(MA)[3] <- "Spin_Buy_Date"
    names(MA)[6] <- "Spin_Buy_Type"
    MA$Spin_Buy_Date <- adjust("UnitedStates/NYSE", MA$Spin_Buy_Date)
    MA$date <- MA$Spin_Buy_Date
    MA$Next_Spin_Buy_Date <- MA$Spin_Buy_Date
    MA$Next_Spin_Buy_Type <- MA$Spin_Buy_Type
    RFile1 <- sprintf('%s.RData', Stocks[i,1])
    load(file = RFile1)
    DF <- merge(DF, MA[,c(3, 6, 8:10)], all.x = TRUE)
    DF <- DF[order(DF$date, decreasing = TRUE),]
    DF$Spin_Buy_Date <- na.locf(DF$Spin_Buy_Date, na.rm = FALSE, fromLast = TRUE)
    DF$Spin_Buy_Type <- na.locf(DF$Spin_Buy_Type, na.rm = FALSE, fromLast = TRUE)
    DF$Next_Spin_Buy_Date <- na.locf(DF$Next_Spin_Buy_Date, na.rm = FALSE)
    DF$Next_Spin_Buy_Type <- na.locf(DF$Next_Spin_Buy_Type, na.rm = FALSE)
    save(DF, file = RFile1)
    print(i)
    print(Stocks[i,1])
  }
}

# Subseting the data with investment signal (TIS):

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Full Equity Data')

TIS <- data.frame()
RFile <- ""
datfr <- ""

for (i in 1:nrow(Stocks)){
  RFile <- sprintf('%s.RData', Stocks[i,1])
  load(file = RFile)
  datfr <- subset(DF, Strategy == "Buy" | Strategy == "Sell")
  TIS <- rbind(TIS, datfr)
  print(i)
  print(Stocks[i,1])
}

# Removing duplicate observation in "FNMA US"

TIS <- TIS[-2613,]

# Row names:

rownames(TIS) <- paste(TIS$Symbol, TIS$date, sep = " / ")

# Refreshing the factor variable "Strategy", to remove the unnecessary category:

TIS$Strategy <- factor(TIS$Strategy)

# Checking data frame

str(TIS)
summary(TIS)
head(TIS)

# Filtered by acceptable investment signals:

  # Prices greater or equall to 15:

  TIS <- subset(TIS, Close_unadj >= 15)
  
  # Extraction of the small-medium biotechnology companies:
  
  setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Evaluation of investment strategies')
  
  Biotech <- as.vector(read.csv("Biotech.csv", stringsAsFactors = FALSE)$Stock)
  Biotech <- paste0(Biotech," US")
  TIS <- subset(TIS, !(Symbol %in% Biotech))
  
  # symbol - Year:
  
  library(lubridate)
  TIS$SymbYr <- paste(TIS$Symbol, year(TIS$date), sep = " ")
  
  # Merging with the static data:
  
  Static$Symbol <- gsub('.{7}$', '', Static$Symbol)
  TIS <- merge(TIS, Static[,c(2, 7)], all.x = TRUE)
  TIS$GICS_INDUSTRY_GROUP_NAME <- as.factor(TIS$GICS_INDUSTRY_GROUP_NAME)
  
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
  TIS <- merge(TIS, Cap, by ="SymbYr", all.x = TRUE)
  TIS$Market_Cap <- as.factor(TIS$Market_Cap)
  TIS$SymbYr <- NULL
  
  # Do not invest in small-medium biotechnology and pharmaceuticals companies
  
  TIS <- subset(TIS, !(GICS_INDUSTRY_GROUP_NAME == "Pharmaceuticals, Biotechnology" & Market_Cap == "Small-Cap"))
  TIS <- subset(TIS, !(GICS_INDUSTRY_GROUP_NAME == "Pharmaceuticals, Biotechnology" & Market_Cap == "Mid-Cap"))
  # 4551 Obs

  # A time lapse of more than 10 business days between the investment signal and the Earnings release
  
  TIS$Next_Earnings[is.na(TIS$Next_Earnings) == TRUE] <- as.Date("2020-01-03")
  
  library(RQuantLib) # For businessDaysBetween
  
  TIS$Days_Next_Earn <- businessDaysBetween("UnitedStates/NYSE", TIS$date, TIS$Next_Earnings, includeFirst = FALSE, includeLast = TRUE)
  
  TIS <- subset(TIS, Days_Next_Earn > 10 | Days_Next_Earn == 0)
                                     
  # Only after-market announcements are accepted in cases of 11 business days between the investment signal and the Earnings release
  
  D11 <- subset(TIS, Days_Next_Earn == 11)
  
  which(TIS$date == "2013-04-23" & TIS$Symbol == "DV US")
  
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "Bef-mkt"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "07:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "07:15"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "07:30"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:30"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:31"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "08:45"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "09:30"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "10:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "11:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "12:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 11 & Next_Earnings_Time == "13:00"))

  # Only announcements before mid-day session are accepted in cases of 0 business days between the investment signal and the Earnings release
  # It means, before 12:45 AM
  
  D11 <- subset(TIS, Days_Next_Earn == 0)
  
  which(TIS$date == "2013-04-23" & TIS$Symbol == "DV")
  
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "Aft-mkt"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:01"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:02"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:05"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:09"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:10"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:20"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:23"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "16:30"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "17:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "17:05"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "18:30"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "18:32"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "18:34"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "19:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "20:00"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "20:01"))
  TIS <- subset(TIS, !(Days_Next_Earn == 0 & Next_Earnings_Time == "20:05"))

  # A time lapse of more than 11 business days between the investment signal and corporate actions
  
  TIS$Ex_Date[is.na(TIS$Ex_Date) == TRUE] <- as.Date("2020-01-03")
  
  TIS$Days_Next_Corp_Act <- businessDaysBetween("UnitedStates/NYSE", TIS$date, TIS$Ex_Date, includeFirst = FALSE, includeLast = TRUE)
  
  TIS <- subset(TIS, Days_Next_Corp_Act > 11)
  
  # Do not invest after news about Spinoff, Poison Pill Rights, Rights Issues, Pfd Rights Redemptions, Stock Dividends, Return of Capital,
  # Bonuses, Discontinued equity, Preferred Rights, Liquidations, In-species, Special Cash, Long Term Cap Gain, Pro Rata and Rights Redemptions
  
  TIS$Declared_Date[is.na(TIS$Declared_Date) == TRUE] <- as.Date("1985-12-31")
  
  TIS$Days_Last_Corp_Act <- businessDaysBetween("UnitedStates/NYSE", TIS$Declared_Date, TIS$date, includeFirst = TRUE, includeLast = TRUE)
  
  TIS <- subset(TIS, Days_Last_Corp_Act > 6)
  
  # Do not invest after news about Spinoff and buybacks
  
  TIS$Spin_Buy_Date[is.na(TIS$Spin_Buy_Date) == TRUE] <- as.Date("1985-12-31")
  
  TIS$Days_Last_Spin_Buy <- businessDaysBetween("UnitedStates/NYSE", TIS$Spin_Buy_Date, TIS$date, includeFirst = TRUE, includeLast = TRUE)
  
  TIS <- subset(TIS, Days_Last_Spin_Buy > 6)
  # 3791 Obs

# Saving TIS
  
setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
save(TIS, file="Total-Investment-Signals.RData")
  
# New stock list:

NStocks <- unique(TIS$Symbol)

setwd('C:/Users/juang/Documents/Camilo/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')

save(NStocks, file="Final_Stock_List.RData")

# Searching for "2761917Q US"

which(TIS$Symbol == "2761917Q US")
which("2761917Q US" == NStocks)

# If it is found in the new stock list, code as appropriate to download the Earnings Dates Announcements files
# Because there is not file for this stock



















