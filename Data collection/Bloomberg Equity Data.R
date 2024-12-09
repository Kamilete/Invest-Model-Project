# Installing Bloomberg package

install.packages("Rblpapi")

library(Rblpapi)

blpConnect()

# Installing multiple global string replacement function

install.packages("devtools")

devtools::install_github("bmewing/mgsub")

library(mgsub)

# The stock list to download

load(file="Stock_List.RData")

# Download the data and save them as RData Files (Daily Equity data)

Ticker <- ""
txtFile <- ""
DF <- ""
DF2 <- ""
RFile <- ""

opt1 <- structure(c("TRUE", "TRUE", "TRUE"), names = c("adjustmentNormal", "adjustmentAbnormal", "adjustmentSplit"))
flds1 <- c("PX_LAST", "PX_LOW", "PX_HIGH",  "PX_VOLUME", "ASK", "BID", "CORR_COEF", "BETA_RAW_OVERRIDABLE",
          "ALPHA_OVERRIDABLE", "PE_RATIO", "MARKET_CAPITALIZATION_TO_BV", "OPEN_INT_TOTAL_CALL", "OPEN_INT_TOTAL_PUT",
          "VOLUME_TOTAL_CALL", "VOLUME_TOTAL_PUT", "DIVIDEND_12_MONTH_YIELD")

opt2 <- structure(c("FALSE", "FALSE", "FALSE"), names = c("adjustmentNormal", "adjustmentAbnormal", "adjustmentSplit"))
flds2 <- c("PX_LAST", "PX_LOW", "PX_HIGH", "PX_VOLUME")

for (i in 1:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bdh(Ticker, fields = flds1, start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"), options = opt1, int.as.double = TRUE)
  DF2 <- bdh(Ticker, fields = flds2, start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"), options = opt2, int.as.double = TRUE)
  names(DF2) <- c("date", "Close_unadj", "Low_unadj", "High_unadj", "Volume_unadj")
  DF <- merge(DF2, DF)
  DF <- DF[order(DF$date, decreasing = TRUE),]
  DF$Symbol <- Stocks[i,1]
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_D.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# No data: "1558783D US" Row: 195 / "GMCEQ US" Row: 1902 / "MDR US" Row: 2463 / "TT US" rOW: 3465
# Few data: "1752326D US" Row: 259 / "1656784D" Row: 227

# Deleting stocks without data:

out <- c(195, 227, 259, 1902, 2463, 3465)
Stocks <- Stocks[-c(out),]

save(Stocks, file="Stock_List.RData")

# Download the data and save them as RData Files (Twice per month data)

Ticker <- ""
txtFile <- ""
DF <- ""
RFile <- ""
flds <- c("SHORT_INT", "SI_PERCENT_EQUITY_FLOAT", "SHORT_INT_RATIO")

for (i in 1:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bdh(Ticker, fields = flds, start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  DF <- DF[order(DF$date, decreasing = TRUE),]
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_2XM.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# Download the data and save them as RData Files (Quarterly data)

Ticker <- ""
txtFile <- ""
DF <- ""
DF2 <- ""
RFile <- ""
flds <- c("TANG_BOOK_VAL_PER_SH", "NUM_OF_EMPLOYEES", 
          "TOT_DEBT_TO_TOT_EQY", "IS_COMP_SALES")
opt <- structure(c("FISCAL", "QUARTERLY"), names = c("periodicityAdjustment", "periodicitySelection")) 
ovrd <- c("BEST_FPERIOD_OVERRIDE"="FQ")

for (i in 1:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bdh(Ticker, fields = flds, start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  DF2 <- bdh(Ticker, "BEST_SALES", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"), options=opt, overrides=ovrd)
  DF <- merge(DF, DF2, all = TRUE)
  DF <- DF[order(DF$date, decreasing = TRUE),]
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_Q.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# Download the data and save them as RData Files (Quarterly Earnings with EPS Data Sets)

Ticker <- ""
txtFile <- ""
DF <- ""
RFile <- ""

for (i in 1:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bds(Ticker, "EARN_ANN_DT_TIME_HIST_WITH_EPS")
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_Q_Earn.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# Download the data and save them as RData Files (Quarterly Dividends Data Sets)

Ticker <- ""
txtFile <- ""
DF <- ""
RFile <- ""
overrd <- c("DVD_START_DT"="19860101", "DVD_END_DT"="20200101")

for (i in 1:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bds(Ticker, "DVD_HIST_ALL",  overrides = overrd)
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_Q_Div.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# Download the data and save them as RData Files (Company data)

StockList <- as.vector(Stocks$Symbol)
StockList <- paste0(StockList," Equity")
flds <- c("GICS_SECTOR_NAME", "GICS_INDUSTRY_GROUP_NAME", "GICS_INDUSTRY_NAME", "GICS_SUB_INDUSTRY_NAME", 
          "COUNTRY_FULL_NAME", "SECURITY_TYP")
Static <- bdp(StockList, fields = flds)
Static$Symbol <- row.names(Static)
Static$Symbol <- gsub('.{7}$', '', Static$Symbol)

# Filling GICS missing data with Security Type data:

Static$GICS_SECTOR_NAME[which(Static$Symbol == "LFB US Equity")] <- "Materials"
Static$GICS_INDUSTRY_GROUP_NAME[which(Static$Symbol == "LFB US Equity")] <- "Materials"
Static$GICS_INDUSTRY_NAME[which(Static$Symbol == "LFB US Equity")] <- "Paper & Forest Products"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "LFB US Equity")] <- "Paper Products"

Static$GICS_SECTOR_NAME[which(Static$Symbol == "NEWCQ US Equity")] <- "Financials"
Static$GICS_INDUSTRY_GROUP_NAME[which(Static$Symbol == "NEWCQ US Equity")] <- "Diversified Financials"
Static$GICS_INDUSTRY_NAME[which(Static$Symbol == "NEWCQ US Equity")] <- "Mortgage Real Estate Investmen"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "NEWCQ US Equity")] <- "Mortgage REITs"

Static$GICS_SECTOR_NAME[Static$GICS_SECTOR_NAME == "" & Static$SECURITY_TYP == "REIT"] <- "Real Estate"
Static$GICS_INDUSTRY_GROUP_NAME[Static$GICS_INDUSTRY_GROUP_NAME == "" & Static$SECURITY_TYP == "REIT"] <- "Real Estate"
Static$GICS_INDUSTRY_NAME[Static$GICS_INDUSTRY_NAME == "" & Static$SECURITY_TYP == "REIT"] <- "Equity Real Estate Investment"

Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "0574018D US Equity")] <- "Residential REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "1013516Q US Equity")] <- "Office REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "1438055D US Equity")] <- "Retail REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "8394653Q US Equity")] <- "Industrial REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "BRE US Equity")] <- "Residential REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "CLP US Equity")] <- "Diversified REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "EOP US Equity")] <- "Office REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "GBP US Equity")] <- "Residential REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "GLB US Equity")] <- "Diversified REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "NHP US Equity")] <- "Health Care REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "NXL US Equity")] <- "Retail REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "SHU US Equity")] <- "Specialized REITs"
Static$GICS_SUB_INDUSTRY_NAME[which(Static$Symbol == "TCT US Equity")] <- "Residential REITs"

save(Static, file = 'Static_data.RData')

# Download the data and save them as RData Files (Mergers & acquisitions Data Sets)

Ticker <- ""
txtFile <- ""
DF <- ""
RFile <- ""

for (i in 1:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bds(Ticker, "MERGERS_AND_ACQUISITIONS")
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_M&A.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# Download the data and save them as RData Files (Earnings dates announcements)

Ticker <- ""
txtFile <- ""
DF <- ""
RFile <- ""

for (i in 3572:nrow(Stocks)){
  Ticker <- paste0(Stocks[i,1]," Equity")
  DF <- bdh(Ticker, "LATEST_ANNOUNCEMENT_DT", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  DF <- DF[order(DF$date, decreasing = TRUE),]
  txtFile <- mgsub(Stocks[i,1], pattern=c("/"," "), replacement=c("-","_"))
  RFile <- sprintf('%s_Q_EDate.RData', txtFile)
  save(DF, file = RFile)
  print(i)
  print(Stocks[i,1])
}

# No data: "2761917Q US" Row: 402 














