# Installing Bloomberg package

install.packages("Rblpapi")

library(Rblpapi)

blpConnect()

# Downloading S&P Composite 1500 members lists since 1990

  setwd('F:/CAMILO/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/S&P 1500')

  DateL <- format(c(seq(as.Date("1995-04-1"), as.Date("2020-01-1"), by="quarter")-1), "%Y%m%d")
  
  overrd <- ""
  Index <- ""
  RFile <- ""

  for (i in 1:length(DateL)){
    overrd <- c("END_DT"=DateL[i])
    Index <- rbind(bds("SPX Index", "INDX_MWEIGHT_HIST", overrides = overrd),
                   bds("MID Index", "INDX_MWEIGHT_HIST", overrides = overrd),
                   bds("SML Index", "INDX_MWEIGHT_HIST", overrides = overrd))
    RFile <- sprintf('S&P1500_%s.RData', substr(DateL[i], 1, 6))
    save(Index, file = RFile)
    print(i)
    print(DateL[i])
  }

  # The final stock list

  RFile <- ""
  Stocks <- data.frame("Index Member" = character(0), "Percent Weight" = numeric(0), check.names = FALSE)
  Index <- ""

  for (i in 1:length(DateL)){
    RFile <- sprintf('S&P1500_%s.RData', substr(DateL[i], 1, 6))
    load(file = RFile)
    Stocks <- merge(Stocks, Index, all = TRUE)
    print(i)
    print(DateL[i])
  }
  
  # Preparing the data
  
  Stocks[2]<- NULL
  names(Stocks)[1] <- "Symbol"
  Stocks$Symbol <- gsub('.{2}$', 'US', Stocks$Symbol)
  Stocks <- unique(Stocks)

  # Searching for the company name of the delisted stocks.
  Stocks <- cbind(Stocks, bdp(paste0(Stocks$Symbol," Equity"), "SECURITY_NAME"))
  names(Stocks)[2] <- "Company"

  # Empty Security Name: RESP US
  Stocks <- Stocks[-which(Stocks$Symbol == "RESP US"),]

  # Delet duplicated values
  Stocks[duplicated(Stocks[2]),]

  # Subseting the delisted stocks.
  Delist <- Stocks[grepl("[0-9]", Stocks$Symbol, ignore.case=TRUE) == TRUE, ]
  length(Delist)
  Delist <- subset(Stocks, grepl("[0-9]", Stocks$Symbol, ignore.case=TRUE) == TRUE)
  nrow(Delist)

  # Saving the stock list

  setwd('F:/CAMILO/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal')
  
  save(Stocks, file="Stock_List.RData")

# Market cap classification (FTSE Russell Indices) since 1990
  
  setwd('F:/CAMILO/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Russell Market Cap')

  DateL <- format(c(seq(as.Date("1995-07-31"), as.Date("2019-07-31"), by="year")), "%Y%m%d")
  
  # Large-Cap lists - Russell 1000
  
  overrd <- ""
  Index <- ""
  RFile <- ""
  
  for (i in 1:length(DateL)){
    overrd <- c("END_DT"=DateL[i])
    Index <- bds("RIY Index", "INDX_MWEIGHT_HIST", overrides = overrd)
    Index[2]<- NULL
    names(Index)[1] <- "Symbol"
    Index$Symbol <- gsub('.{2}$', 'US', Index$Symbol)
    RFile <- sprintf('Large_Cap_%s.RData', substr(DateL[i], 1, 4))
    save(Index, file = RFile)
    print(i)
    print(DateL[i])
  }
  
  # Mid-Cap lists - Russell Midcap
  
  overrd <- ""
  Index <- ""
  RFile <- ""
  
  for (i in 1:length(DateL)){
    overrd <- c("END_DT"=DateL[i])
    Index <- bds("RMC Index", "INDX_MWEIGHT_HIST", overrides = overrd)
    Index[2]<- NULL
    names(Index)[1] <- "Symbol"
    Index$Symbol <- gsub('.{2}$', 'US', Index$Symbol)
    RFile <- sprintf('Mid_Cap_%s.RData', substr(DateL[i], 1, 4))
    save(Index, file = RFile)
    print(i)
    print(DateL[i])
  }
  
  # Small-Cap lists - Russell 2000
  
  overrd <- ""
  Index <- ""
  RFile <- ""
  
  for (i in 1:length(DateL)){
    overrd <- c("END_DT"=DateL[i])
    Index <- bds("RTY Index", "INDX_MWEIGHT_HIST", overrides = overrd)
    Index[2]<- NULL
    names(Index)[1] <- "Symbol"
    Index$Symbol <- gsub('.{2}$', 'US', Index$Symbol)
    RFile <- sprintf('Samll_Cap_%s.RData', substr(DateL[i], 1, 4))
    save(Index, file = RFile)
    print(i)
    print(DateL[i])
  }
  
  # For anti_join:
  
  library(dplyr)
  
  # Large-Cap lists
  
  YY <- c(1995:2019)
  Index <- ""
  Index2 <- ""
  RFile <- ""
  RFile2 <- ""
  
  for (i in 1:length(YY)){
    RFile2 <- sprintf('Mid_Cap_%s.RData', YY[i])
    load(file = RFile2)
    Index2 <- Index
    RFile <- sprintf('Large_Cap_%s.RData', YY[i])
    load(file = RFile)
    Index <- anti_join(Index, Index2)
    save(Index, file = RFile)
    print(i)
    print(YY[i])
  }
  
# Value stocks / growth stocks (FTSE Russell Indices) since 1990
  
  setwd('F:/CAMILO/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Russell Value - Growth')

  DateL <- format(c(seq(as.Date("1995-07-31"), as.Date("2019-07-31"), by="year")), "%Y%m%d")
  
  # Value stocks - Russell 1000 Value & Russell 2000 Value
  
  overrd <- ""
  Index <- ""
  RFile <- ""
  
  for (i in 1:length(DateL)){
    overrd <- c("END_DT"=DateL[i])
    Index <- rbind(bds("RLV Index", "INDX_MWEIGHT_HIST", overrides = overrd),
                   bds("RUJ Index", "INDX_MWEIGHT_HIST", overrides = overrd))
    Index[2]<- NULL
    names(Index)[1] <- "Symbol"
    Index$Symbol <- gsub('.{2}$', 'US', Index$Symbol)
    RFile <- sprintf('Value_%s.RData', substr(DateL[i], 1, 4))
    save(Index, file = RFile)
    print(i)
    print(DateL[i])
  }
  
  # Growth stocks - Russell 1000 Growth & Russell 2000 Growth
  
  overrd <- ""
  Index <- ""
  RFile <- ""
  
  for (i in 1:length(DateL)){
    overrd <- c("END_DT"=DateL[i])
    Index <- rbind(bds("RLG Index", "INDX_MWEIGHT_HIST", overrides = overrd),
                   bds("RUO Index", "INDX_MWEIGHT_HIST", overrides = overrd))
    Index[2]<- NULL
    names(Index)[1] <- "Symbol"
    Index$Symbol <- gsub('.{2}$', 'US', Index$Symbol)
    RFile <- sprintf('Growth_%s.RData', substr(DateL[i], 1, 4))
    save(Index, file = RFile)
    print(i)
    print(DateL[i])
  }
  
  # For anti_join:
  
  library(dplyr)
  
  # Value stocks list
  
  YY <- c(1995:2019)
  Index <- ""
  Index2 <- ""
  RFile <- ""
  RFile2 <- ""
  
  for (i in 1:length(YY)){
    RFile2 <- sprintf('Growth_%s.RData', YY[i])
    load(file = RFile2)
    Index2 <- Index
    RFile <- sprintf('Value_%s.RData', YY[i])
    load(file = RFile)
    Index <- anti_join(Index, Index2)
    save(Index, file = RFile)
    print(i)
    print(YY[i])
  }

# Daily market data
  
  setwd('F:/CAMILO/Alpha Capital Investments/Hedge Fund/Research/Bloomberg Terminal/Market Data')

  # S&P 500
  
  SP500 <- bdh("SPX Index", c("PX_LAST", "PX_LOW", "PX_HIGH", "PX_VOLUME"), start.date = as.Date("1986-01-31"), end.date = as.Date("2019-12-31"), int.as.double = TRUE)

  # Citigroup Economic Surprise Index
  
  CitiIndex <- bdh("CESIUSD Index", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  
  # Equity Put/Call Ratio
  
  PutCallRatio  <- bdh("PCUSEQTR Index", "PX_LAST", start.date = as.Date("1986-08-01"), end.date = as.Date("2019-12-31"))
  
  # VIX
  
  VIX <- bdh("VIX Index", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  
  # Gold
  
  Gold <- bdh("XAU BGN Curncy", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # Oil
  
  Oil <- bdh("CL1 COMB Comdty", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # Natural Gas 

  NatuGas <- bdh("NG1 COMB Comdty", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # Copper
  
  Copper <- bdh("HG1 COMB Comdty", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # Silver
  
  Silver <- bdh("SI1 COMB Comdty", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # VXO
  
  VXO <- bdh("VXO Index", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  
  # Saving data series
  
  save(SP500, CitiIndex, PutCallRatio, VIX, Gold, Oil, NatuGas, Copper, Silver, VXO, file="Daily-market-Data.RData")
  
# Weekly market data
  
  # US Investor sentiment bullish readings
  
  BullSent <- bdh("AAIIBULL Index", "PX_LAST", start.date = as.Date("1987-07-23"), end.date = as.Date("2019-12-31"))
  
  # US Investor sentiment bearish readings
  
  BearSent <- bdh("AAIIBEAR Index", "PX_LAST", start.date = as.Date("1987-07-23"), end.date = as.Date("2019-12-31"))
  
  # US Investor sentiment neutral readings
  
  NeutralSent <- bdh("AAIINEUT Index", "PX_LAST", start.date = as.Date("1987-07-23"), end.date = as.Date("2019-12-31"))
  
  # Saving data series
  
  save(BullSent, BearSent, NeutralSent, file="Weekly-market-Data.RData")
  
# Monthly market data
  
  # U.S. Recession Indicator
  
  USRecession <- bdh("USRINDEX Index", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # PMI index & release dates
  
  PMI <- bdh("NAPMPMI Index", c("PX_LAST", "ECO_RELEASE_DT"), start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  
  # Nonfarm payrolls - release dates
  
  NonfarmPayRelease <- bdh("NFP TCH Index", "ECO_RELEASE_DT", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # housing starts - release dates
  
  HouseStartRelease <- bdh("NHSPSTOT Index", "ECO_RELEASE_DT", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # CPI - release dates
  
  CPIRelease <- bdh("CPI CHNG Index", "ECO_RELEASE_DT", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # FOMC rate decisions - release dates
  
  FOMCRateRelease <- bdh("FDTR Index", "ECO_RELEASE_DT", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))

  # NY Fed prob of recession in US twelve months ahead
  
  RecessionProb <- bdh("NYFYPROB Index", "PX_LAST", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  
  # Sentix Economic US Aggregate overall Index (Sentix Behavioral Index)
  
  BehavIndex <- bdh("SNTEUSGX Index", "PX_LAST", start.date = as.Date("1986-07-31"), end.date = as.Date("2019-12-31"))
  
  # NYSE Margin Debt
  
  MarginDebt <- bdh("MARGDEBT Index", "PX_LAST", start.date = as.Date("1986-01-31"), end.date = as.Date("2019-12-31"))
  
  # Saving data series
  
  save(USRecession, PMI, NonfarmPayRelease, HouseStartRelease, CPIRelease, FOMCRateRelease, 
       RecessionProb, BehavIndex, MarginDebt, file="Monthly-market-Data.RData")

# Quarterly market data
  
  # Economic growth
  
  RealGDP <- bdh("GDP CYOY Index", "PX_LAST", start.date = as.Date("1985-12-31"), end.date = as.Date("2019-12-31"))
  
  # GDP - release dates
  
  GDPRelease <- bdh("GDP CQOQ Index", "ECO_RELEASE_DT", start.date = as.Date("1986-01-01"), end.date = as.Date("2019-12-31"))
  # It only reports the third estimate dates
  # I need the Advance Estimates
  
  # Saving data series
  
  save(RealGDP, GDPRelease, file="Quarterly-market-Data.RData")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  