# Summary of the relevant variables

PDB <- subset(PD, Strategy == "Buy")
PDS <- subset(PD, Strategy == "Sell")

PDBVar <- subset(PDB, select = c(DT:DVolUp,MaxCorr10D:MaxMoment10D,MonthN:ChngSP500D5,ChngSP500:ChngVIX))
PDSVar <- subset(PDS, select = c(DT:DVolUp,MaxCorr10D:MaxMoment10D,MonthN:ChngSP500D5,ChngSP500:ChngVIX))

my.summary <- function(x) {data.frame(Min=min(x,na.rm=TRUE),
                                   Median=median(x,na.rm=TRUE),
                                   Mean=mean(x,na.rm=TRUE),
                                   Max=max(x,na.rm=TRUE),
                                   SD=sd(x, na.rm=TRUE))}

sapply(PDBVar, my.summary)
sapply(PDSVar, my.summary)

# Max. % change momentum correction in 5 days:

plot(PD$Strategy, PD$MaxCorr5D, xlab = "Strategy", ylab = "Max. % change", main = "Max. % change momentum correction in 5 days")

tapply(PD$MaxCorr5D, PD$Strategy, summary)

# Max. % change in the continuity of the momentum in 5 days:

plot(PD$Strategy, PD$MaxMoment5D, xlab = "Strategy", ylab = "Max. % change", main = "Max. % change in the continuity of the momentum in 5 days")

tapply(PD$MaxMoment5D, PD$Strategy, summary)

# % change of the momentum patron per strategy:

plot(PD$Strategy, PD$MomentumCh, xlab = "Strategy", ylab = "% change", main = "% change of the momentum patron")

tapply(PD$MomentumCh, PD$Strategy, summary)

# Date per strategy:

summary(PD$Date)

tapply(PD$Date, PD$Strategy, summary)

# Max & Min (outliers analysis):

tapply(PD$MaxCorr5D, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$MaxCorr5D) # Profit
which.min(subset(PD, Strategy == "Buy")$MaxCorr5D) # Loss

which.max(subset(PD, Strategy == "Sell")$MaxCorr5D) # Loss
which.min(subset(PD, Strategy == "Sell")$MaxCorr5D) # Profit - Error in data

tapply(PD$MaxMoment5D, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$MaxMoment5D) # Profit
which.min(subset(PD, Strategy == "Buy")$MaxMoment5D) # Loss

which.max(subset(PD, Strategy == "Sell")$MaxMoment5D) # Loss
which.min(subset(PD, Strategy == "Sell")$MaxMoment5D) # Profit

tapply(PD$MomentumCh, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$MomentumCh) # Profit
which.min(subset(PD, Strategy == "Buy")$MomentumCh) # Loss

which.max(subset(PD, Strategy == "Sell")$MomentumCh) # Loss
which.min(subset(PD, Strategy == "Sell")$MomentumCh) # Loss

# Plotting:

# Scatterplots

par(mfrow = c(1,2))

plot(PD$MomentumCh, PD$MaxCorr10D, xlab = "% change of the momentum patron", ylab = "% change of the Max.correction in 5 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = -0.5, col = "red")
abline(v = 0.5, col = "red")
abline(h = -0.25, col = "red")
abline(h = 0.25, col = "red")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(PD$MomentumCh, PD$MaxMoment10D, xlab = "% change of the momentum patron", ylab = "% change in the Max. continuity of the momentum in 5 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = -0.5, col = "red")
abline(v = 0.5, col = "red")
abline(h = -0.25, col = "red")
abline(h = 0.25, col = "red")

par(mfrow = c(1,1))

plot(subset(PD, Strategy == "Buy")$MaxMoment10D, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "% change in the Max. continuity of the momentum", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = -0.1, col = "red")
abline(h = 0.1, col = "red")

plot(subset(PD, Strategy == "Sell")$MaxMoment5D, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "% change in the Max. continuity of the momentum", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = 0.1, col = "red")
abline(h = -0.1, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "Date", ylab = "% change", main = "Max. continuity of the momentum in 5 days - Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")
plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = "Date", ylab = "% change", main = "Max. continuity of the momentum in 5 days - Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$MomentumCh, xlab = "Date", ylab = "% change", main = "Momentum patron - Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = -0.2, col = "blue")
abline(h = -0.1, col = "red")
plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$MomentumCh, xlab = "Date", ylab = "% change", main = "Momentum patron - Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.2, col = "blue")
abline(h = 0.1, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "Date", ylab = "% change", main = "Max.correction in 5 days - Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")
abline(h = 0.0125, col = "red")
plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "Date", ylab = "% change", main = "Max.correction in 5 days - Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")
abline(h = -0.0125, col = "red")

# Histograms

hist(PD$Date, freq = TRUE, ylim = c(0,300), breaks = 30, xlab="Year", main="Date")
hist(PD$Date, freq = TRUE, breaks = 30)

hist(subset(PD, Strategy == "Buy")$Date, freq = TRUE, ylim = c(0,250), breaks = 30, xlab="Year", main="Date - Buy Strategy")
hist(subset(PD, Strategy == "Buy")$Date, plot = FALSE, breaks = 30)

hist(subset(PD, Strategy == "Sell")$Date, freq = TRUE, ylim = c(0,100), breaks = 30, xlab="Year", main="Date - Sell Strategy")
hist(subset(PD, Strategy == "Sell")$Date, plot = FALSE, breaks = 30)

hist(subset(PD, Strategy == "Buy")$MaxCorr5D, breaks = 20, ylim = c(0,700), xlab="% change", main="Max.correction in 5 days - Buy Strategy")
hist(subset(PD, Strategy == "Buy")$MaxCorr5D, plot = FALSE, breaks = 20)

hist(subset(PD, Strategy == "Sell")$MaxCorr5D, breaks = 20, ylim = c(0,850), xlab="% change", main="Max.correction in 5 days - Sell Strategy")
hist(subset(PD, Strategy == "Sell")$MaxCorr5D, plot = FALSE, breaks = 20)

hist(subset(PD, Strategy == "Buy")$MaxMoment5D, breaks = 30, ylim = c(0,400), xlab="% change", main="Max. continuity of the momentum in 5 days - Buy Strategy")
hist(subset(PD, Strategy == "Buy")$MaxMoment5D, plot = FALSE, breaks = 30)

hist(subset(PD, Strategy == "Sell")$MaxMoment5D, breaks = 30, ylim = c(0,900), xlab="% change", main="Max. continuity of the momentum in 5 days - Sell Strategy")
hist(subset(PD, Strategy == "Sell")$MaxMoment5D, plot = FALSE, breaks = 30)

hist(subset(PD, Strategy == "Buy")$MomentumCh, breaks = 20, ylim = c(0,450), xlab="% change", main="Momentum patron - Buy Strategy")
hist(subset(PD, Strategy == "Buy")$MomentumCh, plot = FALSE, breaks = 20)

hist(subset(PD, Strategy == "Sell")$MomentumCh, breaks = 50, ylim = c(0,350), xlab="% change", main="Momentum patron - Sell Strategy")
hist(subset(PD, Strategy == "Sell")$MomentumCh, plot = FALSE, breaks = 50)
	
hist(subset(PD, Strategy == "Buy" & MaxCorr5D < 0)$MaxCorr5D, plot = FALSE, breaks = 30)

hist(subset(PD, Strategy == "Sell" & MaxCorr5D > 0)$MaxCorr5D, plot = FALSE, breaks = 10)

# Standard deviations:

sd(PD$MaxCorr5D, na.rm = TRUE)
sd(PD$MaxMoment5D, na.rm = TRUE)
sd(PD$MomentumCh)

tapply(PD$MaxCorr5D, PD$Strategy, sd, na.rm=TRUE)
tapply(PD$MaxMoment5D, PD$Strategy, sd, na.rm=TRUE)
tapply(PD$MomentumCh, PD$Strategy, sd, na.rm=TRUE)

# Tables:

tail(sort(table(PD$Symbol)),20)
tail(sort(table(PD$Date)),30)

prop.table(table(subset(PD, Strategy == "Buy")$MaxCorr5D >= 0.0125))
prop.table(table(subset(PD, Strategy == "Sell")$MaxCorr5D <= -0.0125))

prop.table(table("XX Century" = subset(PD, Strategy == "Buy")$Date <= "1999-12-31"))
prop.table(table("XX Century" = subset(PD, Strategy == "Sell")$Date <= "1999-12-31"))

prop.table(table("Investment's signs" = subset(PD, Strategy == "Buy")$MaxCorr5D > 0, "XXI Century" = subset(PD, Strategy == "Buy")$Date >= "2000-01-01"), 2)
prop.table(table("Investment's signs" = subset(PD, Strategy == "Buy")$MaxCorr5D > 0.0125, "XXI Century" = subset(PD, Strategy == "Buy")$Date >= "2000-01-01"), 2)

prop.table(table("Investment's signs" = subset(PD, Strategy == "Sell")$MaxCorr5D < 0, "XXI Century" = subset(PD, Strategy == "Sell")$Date >= "2000-01-01"), 2)
prop.table(table("Investment's signs" = subset(PD, Strategy == "Sell")$MaxCorr5D < -0.0125, "XXI Century" = subset(PD, Strategy == "Sell")$Date >= "2000-01-01"), 2)

tail(sort(table(subset(PD, Strategy == "Buy")$Symbol)),20)
tail(sort(table(subset(PD, Strategy == "Sell")$Symbol)),20)

tail(sort(table(subset(PD, Strategy == "Buy")$Date)),20)
tail(sort(table(subset(PD, Strategy == "Sell")$Date)),20)

table("Strong Continuity" = subset(PD, Strategy == "Buy" & MaxCorr5D <= 0)$MaxMoment5D <= -0.1)/nrow(subset(PD, Strategy == "Buy"))
table("Strong Correction" = subset(PD, Strategy == "Buy" & MaxMoment5D >= 0)$MaxCorr5D >= 0.1)/nrow(subset(PD, Strategy == "Buy"))
table("Big Correction" = subset(PD, Strategy == "Buy" & MaxMoment5D < 0 & MaxCorr5D > 0)$MaxCorr5D >= 0.1, "Big Continuity" = subset(PD, Strategy == "Buy" & MaxMoment5D < 0 & MaxCorr5D > 0)$MaxMoment5D <= -0.1)/nrow(subset(PD, Strategy == "Buy"))

table("Strong Continuity" = subset(PD, Strategy == "Sell" & MaxCorr5D >= 0)$MaxMoment5D >= 0.1)/nrow(subset(PD, Strategy == "Sell"))
table("Strong Correction" = subset(PD, Strategy == "Sell" & MaxMoment5D <= 0)$MaxCorr5D <= -0.1)/nrow(subset(PD, Strategy == "Sell"))
table("Big Correction" = subset(PD, Strategy == "Sell" & MaxMoment5D > 0 & MaxCorr5D < 0)$MaxCorr5D <= -0.1, "Big Continuity" = subset(PD, Strategy == "Sell" & MaxMoment5D > 0 & MaxCorr5D < 0)$MaxMoment5D >= 0.1)/nrow(subset(PD, Strategy == "Sell"))

prop.table(table(subset(PD, Strategy == "Buy")$TotIS))
prop.table(table(subset(PD, Strategy == "Buy")$BuyIS))
prop.table(table(subset(PD, Strategy == "Buy")$SellIS))

prop.table(table(subset(PD, Strategy == "Sell")$TotIS))
prop.table(table(subset(PD, Strategy == "Sell")$BuyIS))
prop.table(table(subset(PD, Strategy == "Sell")$SellIS))

# Days in a trend:

tapply(PD$DT, PD$Strategy, summary)

tail(PD[order(PD$DT),c("Date","Symbol","DT")])

tapply(PD$MaxCorr10D, list(PD$DT , PD$Strategy), mean)

aggregate(MaxCorr10D ~ DT + Strategy, data = PD, mean)

table(PD$DT, PD$Strategy)

# % in a trend:

plot(PD$Strategy, PD$TCh, xlab = "Strategy", ylab = "Max. % change", main = "Max. % change in a trend (Strong)")

tapply(PD$TCh, PD$Strategy, summary)

# Days in a momentum:

table(PD$DM, PD$Strategy)

tapply(PD$DM, PD$Strategy, summary)

tail(PD[order(PD$DM),c("Date","Symbol","Strategy","DM")])

tapply(PD$MaxCorr10D, list((PD$DM >= 6), PD$Strategy), mean)

tapply(PD$MaxCorr10D, list(PD$DM , PD$Strategy), mean)

# % change of the momentum patron:

plot(PD$Strategy, PD$MomentumCh, xlab = "Strategy", ylab = "Max. % change", main = "Max. % change of the momentum patron")

tapply(PD$MomentumCh, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list(cut(PD$MomentumCh, breaks = seq(-1,1,0.1)), PD$Strategy), mean)

# The volume of today is greater than yesterdays volume: 

table(PD$VolUp, PD$Strategy)

tapply(PD$VolUp, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list(PD$VolUp, PD$Strategy), mean)

# Days with volume up: 

table(PD$DVolUp, PD$Strategy)

tapply(PD$DVolUp, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list((PD$DVolUp >= 3), PD$Strategy), mean)

tapply(PD$MaxCorr10D, list(PD$DVolUp, PD$Strategy), mean)
aggregate(MaxCorr10D ~ DVolUp + Strategy, data = PD, mean)

# Max. % change momentum correction in 10 days:

plot(PD$Strategy, PD$MaxCorr10D, xlab = "Strategy", ylab = "Max. % change", main = "Max. % change momentum correction in 10 days")

tapply(PD$MaxCorr10D, PD$Strategy, summary)

tail(PD[order(PD$Date),c("Date","Symbol","Strategy","MaxCorr10D")],35)

# Max. % change in the continuity of the momentum in 10 days:

plot(PD$Strategy, PD$MaxMoment10D, xlab = "Strategy", ylab = "Max. % change", main = "Max. % change in the continuity of the momentum in 10 days")

tapply(PD$MaxMoment10D, PD$Strategy, summary)

# S&P 500 Change during the last 5 days

plot(PD$Strategy, PD$ChngSP500D5, xlab = "Strategy", ylab = "% change", main = "S&P 500 Change during the last 5 days")

tapply(PD$ChngSP500D5, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list(cut(PD$ChngSP500D5, breaks = seq(-0.2,0.14,0.02)), PD$Strategy), mean)

# S&P 500 Change during the last days

plot(PD$Strategy, PD$ChngSP500, xlab = "Strategy", ylab = "% change", main = "S&P 500 Change during the last days")

tapply(PD$ChngSP500, PD$Strategy, summary)

# Momentum S&P 500:

tapply(PD$MomentumSP500, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list(PD$MomentumSP500, PD$Strategy), mean)
aggregate(MaxCorr10D ~ MomentumSP500 + Strategy, data = PD, mean)

tail(PD[order(PD$MomentumSP500),c("Date","Symbol","Strategy","MomentumSP500")],60)

# VIX Index Level:

plot(PD$Strategy, PD$VIX.Close, xlab = "Strategy", ylab = "VIX Index")

tapply(PD$VIX.Close, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list((PD$VIX.Close >= 30), PD$Strategy), mean)

tapply(PD$MaxCorr10D, list((PD$VIX.Close <= 20), PD$Strategy), mean)

table(PD$VIX.Close >= 30, PD$Strategy)

tapply(PD$MaxCorr10D, list(cut(PD$VIX.Close, breaks = c(0,10,20,30,40,50,60,70,81)), PD$Strategy), mean)

# VIX Change during the last 5 days

plot(PD$Strategy, PD$ChngVIXD5, xlab = "Strategy", ylab = "% change", main = "VIX Change during the last 5 days")

tapply(PD$ChngVIXD5, PD$Strategy, summary)

tail(PD[order(PD$ChngVIXD5),c("Date","Symbol","Strategy","ChngVIXD5")],5)

tapply(PD$MaxCorr10D, list(cut(PD$ChngVIXD5, breaks = seq(-0.4,0.7,0.05)), PD$Strategy), mean)

# VIX Change during the last days

plot(PD$Strategy, PD$ChngVIX, xlab = "Strategy", ylab = "% change", main = "VIX Change during the last days")

tapply(PD$ChngVIX, PD$Strategy, summary)

# Number of investment signals per day (buy, sell or total)

tapply(PD$TotIS, PD$Strategy, summary)
tapply(PD$BuyIS, PD$Strategy, summary)
tapply(PD$SellIS, PD$Strategy, summary)

tapply(PD$MaxCorr10D, list(PD$TotIS, PD$Strategy), mean)
tapply(PD$MaxCorr10D, list(PD$BuyIS, PD$Strategy), mean)
tapply(PD$MaxCorr10D, list(PD$SellIS, PD$Strategy), mean)

# Max & Min (outliers analysis):

tail(PD[order(PD$DT),c("Date","Symbol","Strategy","DT","MaxCorr5D","MaxMoment5D","MaxCorr10D","MaxMoment10D","ChngSP500")],15) # Profits

tapply(PD$TCh, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$TCh) # Profit
which.min(subset(PD, Strategy == "Buy")$TCh) # Profit

which.max(subset(PD, Strategy == "Sell")$TCh) # Profit
which.min(subset(PD, Strategy == "Sell")$TCh) # Loss

tail(PD[order(PD$DM),c("Date","Symbol","Strategy","DM","MaxCorr5D","MaxMoment5D","MaxCorr10D","MaxMoment10D","ChngSP500")],15) # Profits

tapply(PD$MomentumCh, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$MomentumCh) # Profit
which.min(subset(PD, Strategy == "Buy")$MomentumCh) # Profit

which.max(subset(PD, Strategy == "Sell")$MomentumCh) # Profit
which.min(subset(PD, Strategy == "Sell")$MomentumCh) # Loss

head(PD[order(PD$DVolUp),c("Date","Symbol","Strategy","DVolUp","MaxCorr5D","MaxMoment5D","MaxCorr10D","MaxMoment10D","ChngSP500")],20)

tapply(PD$MaxCorr10D, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$MaxCorr10D) # Profit
which.min(subset(PD, Strategy == "Buy")$MaxCorr10D) # Loss

which.max(subset(PD, Strategy == "Sell")$MaxCorr10D) # Loss
which.min(subset(PD, Strategy == "Sell")$MaxCorr10D) # Profit - Error in data

head(PD[order(PD$MaxCorr10D),c("Date","Symbol","Strategy","MaxCorr5D","MaxMoment5D","MaxCorr10D","MaxMoment10D","ChngSP500")],20)

tapply(PD$MaxMoment10D, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$MaxMoment10D) # Profit
which.min(subset(PD, Strategy == "Buy")$MaxMoment10D) # Loss

which.max(subset(PD, Strategy == "Sell")$MaxMoment10D) # Loss
which.min(subset(PD, Strategy == "Sell")$MaxMoment10D) # Profit

tapply(PD$ChngSP500D5, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$ChngSP500D5) # Profit
which.min(subset(PD, Strategy == "Buy")$ChngSP500D5) # Loss

which.max(subset(PD, Strategy == "Sell")$ChngSP500D5) # Profit
which.min(subset(PD, Strategy == "Sell")$ChngSP500D5) # Profit

tapply(PD$ChngSP500, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$ChngSP500) # Profit
which.min(subset(PD, Strategy == "Buy")$ChngSP500) # Loss

which.max(subset(PD, Strategy == "Sell")$ChngSP500) # Profit
which.min(subset(PD, Strategy == "Sell")$ChngSP500) # Profit

tapply(PD$VIX.Close, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$VIX.Close) # Loss
which.min(subset(PD, Strategy == "Buy")$VIX.Close) # Profit

which.max(subset(PD, Strategy == "Sell")$VIX.Close) # Profit
which.min(subset(PD, Strategy == "Sell")$VIX.Close) # Profit

tapply(PD$ChngVIXD5, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$ChngVIXD5) # Profit
which.min(subset(PD, Strategy == "Buy")$ChngVIXD5) # Profit

which.max(subset(PD, Strategy == "Sell")$ChngVIXD5) # Profit
which.min(subset(PD, Strategy == "Sell")$ChngVIXD5) # Profit

tapply(PD$ChngVIX, PD$Strategy, summary)

which.max(subset(PD, Strategy == "Buy")$ChngVIX) # Profit
which.min(subset(PD, Strategy == "Buy")$ChngVIX) # Profit

which.max(subset(PD, Strategy == "Sell")$ChngVIX) # Profit
which.min(subset(PD, Strategy == "Sell")$ChngVIX) # Profit

# Plotting:

# Scatterplots

plot(subset(PD, Strategy == "Buy")$TCh, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "% change in a trend", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$TCh, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "% change in a trend", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500D5, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500D5, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "S&P 500 Change during the last days", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "S&P 500 Change during the last days", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MarkCapt, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "Market Capitalization", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$MarkCapt, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "Market Capitalization", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$TCh, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "% change in a trend", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = -0.17, col = "red")

plot(subset(PD, Strategy == "Sell")$TCh, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = "% change in a trend", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500D5, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500D5, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "S&P 500 Change during the last days", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = "S&P 500 Change during the last days", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MarkCapt, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "Market Capitalization", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$MarkCapt, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = "Market Capitalization", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$TCh, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "% change in a trend", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$TCh, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "% change in a trend", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MomentumCh, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "% change of the momentum patron", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$MomentumCh, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "% change of the momentum patron", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500D5, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500D5, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "S&P 500 Change during the last days", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "S&P 500 Change during the last days", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MarkCapt, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "Market Capitalization", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$MarkCapt, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "Market Capitalization", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$TCh, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "% change in a trend", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$TCh, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "% change in a trend", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MomentumCh, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "% change of the momentum patron", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$MomentumCh, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "% change of the momentum patron", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500D5, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500D5, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "S&P 500 Change during the last 5 days", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "S&P 500 Change during the last days", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "S&P 500 Change during the last days", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MarkCapt, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "Market Capitalization", ylab = "% change in the Max. continuity of the momentum", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$MarkCapt, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "Market Capitalization", ylab = "% change in the Max. continuity of the momentum", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MaxMoment10D, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "% change in the Max. continuity of the momentum", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = -0.1, col = "red")
abline(h = 0.1, col = "red")

plot(subset(PD, Strategy == "Sell")$MaxMoment10D, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "% change in the Max. continuity of the momentum", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")
abline(v = 0.1, col = "red")
abline(h = -0.1, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$TCh, xlab = "Date", ylab = "% change in a trend", main = "Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = -0.2, col = "blue")
abline(h = -0.1, col = "red")

plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$TCh, xlab = "Date", ylab = "% change in a trend", main = "Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.2, col = "blue")
abline(h = 0.1, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$ChngSP500D5, xlab = "Date", ylab = "S&P 500 Change during the last 5 days", main = "Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = -0.2, col = "blue")
abline(h = -0.1, col = "red")

plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$ChngSP500D5, xlab = "Date", ylab = "S&P 500 Change during the last 5 days", main = "Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.2, col = "blue")
abline(h = 0.1, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$ChngSP500, xlab = "Date", ylab = "S&P 500 Change during the last days", main = "Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = -0.2, col = "blue")
abline(h = -0.1, col = "red")

plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$ChngSP500, xlab = "Date", ylab = "S&P 500 Change during the last days", main = "Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.2, col = "blue")
abline(h = 0.1, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$MarkCapt, xlab = "Date", ylab = "Market Capitalization", main = "Buy Strategy")
abline(h = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$MarkCapt, xlab = "Date", ylab = "Market Capitalization", main = "Sell Strategy")
abline(h = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "Date", ylab = "% change", main = "Max.correction in 10 days - Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")
abline(h = 0.0125, col = "red")

plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "Date", ylab = "% change", main = "Max.correction in 10 days - Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")
abline(h = -0.0125, col = "red")

plot(subset(PD, Strategy == "Buy")$Date, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "Date", ylab = "% change", main = "Max. continuity of the momentum in 10 days - Buy Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")

plot(subset(PD, Strategy == "Sell")$Date, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "Date", ylab = "% change", main = "Max. continuity of the momentum in 10 days - Sell Strategy")
abline(h = 0, col = "gray60")
abline(h = 0.1, col = "blue")
abline(h = -0.1, col = "blue")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr5D ~ subset(PD, Strategy == "Buy")$DT, outline = FALSE, xlab = "Days in a trend", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr5D ~ subset(PD, Strategy == "Sell")$DT, xlab = "Days in a trend", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment5D ~ subset(PD, Strategy == "Buy")$DT, xlab = "Days in a trend", ylab = "% change of the Max. continuity", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment5D ~ subset(PD, Strategy == "Sell")$DT, xlab = "Days in a trend", ylab = "% change of the Max. continuity", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$DT, xlab = "Days in a trend", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$DT, xlab = "Days in a trend", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$DT, xlab = "Days in a trend", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$DT, xlab = "Days in a trend", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr5D ~ subset(PD, Strategy == "Buy")$DM, xlab = "Days in a momentum", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr5D ~ subset(PD, Strategy == "Sell")$DM, xlab = "Days in a momentum", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment5D ~ subset(PD, Strategy == "Buy")$DM, xlab = "Days in a momentum", ylab = "% change of the Max. continuity", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment5D ~ subset(PD, Strategy == "Sell")$DM, xlab = "Days in a momentum", ylab = "% change of the Max. continuity", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$DM, xlab = "Days in a momentum", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$DM, xlab = "Days in a momentum", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$DM, xlab = "Days in a momentum", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$DM, xlab = "Days in a momentum", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr5D ~ subset(PD, Strategy == "Buy")$VolUp, xlab = "Volume up", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr5D ~ subset(PD, Strategy == "Sell")$VolUp, xlab = "Volume up", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment5D ~ subset(PD, Strategy == "Buy")$VolUp, xlab = "Volume up", ylab = "% change of the Max. continuity", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment5D ~ subset(PD, Strategy == "Sell")$VolUp, xlab = "Volume up", ylab = "% change of the Max. continuity", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$VolUp, xlab = "Volume up", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$VolUp, xlab = "Volume up", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$VolUp, xlab = "Volume up", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$VolUp, xlab = "Volume up", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr5D ~ subset(PD, Strategy == "Buy")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr5D ~ subset(PD, Strategy == "Sell")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment5D ~ subset(PD, Strategy == "Buy")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max. continuity", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment5D ~ subset(PD, Strategy == "Sell")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max. continuity", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$DVolUp, xlab = "Days with volume up", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr5D ~ subset(PD, Strategy == "Buy")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr5D ~ subset(PD, Strategy == "Sell")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment5D ~ subset(PD, Strategy == "Buy")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max. continuity", main = "Buy Strategy - 5 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment5D ~ subset(PD, Strategy == "Sell")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max. continuity", main = "Sell Strategy - 5 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$MomentumSP500, xlab = "Momentum S&P 500", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$TotIS, xlab = "Total IS", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$TotIS, xlab = "Total IS", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$TotIS, xlab = "Total IS", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$TotIS, xlab = "Total IS", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$BuyIS, xlab = "Total Buy IS", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$BuyIS, xlab = "Total Buy IS", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$BuyIS, xlab = "Total Buy IS", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$BuyIS, xlab = "Total Buy IS", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxCorr10D ~ subset(PD, Strategy == "Buy")$SellIS, xlab = "Total Sell IS", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxCorr10D ~ subset(PD, Strategy == "Sell")$SellIS, xlab = "Total Sell IS", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")

boxplot(subset(PD, Strategy == "Buy")$MaxMoment10D ~ subset(PD, Strategy == "Buy")$SellIS, xlab = "Total Sell IS", ylab = "% change of the Max. continuity", main = "Buy Strategy - 10 days")
boxplot(subset(PD, Strategy == "Sell")$MaxMoment10D ~ subset(PD, Strategy == "Sell")$SellIS, xlab = "Total Sell IS", ylab = "% change of the Max. continuity", main = "Sell Strategy - 10 days")

plot(subset(PD, Strategy == "Buy")$VIX.Close, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "VIX Index", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$VIX.Close, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = "VIX Index", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIXD5, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "% change VIX Index 5 days", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIXD5, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = " % change VIX Index 5 days", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIX, subset(PD, Strategy == "Buy")$MaxCorr5D, xlab = "% change VIX Index", ylab = "% change of the Max.correction", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIX, subset(PD, Strategy == "Sell")$MaxCorr5D, xlab = " % change VIX Index", ylab = "% change of the Max.correction", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$VIX.Close, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "VIX Index", ylab = "% change of the Max.continuity", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$VIX.Close, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = "VIX Index", ylab = "% change of the Max.continuity", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIXD5, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "% change VIX Index 5 days", ylab = "% change of the Max.continuity", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIXD5, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = " % change VIX Index 5 days", ylab = "% change of the Max.continuity", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIX, subset(PD, Strategy == "Buy")$MaxMoment5D, xlab = "% change VIX Index", ylab = "% change of the Max.continuity", main = "Buy Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIX, subset(PD, Strategy == "Sell")$MaxMoment5D, xlab = " % change VIX Index", ylab = "% change of the Max.continuity", main = "Sell Strategy - 5 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$VIX.Close, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "VIX Index", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$VIX.Close, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "VIX Index", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIXD5, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "% change VIX Index 5 days", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIXD5, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = " % change VIX Index 5 days", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIX, subset(PD, Strategy == "Buy")$MaxCorr10D, xlab = "% change VIX Index", ylab = "% change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIX, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = " % change VIX Index", ylab = "% change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$VIX.Close, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "VIX Index", ylab = "% change of the Max.continuity", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$VIX.Close, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = "VIX Index", ylab = "% change of the Max.continuity", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIXD5, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "% change VIX Index 5 days", ylab = "% change of the Max.continuity", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIXD5, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = " % change VIX Index 5 days", ylab = "% change of the Max.continuity", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngVIX, subset(PD, Strategy == "Buy")$MaxMoment10D, xlab = "% change VIX Index", ylab = "% change of the Max.continuity", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngVIX, subset(PD, Strategy == "Sell")$MaxMoment10D, xlab = " % change VIX Index", ylab = "% change of the Max.continuity", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$MomentumCh, subset(PD, Strategy == "Buy")$LogMaxCorr10D, xlab = "% change in a trend", ylab = "Log % change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(log(subset(PD, Strategy == "Sell")$MomentumCh + 1), subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "% change in a trend", ylab = "Log % change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Buy")$ChngSP500D5, subset(PD, Strategy == "Buy")$LogMaxCorr10D, xlab = "S&P 500 Change during the last 5 days", ylab = "Log % change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(subset(PD, Strategy == "Sell")$ChngSP500D5, subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "S&P 500 Change during the last 5 days", ylab = "Log % change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(log(subset(PD, Strategy == "Buy")$VIX.Close), subset(PD, Strategy == "Buy")$LogMaxCorr10D, xlab = "VIX Index", ylab = "Log % change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(log(subset(PD, Strategy == "Sell")$VIX.Close), subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = "VIX Index", ylab = "Log % change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(log(subset(PD, Strategy == "Buy")$ChngVIXD5 + 1), subset(PD, Strategy == "Buy")$LogMaxCorr10D, xlab = "% change VIX Index 5 days", ylab = "Log % change of the Max.correction", main = "Buy Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

plot(log(subset(PD, Strategy == "Sell")$ChngVIXD5 + 1), subset(PD, Strategy == "Sell")$MaxCorr10D, xlab = " % change VIX Index 5 days", ylab = "Log % change of the Max.correction", main = "Sell Strategy - 10 days")
abline(h = 0, v = 0, col = "gray60")

# Histograms

hist(subset(PD, Strategy == "Buy")$MaxCorr10D, breaks = 20, ylim = c(0,600), xlab="% change", main="Max.correction in 10 days - Buy Strategy")

hist(subset(PD, Strategy == "Sell")$MaxCorr10D, breaks = 20, ylim = c(0,300), xlab="% change", main="Max.correction in 10 days - Sell Strategy")

hist(subset(PD, Strategy == "Buy")$LogMaxCorr10D, breaks = 20, ylim = c(0,600), xlab="% change", main="Max.correction in 10 days - Buy Strategy")

hist(subset(PD, Strategy == "Sell")$LogMaxCorr10D, breaks = 20, ylim = c(0,300), xlab="% change", main="Max.correction in 10 days - Sell Strategy")

hist(subset(PD, Strategy == "Buy")$MomentumCh, breaks = 20, ylim = c(0,200), xlab="% change", main="Trend Change - Buy Strategy")

hist(subset(PD, Strategy == "Sell")$MomentumCh, breaks = 20, ylim = c(0,350), xlab="% change", main="Trend Change - Sell Strategy")

hist(log(subset(PD, Strategy == "Buy")$MomentumCh + 1), breaks = 20, ylim = c(0,400), xlab="% change", main="Trend Change - Buy Strategy")

hist(log(subset(PD, Strategy == "Sell")$MomentumCh + 1), breaks = 20, ylim = c(0,350), xlab="% change", main="Trend Change - Sell Strategy")

hist(subset(PD, Strategy == "Buy")$ChngSP500D5, breaks = 20, ylim = c(0,400), xlab="% change", main="S&P Change in 5 days - Buy Strategy")

hist(subset(PD, Strategy == "Sell")$ChngSP500D5, breaks = 20, ylim = c(0,350), xlab="% change", main="S&P Change - Sell Strategy")

hist(subset(PD, Strategy == "Buy")$VIX.Close, breaks = 20, ylim = c(0,350), xlab="% change", main="VIX Close - Buy Strategy")

hist(log(subset(PD, Strategy == "Buy")$VIX.Close), breaks = 20, ylim = c(0,200), xlab="% change", main="VIX Close - Buy Strategy")

hist(subset(PD, Strategy == "Sell")$VIX.Close, breaks = 20, ylim = c(0,250), xlab="% change", main="VIX Close - Sell Strategy")

hist(log(subset(PD, Strategy == "Sell")$VIX.Close), breaks = 20, ylim = c(0,200), xlab="% change", main="VIX Close - Sell Strategy")

hist(subset(PD, Strategy == "Buy")$ChngVIXD5, breaks = 20, ylim = c(0,300), xlab="% change", main="VIX Change in 5 days - Buy Strategy")

hist(log(subset(PD, Strategy == "Buy")$ChngVIXD5 + 1), breaks = 20, ylim = c(0,350), xlab="% change", main="VIX Change in 5 days - Buy Strategy")

hist(subset(PD, Strategy == "Sell")$ChngVIXD5, breaks = 20, ylim = c(0,300), xlab="% change", main="VIX Change in 5 days - Sell Strategy")

hist(log(subset(PD, Strategy == "Sell")$ChngVIXD5 + 1), breaks = 20, ylim = c(0,300), xlab="% change", main="VIX Change in 5 days - Sell Strategy")

# Month:

sort(table(PD$Month))
prop.table(table(PD$Month, PD$Strategy),1)
sort(table(subset(PD, Strategy == "Buy")$Month))
sort(table(subset(PD, Strategy == "Sell")$Month))

tapply(PD$MaxCorr10D, list(PD$Month, PD$Strategy), mean)

# Note: Analyze the tax-loss selling effect

# Weekday:

sort(table(PD$Weekday))
prop.table(table(PD$Weekday, PD$Strategy),1)
sort(table(subset(PD, Strategy == "Buy")$Weekday))
sort(table(subset(PD, Strategy == "Sell")$Weekday))

# Quarter:

sort(table(PD$Quarter))
prop.table(table(PD$Quarter, PD$Strategy),1)
sort(table(subset(PD, Strategy == "Buy")$Quarter))
sort(table(subset(PD, Strategy == "Sell")$Quarter))

# Boxplots for the Binary variable Max. correction of 5%:

boxplot(subset(PD, Strategy == "Buy")$DT ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Days in a trend", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$DT ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Days in a trend", main = "Sell Strategy - Corr >= 5%")

boxplot(subset(PD, Strategy == "Buy")$TCh ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change in a trend", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$TCh ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change in a trend", main = "Sell Strategy - Corr >= 5%")

boxplot(subset(PD, Strategy == "Buy")$DM ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Days in the momentum patron", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$DM ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Days in the momentum patron", main = "Sell Strategy - Corr >= 5%")

table(subset(PD, Strategy == "Sell")$DM, subset(PD, Strategy == "Sell")$MaxCorr5Per)

boxplot(subset(PD, Strategy == "Buy")$MomentumCh ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change in the momentum patron", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$MomentumCh ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change in the momentum patron", main = "Sell Strategy - Corr >= 5%")

boxplot(log(subset(PD, Strategy == "Buy")$MarkCapt) ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Market Capitalization", main = "Buy Strategy - Corr >= 5%")
boxplot(log(subset(PD, Strategy == "Sell")$MarkCapt) ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Market Capitalization", main = "Sell Strategy - Corr >= 5%")

boxplot(subset(PD, Strategy == "Buy")$ChngSP500D5 ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change S&P 500 in 5 days", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$ChngSP500D5 ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change S&P 500 in 5 days", main = "Sell Strategy - Corr >= 5%")

boxplot(log(subset(PD, Strategy == "Buy")$VIX.Close) ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "VIX close", main = "Buy Strategy - Corr >= 5%")
boxplot(log(subset(PD, Strategy == "Sell")$VIX.Close) ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "VIX close", main = "Sell Strategy - Corr >= 5%")

boxplot(log(subset(PD, Strategy == "Buy")$ChngVIXD5 + 1) ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change VIX index in 5 days", main = "Buy Strategy - Corr >= 5%")
boxplot(log(subset(PD, Strategy == "Sell")$ChngVIXD5 + 1) ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "% Change VIX index in 5 days", main = "Sell Strategy - Corr >= 5%")

boxplot(subset(PD, Strategy == "Buy")$TotIS ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Total invesment signs", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$TotIS ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Total invesment signs", main = "Sell Strategy - Corr >= 5%")

table(subset(PD, Strategy == "Sell")$TotIS, subset(PD, Strategy == "Sell")$MaxCorr5Per)

boxplot(subset(PD, Strategy == "Buy")$BuyIS ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Total buy invesment signs", main = "Buy Strategy - Corr >= 5%")
boxplot(subset(PD, Strategy == "Sell")$BuyIS ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Total buy invesment signs", main = "Sell Strategy - Corr >= 5%")

boxplot(subset(PD, Strategy == "Buy")$SellIS ~ subset(PD, Strategy == "Buy")$MaxCorr5Per, outline = TRUE, xlab = "Max. correction greater than 5%", ylab = "Total sell invesment signs", main = "Buy Strategy - Corr >= 5%")
table(subset(PD, Strategy == "Buy")$SellIS, subset(PD, Strategy == "Buy")$MaxCorr5Per)
boxplot(subset(PD, Strategy == "Sell")$SellIS ~ subset(PD, Strategy == "Sell")$MaxCorr5Per, outline = FALSE, xlab = "Max. correction greater than 5%", ylab = "Total sell invesment signs", main = "Sell Strategy - Corr >= 5%")

# Interaction between DVolUp and Weekday
  
MSDW <- aggregate(MaxCorr10D ~ Strategy + DVolUp + Weekday, data = PD, mean)
Tab1 <- xtabs(MaxCorr10D ~ Strategy + DVolUp + Weekday, data = MSDW)
round(ftable(Tab1)*100,2)

round(tapply(PD$MaxCorr10D, list(PD$DVolUp, PD$Weekday, PD$Strategy), mean)*100,2)

# Interaction between DM and Weekday

MSDW2 <- aggregate(MaxCorr10D ~ Strategy + DM + Weekday, data = PD, mean)
Tab2 <- xtabs(MaxCorr10D ~ Strategy + DM + Weekday, data = MSDW2)
round(ftable(Tab2)*100,2)

# Interaction between DM and VolUp

MSDV <- aggregate(MaxCorr10D ~ Strategy + DM + VolUp, data = PD, mean)
Tab3 <- xtabs(MaxCorr10D ~ Strategy + DM + VolUp, data = MSDV)
round(ftable(Tab3)*100,2)

# Interaction between DM and DVolUp

MSDD <- aggregate(MaxCorr10D ~ Strategy + DM + DVolUp, data = PD, mean)
Tab4 <- xtabs(MaxCorr10D ~ Strategy + DM + DVolUp, data = MSDD)
round(ftable(Tab4)*100,2)

round(tapply(PD$MaxCorr10D, list(PD$DM, PD$DVolUp, PD$Strategy), mean)*100,2)

# Interaction between MomentumCh and VIX.Close

tapply(PD$MaxCorr10D, list(cut(PD$MomentumCh, breaks = seq(-1,1,0.1)), cut(PD$VIX.Close, breaks = c(0,10,20,30,40,50,60,70,81)), PD$Strategy), mean)

# Interaction between ChngSP500D5 and ChngVIXD5

round(tapply(PD$MaxCorr10D, list(cut(PD$ChngSP500D5, breaks = seq(-0.2,0.14,0.02)), cut(PD$ChngVIXD5, breaks = seq(-0.4,0.7,0.1)), PD$Strategy), mean)*100, 2)
