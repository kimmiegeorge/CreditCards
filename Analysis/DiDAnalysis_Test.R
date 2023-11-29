dta_dir <- '~/Dropbox/CreditCards/Data/DiD_Filter50'
files <- list.files(dta_dir)

dta <- data.table()

for (f in files){
  file <- fread(paste0(dta_dir, '/', f))
  dta <- rbind(dta, file)
}

ea <- fread("~/Dropbox/CreditCards/Data/SampleEAData.csv")
rm(file)
rm(files)

dta[, UserEvent := paste0(ARM_ID_NBR, "_", EventEA_ID)]
dta[, MonthEvent := paste0(as.character(MonthID), "_", EventEA_ID)]
dta[, TickerMonth := paste0(MERCHANT_TICKER, "_", as.character(MonthID))]


# Wins
dta[, DepositsWin := Winsorize(MonthlyDeposits, probs = c(0.01, 0.99))]
dta[, logDeposits := log(MonthlyDeposits)]
dta[, logDepositsWin := Winsorize(logDeposits, probs = c(0.01, 0.99))]

### XS - pre period average monthly deposit within firm 
dta[, PrePeriodAvgDeposit := mean(MonthlyDeposits[POST == 0]), .(ARM_ID_NBR, MERCHANT_TICKER, EventEA_ID)]
dta[, MedianDeposit := median(PrePeriodAvgDeposit), .(MERCHANT_TICKER, EventEA_ID)]

sub <- dta[MonthID >= 22]
sub[, CountMonths := uniqueN(MonthID), .(EventEA_ID)]
sub <- sub[CountMonths == 6]

dta[, NumFirms := uniqueN(MERCHANT_TICKER), .(EventEA_ID)]
sub2 <- dta[NumFirms > 2]
sub3 <- dta[NumFirms > 3]

# log adj

reg_full <- felm(logDeposits ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = dta)
summary(reg_full)

reg_sub <- felm(logDeposits ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = sub)
summary(reg_sub)

reg_sub2 <- felm(logDeposits ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = sub2)
summary(reg_sub2)

reg_sub3 <- felm(logDeposits ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = sub3)
summary(reg_sub3)

# log adj and wins

wreg_full <- felm(logDepositsWin ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = dta)
summary(wreg_full)

wreg_sub <- felm(logDepositsWin ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = sub)
summary(reg_sub)

wreg_sub2 <- felm(logDepositsWin ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = sub2)
summary(reg_sub2)

wreg_sub3 <- felm(logDepositsWin ~ POST:Treated|UserEvent+MonthEvent|0|TickerMonth, data = sub3)
summary(reg_sub3)





