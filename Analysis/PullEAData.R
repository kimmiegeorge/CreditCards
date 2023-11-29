# Pull earnings announcement and earnings surprise variables for sample firms 

ea <- readRDS("~/Dropbox/MainDataFiles/Compustat/ProcessedData/EAData.RDS")
ea <- ea[fyearq %in% 2012:2021]

 #list of tickers 
tickers <- c('KR', 'SQ', 'SBUX', 'MAR', 'JCP', 'AAPL', 'FDX', 'AAP', 'FB',
            'QSR', 'MGI', 'BKS', 'SHLDQ', 'AMZN', 'CVX', 'DENN', 'JBLU', 'HD',
            'IMKTA', 'DIN', 'WTW', 'IHG', 'PBI', 'GPS', 'CUBE', 'FISV', 'UBER',
            'LYFT', 'MIK', 'ARMK', 'BOBE', 'ACI', 'GOOGL', 'ROST', 'SFM',
            'CVS', 'RGC', 'DG', 'WU', 'DRI', 'APRN', 'IAC', 'MPC', 'DLTR',
            'RNDY', 'KKR', 'PGR', 'FNF', 'PBPB', 'MW', 'CR', 'HQY', 'S', 'WFM',
            'NDLS', 'TLRD', 'DIS', 'ALL', 'SAVE', 'HTSI', 'FIVE', 'POST',
            'ALK', 'NFLX', 'TACO', 'WH', 'EFX', 'TSO', 'BBW', 'GM', 'T',
            'PYPL', 'TCS', 'VG', 'RDSB', 'WTRH', 'CINE', 'HA', 'ANGI', 'SNAP',
            'BMO', 'AMX A', 'ATVI', 'LOCK', 'LQ', 'CAR', 'GDOT', 'TRU', 'EBAY',
            'LYV', 'OLLI', 'LUB', 'USAT', 'OFG', 'INTU', 'NWSA', 'FCNCA',
            'MCK', 'CHH', 'CNNE', 'INDB', 'TXRH', 'EIGI', 'EVLV', 'ZU', 'VLO',
            'EXPE', 'SUN', 'MSFT', 'TMUS', 'BBY', 'ANDV', 'USB', 'ULVR',
            'SYMC', 'CHSCP', 'BX', 'QVCA', 'SABR', 'HSNi', 'ATD.B', 'JWN',
            'TGT', 'DISH', 'MTCH', 'NLOK', 'BRK.B', 'ETSY', 'ROKU', 'QRTEA',
            'GOGO', 'FTRCQ', 'WMT', 'NESN',  'WBA', 'WAG', 'RAD', 'BH')

# pull crsp for merge with compustat
crsp <- readRDS("~/Dropbox/MainDataFiles/CRSP/Processed/CRSP_2000_Jan2022.RDS")
crsp <- crsp[, .(PERMNO, date, TICKER)]
crsp <- crsp[TICKER %in% tickers]

# merge 
setkey(ea, PERMNO, datadate)
crsp[, date := as.Date(as.character(date), format = "%Y%m%d")]
setkey(crsp, PERMNO,  date)

ea <- crsp[ea, roll = T]
setnames(ea, "date", "datadate")

ea <- ea[!is.na(TICKER)]

# UPDATE ANALYST SUE 

ea[, AnalystBasedSurprise := NULL]
ea[, AnalystSUE := NULL]

### Get median estimate and actual value from IBES ### 

# load ibes detail file to get dispersion
ibesDetail <- fread('unzip -p /Users/kimmiegeorge/Dropbox/MainDataFiles/Compustat/RawData/IBESDetail.zip', fill = T)

# use REVDATE - this is most up to date 
ibesDetail[, ForecastDate := as.Date(as.character(REVDATS), format = '%Y-%m-%d')]

# fpe date to date 
ibesDetail[, FPEDATS := as.Date(as.character(FPEDATS), format = '%Y-%m-%d')]

# merge to get rdq 
ibesDetail <- ibesDetail[CUSIP %in% ea$NCUSIP]
setkey(ibesDetail, CUSIP, FPEDATS)
ea_merge <- unique(ea[, .(NCUSIP, PERMNO, rdq, datadate)])
setkey(ea_merge, NCUSIP, datadate)
ibesDetail <- ea_merge[ibesDetail]
ibesDetail <- ibesDetail[!is.na(rdq)]

# only keep forecasts within 90 days of the announcement, and keep last forecast for each analyst 
ibesDetail[, Diff := as.numeric(as.Date(rdq) - as.Date(ForecastDate))]
ibesDetail <- ibesDetail[Diff %in% c(1:91)]
# keep last for each analyst 
ibesDetail <- ibesDetail[order(ANALYS, NCUSIP, rdq, -ForecastDate)]
ibesDetail <- ibesDetail[, head(.SD, 1), .(ANALYS, NCUSIP, PERMNO, rdq)]

# compute median estimate and pull actual 
ibesSummary <- ibesDetail[, list(MedianEstimate = median(VALUE), 
                                 Actual = ACTUAL[1]), .(NCUSIP, PERMNO, rdq, datadate)]

### Get end of previous quarter price ### 
ea_merge <- unique(ea[, .(PERMNO, rdq, datadate)])


# load crsp 
crsp <- readRDS('~/Dropbox/MainDataFiles/CRSP/Processed/CRSPDaily_2000_2022.RDS')
crsp <- crsp[, .(PERMNO, date, PRC)]
crsp <- crsp[!is.na(PRC)]
#setnames(crsp, 'PERMNO', 'permno')
setkey(crsp, PERMNO, date)
setkey(ea_merge, PERMNO, datadate)
ea_merge <- crsp[ea_merge, roll = T]

#### Merge back with ibesSummary#### 
ibesSummary <- ea_merge[, .(PERMNO, rdq, PRC)][ibesSummary, on = .(PERMNO, rdq)]
ibesSummary[, AnalystSUE := (Actual-MedianEstimate)/PRC]
ibesSummary[, AnalystSUEDiff := (Actual-MedianEstimate)]

###### add back to earnings file 
ea <- ibesSummary[, .(PERMNO, rdq, AnalystSUE, AnalystSUEDiff, PRC)][ea, on = .(PERMNO, rdq)]

# add industry
ind <- readRDS("~/Dropbox/MainDataFiles/Compustat/ProcessedData/CompQuarterlyPERMNO_1990_2022.RDS")


write.csv(ea, "/Users/kimmiegeorge/Dropbox/CreditCards/Data/SampleEAData.csv")


