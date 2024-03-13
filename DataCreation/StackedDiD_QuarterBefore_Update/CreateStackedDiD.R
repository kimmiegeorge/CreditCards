# Create Stacked DiD Data - One Quarter Before Control 

#=====================
# Set up 
#=====================
library(pacman)
p_load(data.table, lfe, dplyr)

# directories 
data_dir <- "~/Dropbox/CreditCards/Data/FullSampleAgg/"
data_dir2 <- "~/Dropbox/CreditCards/Data/"
output_dir <- "~/Dropbox/CreditCards/Data/DiD_QuarterBeforeControl_Update/"

#=====================
# Load data 
#=====================
# load deposit data 
fname <- paste0(data_dir, "full_dta.RDS")
data <- readRDS(fname)

# load EA Data
fname <- paste0(data_dir2, "SampleEAData_MonthQuarters.csv")
ea_month_data <- fread(fname)
ea_month_data[, YearMonth := paste0(as.character(year), "_", as.character(month))]
ea_data <- unique(ea_month_data[, .(TICKER, EA_ID, datadate)])
ea_data <- ea_data[order(TICKER, EA_ID)]
# get lag EA_ID within TICKER 
ea_data[, lagEA_ID := lag(EA_ID, ), by = (TICKER)]

# merge with suspect 
suspect <- unique(data[, .(MERCHANT_TICKER, EA_ID, Suspect)])
setnames(suspect, "MERCHANT_TICKER", "TICKER")
setkey(ea_data, TICKER, EA_ID)
setkey(suspect, TICKER, EA_ID)
ea_data <- suspect[ea_data]
ea_data <- ea_data[!is.na(Suspect)]

# merge ea month data with suspect
setkey(ea_month_data, TICKER, EA_ID)
setkey(suspect, TICKER, EA_ID)
ea_month_data <- suspect[ea_month_data]

# merge with previous quarter suspect 
setnames(suspect, "Suspect", "PrevQuarterSuspect")
setkey(suspect, TICKER, EA_ID)
setkey(ea_data, TICKER, lagEA_ID)
ea_data <- suspect[ea_data]
setnames(ea_data, "EA_ID", "lagEA_ID")
setnames(ea_data, "i.EA_ID", "EA_ID")




#=====================
# Determine treat and control observation months 
#=====================
treat_obs <- ea_data[Suspect == 1 & PrevQuarterSuspect == 0]

treat_control_months <- data.table()


# loop through treat obs and determine control obs 
for (i in seq_len(nrow(treat_obs))){
  # pull values 
  tic <- treat_obs[i, TICKER]
  CurrentEA <- treat_obs[i, EA_ID]
  PrevEA <- treat_obs[i, lagEA_ID]
  # check if missing and go to next row if so
  if (is.na(PrevEA)) next
  
  # now determine year month values to include in potential control obs pull 
  yms <- unique(ea_month_data[TICKER == tic & EA_ID %in% c(CurrentEA, PrevEA)]$YearMonth)
  post_yms <- unique(ea_month_data[TICKER == tic & EA_ID == CurrentEA]$YearMonth)
  
  # treat obs 
  treat <- ea_month_data[TICKER == tic & EA_ID %in% c(CurrentEA, PrevEA)]
  treat[, Treat := 1]
  treat[, Post := ifelse(EA_ID == CurrentEA, 1, 0)]
  treat[, Event := paste0(tic, "_", CurrentEA)]

  # control obs 
  # filter to other firms, and year months in yms
  control <- ea_month_data[TICKER != tic & YearMonth %in% yms]
  # filter to firms with zero suspect months in this time period 
  control[, TotalSuspect := sum(Suspect), .(TICKER)]
  control <- control[TotalSuspect == 0]
  control[, TotalSuspect := NULL]
  control[, Treat := 0]
  control[, Post := ifelse(YearMonth %in% post_yms, 1, 0)]
  control[, Event := paste0(tic, "_", CurrentEA)]
  
  treat_control <- rbind(treat, control)
  treat_control[, V1 := NULL]
  
  treat_control_months <- rbind(treat_control_months, treat_control)
  
  # update 
  if (i %% 10 == 0) {
    cat("On observation", i, "out of", nrow(treat_obs), "/n")
  }
  
}

#=====================
# Create full sample 
#=====================

events <- unique(treat_control_months$Event)

# prep data
data[, YearMonth := paste0(as.character(year), "_", as.character(month))]
setnames(data, "MERCHANT_TICKER", "TICKER")
setkey(data, TICKER, YearMonth)

# prep for loop 
count = 0


 #loop 
for (e in events){
  
  file <- paste0("DiD_", e, ".csv")
  filename <- paste0(data_dir2, "DiD_QuarterBeforeControl_Update/", file)
  
  # count
  count = count + 1
  
  # pull month observations for event 
  months <- treat_control_months[Event == e]
  setkey(months, TICKER, YearMonth)
  months <- months[, .(TICKER, YearMonth, Event, Treat, Post)]
  
  # merge with employee deposits data 
  months_emp <- data[months]
  
 
  write.csv(months_emp, filename)
 
}
