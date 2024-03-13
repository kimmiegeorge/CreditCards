# Create Stacked DiD Data - One Quarter Before Control - Bind Data 
library(pacman)
p_load(data.table)


dir <- "~/Dropbox/CreditCards/Data/DiD_TwoMonthsBefore/"
files <- list.files(paste0(dir, "Event_files/"))

full_dta <- data.table()

count = 0

# create df of employees employed the entire period - here require more than 50 employees 
for (f in files){
  # update count 
  count = count + 1
  # read in file 
  fname <- paste0(dir, "Event_files/", f)
  dta <- fread(fname)
  # count number months per employee-firm combo
  dta[, NumMonths := uniqueN(YearMonth), .(ARM_ID_NBR, TICKER)]
  # keep those employed the whole time
  dta <- dta[NumMonths == 3]
  # require more than 50 employees 
  dta[, NumEmployees := uniqueN(ARM_ID_NBR), .(TICKER)]
  dta <- dta[NumEmployees >= 50]
  # determine if there is still a treated firm 
  maxTreat <- max(dta$Treat)
  if (maxTreat == 1){
    full_dta <- rbind(full_dta, dta)
  }
  
  if (count %% 10 == 0){
    gc()
    print(paste0("On file ", as.character(count)))
  }
}

# save 
fname <- paste0(dir, "StackedData_EmployedFullEvent.RDS")
saveRDS(full_dta,fname )

rm(dta)
rm(full_dta)

full_dta <- data.table()

# create df of turnover status 
for (f in files){
  # update count 
  count = count + 1
  # read in file 
  fname <- paste0(dir, "Event_files/", f)
  dta <- fread(fname)
  # count number months per employee-firm combo
  dta_agg <- dta[, list(NumMonths = uniqueN(YearMonth)), .(ARM_ID_NBR, TICKER, Post, Event, 
                                                           CRD_HLDR_DOB, CRD_HLDR_ZIP, Treat)]
  dta_agg[, FullEmployPre := ifelse(NumMonths[Post == 0][1] == 2, 1, 0), .(ARM_ID_NBR, TICKER)]
  dta_agg[, FullEmployPost := ifelse(NumMonths[Post == 1][1] == 1, 1, 0), .(ARM_ID_NBR, TICKER)]
  # keep those employed the whole pre period 
  dta_agg <- dta_agg[FullEmployPre == 1]
  # determine if there is still a treated firm 
  maxTreat <- max(dta_agg$Treat)
  if (maxTreat == 1){
    full_dta <- rbind(full_dta, dta_agg)
  }
  
  if (count %% 10 == 0){
    gc()
    print(paste0("On file ", as.character(count)))
  }
}

# save 
fname <- paste0(dir, "StackedData_Turnover.RDS")
saveRDS(full_dta,fname )
