library(data.table)
rm(list=ls())

## CRSP
# Load CRSP stock data
crsp = data.table(read.csv("crsp_daily2.csv")) 
setkey(crsp, PERMNO, date)
# Restrict Universe of Stocks to common stock only
crsp <- crsp[SHRCD == 10 | SHRCD == 11]
# Restrict Universe of Stocks on exchanges
crsp <- crsp[EXCHCD == 1 | EXCHCD == 2 | EXCHCD == 3]
# Remove all finaical firms (SIC code 6190-6199)
crsp[is.na(as.numeric(as.character(SICCD))) , SICCD := NA]
crsp[, SICCD := (as.numeric(as.character(SICCD)))]
crsp = crsp[SICCD < 6190 | SICCD > 6199]

# Market Capitalization
# Drop all NA prices and take absolute values
crsp[, PRC := abs(PRC)]
# Calculate Market Equity (and Lagged)
crsp[, ME := (PRC * SHROUT), by = PERMNO]
crsp[, laggedME := shift(PRC * SHROUT), by = PERMNO]

## Missing Returns
# Drop out returns that are missing (specified by CRSP)
crsp = crsp[!RET %in% c(-66.0,-77.0,-88.0,-99.0)]
crsp = crsp[!DLRET %in% c(-66.0,-55.0,-88.0,-99.0)]
# convert all non-numeric returns to empty
crsp[is.na(as.numeric(as.character(RET))) , RET := NA]
crsp[is.na(as.numeric(as.character(DLRET))) , DLRET := NA]
# Converet to Numeric
crsp[, RET := (as.numeric(as.character(RET)))]
crsp[, DLRET := (as.numeric(as.character(DLRET)))]
# if both returns are not missing, calculate Delisting-adjusted return
crsp[!is.na(RET) & !is.na(DLRET), RET := (1 + RET)*(1+DLRET)-1 ]
# if r is missing but r_D is not missing, use r_D
crsp[is.na(RET) & !is.na(DLRET), RET := DLRET ]
# Drop NA returns
crsp = crsp[!is.na(RET)]


# Convert CRSP from Security level to company's level
# if a company issued multiple stocks ,value weighted each stock
crsp[, weight_company := laggedME / sum(laggedME, na.rm = T), by = list(PERMCO,date)]
# calculate total value-weighted return for each company
crsp[, RET := sum(weight_company * RET, na.rm = T), by = list(PERMCO,date)]
# Get  ME at the company level
crsp[, ME := sum(ME, na.rm = T), by = list(PERMCO,date)]
crsp[, laggedME := sum(laggedME, na.rm = T), by = list(PERMCO,date)]
crsp = crsp[ME > 0]
crsp = crsp[laggedME > 0]

# Get company level data
company_crsp = unique(crsp[, .(EXCHCD, RET,ME,laggedME), by = list(PERMCO,date)])
setorder(company_crsp, PERMCO, date)


## Link Table
# Load link table
linktable = data.table(read.csv("link.csv")) 
# Restrict link types to primary only
linktable = linktable[LINKPRIM == 'P' | LINKPRIM == 'C']
# Convert dates to numerical values
linktable[is.na(as.numeric(as.character(LINKDT))) , LINKDT := NA]
linktable[is.na(as.numeric(as.character(LINKENDDT))) , LINKENDDT := NA]
linktable[, LINKDT := (as.numeric(as.character(LINKDT)))]
linktable[, LINKENDDT := (as.numeric(as.character(LINKENDDT)))]

# first do a gross merge. There are duplicates
merged = data.table(merge(company_crsp, linktable, by.x = 'PERMCO', by.y = 'LPERMCO',allow.cartesian = T))
# only keep the ones that are within the date range
merged = merged[(is.na(LINKDT) | date >= LINKDT) & (is.na(LINKENDDT) | date <= LINKENDDT)]
setorder(merged, gvkey, date)

## Cleaning
merged[, ifToKeep := 1]
# 1. LC is the highest quality match - only keep LC
merged[, ifMultiple := .N > 1 , by = list(PERMCO,date)]
merged[ifMultiple == T & LINKTYPE != "LC", ifToKeep := 0]
merged = merged[ifToKeep == 1]

# 2. LINKPRIM only keep Primary
merged[, ifMultiple := .N > 1 , by = list(PERMCO,date)]
merged[ifMultiple == T & LINKPRIM != "P", ifToKeep := 0]
merged = merged[ifToKeep == 1]

# 3. LIID only keep 1
merged[, ifMultiple := .N > 1 , by = list(PERMCO,date)]
merged[ifMultiple == T & LIID != '01', ifToKeep := 0]
merged = merged[ifToKeep == 1]

# 4. Keep most current 
merged[, ifMultiple := .N > 1 , by = list(PERMCO,date)]
merged[ifMultiple == T, NumofMostCurrent := sum(is.na(LINKENDDT)), by = list(PERMCO,date)]
merged[NumofMostCurrent != 0 & ifMultiple == T & !is.na(LINKENDDT), 
       ifToKeep := 0, by = list(PERMCO,date)]
merged = merged[ifToKeep == 1]
merged$NumofMostCurrent = NULL

# 5. Use the smaller GVKey
setorder(merged, PERMCO, date, gvkey)
merged = unique(merged, by = c('PERMCO','date'))

cleaned_merged = data.table(merged[,.(gvkey,PERMCO)])
setorder(cleaned_merged, PERMCO)
cleaned_merged = unique(cleaned_merged, by = c('PERMCO'))
cleaned_merged = unique(cleaned_merged, by = c('gvkey'))
library(foreign)
write.dta(cleaned_merged, 'gvkey_permco_lookup.dta')
cleaned_merged = read.dta('gvkey_permco_lookup.dta')


earningsA = data.table(read.csv("earningsannoucements.csv"))
earningsA = merge(earningsA, cleaned_merged, by = 'gvkey')
earningsA = earningsA[!is.na(rdq)]



calcRet5DayBefore = function(annoucement_date, permco) {
  subset = company_crsp[PERMCO == permco]
  index = 0
  if (nrow(subset) >=5)
    for (i in 1:nrow(subset)) {
      if (subset$date[i] == annoucement_date) {
        index = i
        break
      }
    }
  if (index != 0) {
    ret_before = NULL
    if (index >= 6)
      ret_before = subset$RET[(index - 5):(index - 1)]
    ret_before = ret_before + 1
    
    return (prod(ret_before) - 1)
  }
}

calcRet5DayAfter = function(annoucement_date, permco) {

  subset = company_crsp[PERMCO == permco]
  index = 0
  if (nrow(subset) >=5)
    for (i in 1:nrow(subset)) {
      if (subset$date[i] == annoucement_date) {
        index = i
        break
      }
    }
  if (index != 0) {
    ret_after = NULL
    if (index <= nrow(subset) - 5)
      ret_after = subset$RET[(index + 1):(index + 5)]
    ret_after = ret_after + 1
    return (prod(ret_after) - 1)
  }
}

calcPast10dayVol = function(annoucement_date, permco) 
{
  subset = company_crsp[PERMCO == permco]
  index = 0
  if (nrow(subset) >=5)
    for (i in 1:nrow(subset)) {
      if (subset$date[i] == annoucement_date) {
        index = i
        break
      }
    }
  if (index != 0) {
    ret_before = NULL
    if (index >= 11)
      ret_before = subset$RET[(index - 10):(index - 1)]
    return (sd(ret_before))
  }
}

calcPast30dayVol = function(annoucement_date, permco) 
{
  subset = company_crsp[PERMCO == permco]
  index = 0
  if (nrow(subset) >=5)
    for (i in 1:nrow(subset)) {
      if (subset$date[i] == annoucement_date) {
        index = i
        break
      }
    }
  if (index != 0) {
    ret_before = NULL
    if (index >= 31)
      ret_before = subset$RET[(index - 30):(index - 1)]
    return (sd(ret_before))
  }
}
earningsA[, Ret5DayBefore := calcRet5DayBefore(rdq, PERMCO), by = seq_len(nrow(earningsA))]
earningsA[, Ret5DayAfter := calcRet5DayAfter(rdq, PERMCO), by = seq_len(nrow(earningsA))]
earningsA[, Past10dayVol := calcPast10dayVol(rdq, PERMCO), by = seq_len(nrow(earningsA))]
earningsA[, Past30dayVol := calcPast30dayVol(rdq, PERMCO), by = seq_len(nrow(earningsA))]



earningsA[, ifPositiveDrift_Lag := shift(Ret5DayAfter), by = PERMCO]
earningsA[, ifPositiveDrift_Lag := ifelse(ifPositiveDrift_Lag > 0, 1, 0)]


market = data.table(read.csv("market.csv"))
market[, dd := as.integer(caldt / 100)]
vv = NULL
for (i in 63:nrow(market)) {
  vv[i] = sd(market$vwretx[(i-62):i])
}
market$Past3MonthVol = vv

assignPast3MonthMarketVol = function(rdq) {
  #date = as.integer(rdq / 100)
  ss = market[dd == rdq]
  return (ss$Past3MonthVol)
}

assignME = function(annoucement_date, permco) {
  subset = company_crsp[PERMCO == permco]
  index = 0
  if (nrow(subset) >=5)
    for (i in 1:nrow(subset)) {
      if (subset$date[i] == annoucement_date) {
        index = i
        break
      }
    }
  if (index != 0) {
    return (subset$laggedME[index])
  }
}


earningsA[, dd := as.integer(rdq / 100)]
earningsA[, PastMktVol3Month:= assignPast3MonthMarketVol(rdq), by = seq_len(nrow(earningsA))]
earningsA[, LaggedME:= assignME(rdq, PERMCO), by = seq_len(nrow(earningsA))]

test = copy(earningsA)

earningsA = data.table(read.dta("Ret_5_day_eps.dta"))



earningsA$dd = NULL
earningsA$indfmt = NULL
earningsA$consol = NULL
earningsA$popsrc = NULL
earningsA$datafmt = NULL
earningsA$curcdq = NULL
earningsA$costat = NULL
write.dta(earningsA, "Ret_5_day_eps.dta")
write.csv(earningsA, "Ret_5_day_eps.csv")


fundamentals = data.table(read.csv('fundamental.csv'))
setorder(fundamentals, gvkey, rdq)
fundamentals[, CashInvestment := shift(cheq), by = gvkey ]
fundamentals[, ChangeInCashInvestment := CashInvestment - shift(CashInvestment), by = gvkey ]
fundamentals[, SharesRepurchased := shift(cshopq), by = gvkey ]
#fundamentals[is.na(SharesRepurchased), SharesRepurchased := 0, by = gvkey ]
fundamentals[, LTDebtDue1Year := shift(dd1q), by = gvkey]
#fundamentals[is.na(SharesRepurchased), SharesRepurchased := 0, by = gvkey ]
fundamentals[, Amortization := shift(dpq), by = gvkey]


test = copy(earningsA)
test = merge(x = test, y = fundamentals[ , c("gvkey","rdq","ChangeInCashInvestment", "SharesRepurchased",
                                             "LTDebtDue1Year","Amortization")], 
             by = c("gvkey","rdq"))
test = unique(test, by = c("gvkey","rdq"))
write.dta(test, "fundamental_Ret_5_day_eps.dta")
write.csv(test, "fundamental_Ret_5_day_eps.csv")



calcRet1DayBefore = function(annoucement_date, permco) {
  subset = company_crsp[PERMCO == permco]
  index = 0
  if (nrow(subset) >=5)
    for (i in 1:nrow(subset)) {
      if (subset$date[i] == annoucement_date) {
        index = i
        break
      }
    }
  if (index != 0) {
    ret_before = NULL
    if (index >= 2)
      ret_before = subset$RET[index - 1]
    return (ret_before)
  }
}

test[, Ret1DayBefore := calcRet1DayBefore(rdq, PERMCO), by = seq_len(nrow(test))]
test[, Ret1DayBefore := Ret1DayBefore + 1]
test[, PastMktVol3Month := assignPast3MonthMarketVol(rdq), by = seq_len(nrow(test))]


test = data.table(read.csv("fundamental_Ret_5_day_eps.csv"))

calcPast200SK = function(r) {
  r = as.numeric(r)
  sk = rep(NA, length(r))
  if (length(r) >= 201) {
    for (i in 201:length(r)) {
      prev_r = r[(i-200):(i-1)]
      sk[i] = skewness(prev_r,na.rm = T)
    }
  }
  return (sk)
}

calcPast200Kur = function(r) {
  r = as.numeric(r)
  sk = rep(NA, length(r))
  if (length(r) >= 201) {
    for (i in 201:length(r)) {
      prev_r = r[(i-200):(i-1)]
      sk[i] = kurtosis(prev_r,na.rm = T)
    }
  }
  return (sk)
}


company_crsp[, Past200SK := as.numeric(calcPast200SK(RET)), by = PERMCO]
company_crsp[, Past200Kur := as.numeric(calcPast200Kur(RET)), by = PERMCO]

library(moments)

test = merge(x = test, y = company_crsp[, c('PERMCO','Past200SK', 'date', 'Past200Kur')], 
             by.x = c('PERMCO','rdq'), by.y = c('PERMCO','date'))



calcRet30DayAfter = function(annoucement_date, permco) {
  subset = company_crsp[PERMCO == permco]
  index = 0
  for (i in 1:nrow(subset)) {
    if (subset$date[i] == annoucement_date) {
      index = i
      break
    }
  }
  if (index != 0) {
    ret_after = NULL
    if (index <= nrow(subset) - 30)
      ret_after = subset$RET[(index + 1):(index + 30)]
    ret_after = ret_after + 1
    return (prod(ret_after) - 1)
  }
}

test[, Ret30DayAfter := calcRet30DayAfter(rdq, PERMCO), by = seq_len(nrow(test))]


calcRet60DayAfter = function(annoucement_date, permco) {
  subset = company_crsp[PERMCO == permco]
  index = 0
  for (i in 1:nrow(subset)) {
    if (subset$date[i] == annoucement_date) {
      index = i
      break
    }
  }
  if (index != 0) {
    ret_after = NULL
    if (index <= nrow(subset) - 60)
      ret_after = subset$RET[(index + 1):(index + 60)]
    ret_after = ret_after + 1
    return (prod(ret_after) - 1)
  }
}

test[, Ret60DayAfter := calcRet60DayAfter(rdq, PERMCO), by = seq_len(nrow(test))]
write.csv(test, "wohewozuihoudejuejiang.csv")
