# Code to set up the table data frames and save them as rds files
# May 7, 2021

pkgs <- c("cansim","tidyverse","stringr","gt","rlist")
inst <- lapply(pkgs,library,character.only=TRUE)

savespot <- "/Users/philipsmith/Documents/R/NatAcctsBrowserV9/"

GDPdf <- get_cansim_vector("v62295562","1961-01-01")
GDPdf$REF_DATE <- as.Date(paste0(GDPdf$REF_DATE,"-01"))
saveRDS(GDPdf,paste0(savespot,"rds/GDPdf.rds"))
GDPNSAdf <- get_cansim_vector("v62295576","1961-01-01")
GDPNSAdf$REF_DATE <- as.Date(paste0(GDPNSAdf$REF_DATE,"-01"))
saveRDS(GDPNSAdf,paste0(savespot,"rds/GDPNSAdf.rds"))
# Historical GDP: 1947 Q1 to 1997 Q2
GDPHdf <- get_cansim_vector("v87224076","1947-01-01")
GDPHdf$REF_DATE <- as.Date(paste0(GDPHdf$REF_DATE,"-01"))
saveRDS(GDPHdf,paste0(savespot,"rds/GDPHdf.rds"))

file_refresh <- TRUE

#(01)===========================================================================
table01_id <- "36-10-0103-01" # Income-based GDP
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(02)===========================================================================
table01_id <- "36-10-0104-01" # Expenditure-based GDP
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Prices=="Current prices",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(03)===========================================================================
table01_id <- "36-10-0104-01" # Expenditure-based GDP - chain Fisher prices
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0104-02"
q0 <- filter(table01,Prices=="Chained (2012) dollars",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(04)===========================================================================
table01_id <- "36-10-0106-01" # GDP price indexes
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Index=="Implicit price indexes")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(05)===========================================================================
table01_id <- "36-10-0122-01" # GDI, GNI and NNI
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(06)===========================================================================
table01_id <- "36-10-0111-01" # Current and capital accounts, national
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
j <- 0 # Get rid of duplicate names
for (i in 1:240) { 
  k <- 1+j*47
  q0[k   ,2] <- "Balance of primary income"
  q0[k+ 1,2] <- "Households (1)"
  q0[k+ 2,2] <- "Non-profit institutions serving households (1)"
  q0[k+ 3,2] <- "Corporations (1)"
  q0[k+ 4,2] <- "General governments (1)"
  q0[k+ 5,2] <- "Plus: statistical discrepancy"
  q0[k+ 6,2] <- "Equals: net national income"
  q0[k+ 7,2] <- "Net national income"
  q0[k+ 8,2] <- "Less: net current transfers to non-residents"
  q0[k+ 9,2] <- "Equals: national disposable income"
  q0[k+10,2] <- "National disposable income"
  q0[k+11,2] <- "Households (2)"
  q0[k+12,2] <- "Non-profit institutions serving households (2)"
  q0[k+13,2] <- "Corporations (2)"
  q0[k+14,2] <- "General governments (2)"
  q0[k+15,2] <- "Statistical discrepancy (1)"
  q0[k+16,2] <- "Less: final consumption expenditure"
  q0[k+17,2] <- "Equals: national net saving"
  q0[k+18,2] <- "National net saving (1)"
  q0[k+19,2] <- "Households (3)"
  q0[k+20,2] <- "Non-profit institutions serving households (3)"
  q0[k+21,2] <- "Corporations (3)"
  q0[k+22,2] <- "General governments (3)"
  q0[k+23,2] <- "Statistical discrepancy (2)"
  q0[k+24,2] <- "Divided by: national disposable income"
  q0[k+25,2] <- "Equals: national saving rate"
  q0[k+26,2] <- "National net saving (2)"
  q0[k+27,2] <- "Plus: consumption of fixed capital"
  q0[k+28,2] <- "Households (4)"
  q0[k+29,2] <- "Non-profit institutions serving households (4)"
  q0[k+30,2] <- "Corporations (4)"
  q0[k+31,2] <- "General governments (4)"
  q0[k+32,2] <- "Plus: national net capital transfers received"
  q0[k+33,2] <- "Households (5)"
  q0[k+34,2] <- "Non-profit institutions serving households (5)"
  q0[k+35,2] <- "Corporations (5)"
  q0[k+36,2] <- "General governments (5)"
  q0[k+37,2] <- "Less: non-financial capital acquisitions"
  q0[k+38,2] <- "Households (6)"
  q0[k+39,2] <- "Non-profit institutions serving households (6)"
  q0[k+40,2] <- "Corporations (6)"
  q0[k+41,2] <- "General governments (6)"
  q0[k+42,2] <- "Equals: national net lending or net borrowing"
  q0[k+43,2] <- "Households (7)"
  q0[k+44,2] <- "Non-profit institutions serving households (7)"
  q0[k+45,2] <- "Corporations (7)"
  q0[k+46,2] <- "General governments (7)"  
  j <- j+1
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(07)===========================================================================
table01_id <- "36-10-0124-01" # Detailed HH final consumption expenditure
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Prices=="Current prices",
  `Seasonal adjustment`=="Seasonally adjusted at quarterly rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1981-01-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(08)===========================================================================
table01_id <- "36-10-0124-01" # Detailed HH final consumption expenditure
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0124-02" # To differentiate from table 7
q0 <- filter(table01,Prices=="2012 constant prices",
  `Seasonal adjustment`=="Seasonally adjusted at quarterly rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1981-01-01"))
for (i in 160:1) { # remove double occurrence of adjusting entry
  q0 <- q0[-i*132,]
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(09)===========================================================================
# q0 matrixes are calculated in GDP_by_industry.R
q0 <- readRDS(paste0(savespot,"rds/36-10-0434-01.rds"))

#(10)===========================================================================
# q0 matrixes are calculated in GDP_by_industry.R
q0 <- readRDS(paste0(savespot,"rds/36-10-0434-02.rds"))

#(11)===========================================================================
# q0 matrixes are calculated in GDP_by_industry.R
q0 <- readRDS(paste0(savespot,"rds/36-10-0434-03.rds"))

#(12)===========================================================================
table01_id <- "36-10-0018-01" # Balance of international payments
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- table01
q1 <- filter(q0,`Receipts, payments and balances`=="Receipts, seasonally adjusted")
q1 <- select(q1,REF_DATE,`Current account`,VALUE)
for (i in 1:nrow(q1)) {
  q1[i,2] <- paste0(q1[i,2]," - receipts")
}
q2 <- filter(q0,`Receipts, payments and balances`=="Payments, seasonally adjusted")
q2 <- select(q2,REF_DATE,`Current account`,VALUE)
for (i in 1:nrow(q2)) {
  q2[i,2] <- paste0(q2[i,2]," - payments")
}
q3 <- filter(q0,`Receipts, payments and balances`=="Balances, seasonally adjusted")
q3 <- select(q3,REF_DATE,`Current account`,VALUE)
for (i in 1:nrow(q3)) {
  q3[i,2] <- paste0(q3[i,2]," - balance")
}
q0 <- rbind(q1,q2,q3)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- arrange(q0,REF_DATE)
q0 <- pivot_wider(q0,names_from=`Current account`,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(13)===========================================================================
table01_id <- "36-10-0108-01" # GFKF
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Prices=="Current prices",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*60+54,2] <- "Total gross fixed capital formation-"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(14)===========================================================================
table01_id <- "36-10-0108-01" # GFKF real
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0108-02"
q0 <- filter(table01,Prices=="Chained (2012) dollars",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*60+54,2] <- "Total gross fixed capital formation-"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(15)===========================================================================
table01_id <- "36-10-0477-01" # General government
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0477-01"
q0 <- filter(table01,`Levels of government`=="General governments",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*43+3,2] <- "From households-"
  q0[(i-1)*43+17,2] <- "Capital transfers-"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(16)===========================================================================
table01_id <- "36-10-0477-01" # Federal general government
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0477-02"
q0 <- filter(table01,`Levels of government`=="Federal general government",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
# Add zero cannabis taxes pre-2017
q1 <- data.frame()
for (i in 1:224) { # This loop takes about 2 minutes
  for (j in 1:18) {
    k1 <- (i-1)*127+j
    k2 <- (i-1)*127+j+(i-1)
    q1[k2,1] <- q0[k1,1]
    q1[k2,2] <- q0[k1,2]
    q1[k2,3] <- q0[k1,3]
  }
  q1[k2+1,1] <- q0[k1,1]
  q1[k2+1,2] <- "Of which: cannabis taxes"
  q1[k2+1,3] <- 0
  for (j in 19:127) {
    k1 <- (i-1)*127+j
    k2 <- (i-1)*127+j+i
    q1[k2,1] <- q0[k1,1]
    q1[k2,2] <- q0[k1,2]
    q1[k2,3] <- q0[k1,3]
  }
}
# The new q0 including cannabis taxes back to 1961:
q2 <- rbind(q1,q0[(28449:nrow(q0)),])
# Get rid of duplicate names
for (i in 1:240) { 
  k <- (i-1)*128
  q2[k+  3,2] <- "From households (1)"
  q2[k+ 36,2] <- "From provincial and territorial general governments (1)"
  q2[k+ 37,2] <- "From provincial administration (1)"
  q2[k+ 38,2] <- "From provincial education (1)"
  q2[k+ 39,2] <- "From provincial health and social services (1)"
  q2[k+ 40,2] <- "From local general governments (1)"
  q2[k+ 41,2] <- "From Aboriginal general governments (1)"
  q2[k+ 48,2] <- "Capital transfers (1)"
  q2[k+ 49,2] <- "From households (2)"
  q2[k+ 53,2] <- "From provincial and territorial general governments (2)"
  q2[k+ 54,2] <- "From provincial administration (2)"
  q2[k+ 55,2] <- "From provincial education (2)"
  q2[k+ 56,2] <- "From provincial health and social services (2)"
  q2[k+ 57,2] <- "From local general governments (2)"
  q2[k+ 58,2] <- "From Aboriginal general governments (2)"
  q2[k+ 80,2] <- "Agriculture (1)"
  q2[k+ 81,2] <- "Non-agriculture (1)"
  q2[k+ 83,2] <- "Agriculture (2)"
  q2[k+ 84,2] <- "Non-agriculture (2)"
  q2[k+ 86,2] <- "To provincial and territorial general governments (1)"
  q2[k+102,2] <- "To provincial health and social services (1)"
  q2[k+103,2] <- "To local general governments (1)"
  q2[k+104,2] <- "To Aboriginal general governments (1)"
  q2[k+108,2] <- "Capital transfers (2)"
  q2[k+113,2] <- "To provincial and territorial general governments (2)"
  q2[k+114,2] <- "To provincial health and social services (2)"
  q2[k+117,2] <- "To local general governments (2)"
  q2[k+118,2] <- "To Aboriginal general governments (2)"
}
q2 <- pivot_wider(q2,names_from=Estimates,values_from=VALUE)
q2[is.na(q2)]=0
q0 <- q2
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(17)===========================================================================
table01_id <- "36-10-0477-01" # Provincial general governments
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0477-03"
q0 <- filter(table01,`Levels of government`=="Provincial and territorial general governments",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) { 
  k <- (i-1)*56
  q0[k+  3,2] <- "From households (1)"
  q0[k+ 20,2] <- "From households (2)"
  q0[k+ 14,2] <- "From federal general government (1)"
  q0[k+ 24,2] <- "From federal general government (2)"
  q0[k+ 15,2] <- "From local general governments (1)"
  q0[k+ 25,2] <- "From local general governments (2)"
  q0[k+ 16,2] <- "From Aboriginal general governments (1)"
  q0[k+ 26,2] <- "From Aboriginal general governments (2)"
  q0[k+ 19,2] <- "Capital transfers (1)"
  q0[k+ 39,2] <- "Capital transfers (2)"
  q0[k+ 35,2] <- "To federal general government (1)"
  q0[k+ 44,2] <- "To federal general government (2)"
  q0[k+ 36,2] <- "To local general governments (1)"
  q0[k+ 45,2] <- "To local general governments (2)"
  q0[k+ 37,2] <- "To Aboriginal general governments (1)"
  q0[k+ 46,2] <- "To Aboriginal general governments (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(18)===========================================================================
table01_id <- "36-10-0114-01" # Compensation of employees
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(19)===========================================================================
table01_id <- "36-10-0117-01" # Undistributed corporate profits
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Corporations=="Total corporations",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(20)===========================================================================
table01_id <- "36-10-0126-01" # Property income of households
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,`Geographic region`=="All countries",
  Currency=="All currencies")
q0 <- select(q0,REF_DATE,`Canada's international investment position`,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=`Canada's international investment position`,
  values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(21)===========================================================================
table01_id <- "36-10-0412-01" # International investment position
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(22)===========================================================================
table01_id <- "36-10-0206-01" # Labour productivity
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,
  "LPMRM"="Labour productivity measures and related measures",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=LPMRM,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(23)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (1)
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Real gross ",
    "domestic product (GDP)"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(24)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (2)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-02"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Total ",
    "number of jobs"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(25)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (3)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-03"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Average ",
    "hours worked"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(26)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (4)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-04"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Hours ",
    "worked"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(27)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (5)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-05"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Labour ",
    "productivity"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(28)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (6)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-06"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Total ",
    "compensation per hour worked"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(29)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (7)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-07"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Unit ",
    "labour cost"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(30)===========================================================================
table01_id <- "36-10-0207-01" # Labour productivity by industry (8)
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0207-08"
q0 <- filter(table01,
  `Labour productivity measures and related variables`==paste0("Unit ",
    "labour cost in United States dollars"))
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- filter(q0,REF_DATE>=as.Date("1997-01-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
q0[is.na(q0)]=0
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(31)===========================================================================
table01_id <- "36-10-0105-01" # GDP-related indexes
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(32)===========================================================================
table01_id <- "36-10-0109-01" # Inventories
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Prices=="Chained (2012) dollars",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*27+ 5,2] <- "Durable goods (1)"
  q0[(i-1)*27+ 6,2] <- "Non-durable goods (1)"
  q0[(i-1)*27+ 9,2] <- "Durable goods (2)"
  q0[(i-1)*27+11,2] <- "Non-durable goods (2)"
  q0[(i-1)*27+13,2] <- "Durable goods (3)"
  q0[(i-1)*27+14,2] <- "Non-durable goods (3)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
# remove line because not millions of dollars like the rest
q0 <- select(q0,-`Quarterly stock to sales ratio, total economy`)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(33)===========================================================================
table01_id <- "36-10-0112-01" # Current and capital accounts - HH
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*48+14,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*48+15,2] <- "From corporations (1)"
  q0[(i-1)*48+16,2] <- "From general governments (1)"
  q0[(i-1)*48+20,2] <- "From non-residents (1)"
  q0[(i-1)*48+38,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*48+39,2] <- "From corporations (2)"
  q0[(i-1)*48+40,2] <- "From general governments (2)"
  q0[(i-1)*48+41,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(34)===========================================================================
table01_id <- "36-10-0115-01" # Current and capital accounts - NPISH
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*33+ 7,2] <- "From households (1)"
  q0[(i-1)*33+ 8,2] <- "From corporations (1)"
  q0[(i-1)*33+ 9,2] <- "From general governments (1)"
  q0[(i-1)*33+10,2] <- "From non-residents (1)"
  q0[(i-1)*33+23,2] <- "From households (2)"
  q0[(i-1)*33+24,2] <- "From corporations (2)"
  q0[(i-1)*33+25,2] <- "From general governments (2)"
  q0[(i-1)*33+26,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(35)===========================================================================
table01_id <- "36-10-0116-01" # Current and capital accounts - corporations
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Corporations=="Total corporations",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*45+19,2] <- "From households (1)"
  q0[(i-1)*45+20,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*45+21,2] <- "From non-residents (1)"
  q0[(i-1)*45+35,2] <- "From households (2)"
  q0[(i-1)*45+36,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*45+38,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(36)===========================================================================
table01_id <- "36-10-0118-01" # Current and capital accounts - governments
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,`Levels of government`=="General governments",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*42+12,2] <- "From households (1)"
  q0[(i-1)*42+16,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*42+17,2] <- "From corporations (1)"
  q0[(i-1)*42+18,2] <- "From non-residents (1)"
  q0[(i-1)*42+32,2] <- "From households (2)"
  q0[(i-1)*42+33,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*42+34,2] <- "From corporations (2)"
  q0[(i-1)*42+35,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(37)===========================================================================
table01_id <- "36-10-0121-01" # Current and capital accounts - NR
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(38)===========================================================================
table01_id <- "36-10-0118-01" # Current and capital accounts - federal govt
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0118-02"
q0 <- filter(table01,`Levels of government`=="Federal general government",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*56+12,2] <- "From households (1)"
  q0[(i-1)*56+16,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*56+17,2] <- "From corporations (1)"
  q0[(i-1)*56+18,2] <- "From general governments (1)"
  q0[(i-1)*56+19,2] <- "From provincial and territorial general governments (1)"
  q0[(i-1)*56+20,2] <- "From local general governments (1)"
  q0[(i-1)*56+21,2] <- "From Aboriginal general governments (1)"
  q0[(i-1)*56+22,2] <- "From non-residents (1)"
  q0[(i-1)*56+40,2] <- "From households (2)"
  q0[(i-1)*56+41,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*56+42,2] <- "From corporations (2)"
  q0[(i-1)*56+45,2] <- "From general governments (2)"
  q0[(i-1)*56+46,2] <- "From provincial and territorial general governments (2)"
  q0[(i-1)*56+47,2] <- "From local general governments (2)"
  q0[(i-1)*56+48,2] <- "From Aboriginal general governments (2)"
  q0[(i-1)*56+49,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(39)===========================================================================
table01_id <- "36-10-0118-01" # Current and capital accounts - prov/terr govt
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0118-03"
q0 <- filter(table01,`Levels of government`=="Provincial and territorial general governments",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*52+12,2] <- "From households (1)"
  q0[(i-1)*52+16,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*52+17,2] <- "From corporations (1)"
  q0[(i-1)*52+18,2] <- "From general governments (1)"
  q0[(i-1)*52+19,2] <- "From federal general government (1)"
  q0[(i-1)*52+20,2] <- "From local general governments (1)"
  q0[(i-1)*52+21,2] <- "From Aboriginal general governments (1)"
  q0[(i-1)*52+22,2] <- "From non-residents (1)"
  q0[(i-1)*52+38,2] <- "From households (2)"
  q0[(i-1)*52+39,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*52+40,2] <- "From corporations (2)"
  q0[(i-1)*52+41,2] <- "From general governments (2)"
  q0[(i-1)*52+42,2] <- "From federal general government (2)"
  q0[(i-1)*52+43,2] <- "From local general governments (2)"
  q0[(i-1)*52+44,2] <- "From Aboriginal general governments (2)"
  q0[(i-1)*52+45,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(39)===========================================================================
table01_id <- "36-10-0118-01" # Current and capital accounts - prov/terr govt
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0118-03"
q0 <- filter(table01,`Levels of government`=="Provincial and territorial general governments",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*52+12,2] <- "From households (1)"
  q0[(i-1)*52+16,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*52+17,2] <- "From corporations (1)"
  q0[(i-1)*52+18,2] <- "From general governments (1)"
  q0[(i-1)*52+19,2] <- "From federal general government (1)"
  q0[(i-1)*52+20,2] <- "From local general governments (1)"
  q0[(i-1)*52+21,2] <- "From Aboriginal general governments (1)"
  q0[(i-1)*52+22,2] <- "From non-residents (1)"
  q0[(i-1)*52+38,2] <- "From households (2)"
  q0[(i-1)*52+39,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*52+40,2] <- "From corporations (2)"
  q0[(i-1)*52+41,2] <- "From general governments (2)"
  q0[(i-1)*52+42,2] <- "From federal general government (2)"
  q0[(i-1)*52+43,2] <- "From local general governments (2)"
  q0[(i-1)*52+44,2] <- "From Aboriginal general governments (2)"
  q0[(i-1)*52+45,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(40)===========================================================================
table01_id <- "36-10-0118-01" # Current and capital accounts - local govt
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0118-04"
q0 <- filter(table01,`Levels of government`=="Local general governments",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*49+12,2] <- "From households (1)"
  q0[(i-1)*49+13,2] <- "From non-profit institutions serving households (1)"
  q0[(i-1)*49+14,2] <- "From corporations (1)"
  q0[(i-1)*49+15,2] <- "From general governments (1)"
  q0[(i-1)*49+16,2] <- "From federal general government (1)"
  q0[(i-1)*49+17,2] <- "From provincial and territorial general governments (1)"
  q0[(i-1)*49+18,2] <- "From Aboriginal general governments (1)"
  q0[(i-1)*49+19,2] <- "From non-residents (1)"
  q0[(i-1)*49+35,2] <- "From households (2)"
  q0[(i-1)*49+36,2] <- "From non-profit institutions serving households (2)"
  q0[(i-1)*49+37,2] <- "From corporations (2)"
  q0[(i-1)*49+38,2] <- "From general governments (2)"
  q0[(i-1)*49+39,2] <- "From federal general government (2)"
  q0[(i-1)*49+40,2] <- "From provincial and territorial general governments (2)"
  q0[(i-1)*49+41,2] <- "From Aboriginal general governments (2)"
  q0[(i-1)*49+42,2] <- "From non-residents (2)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(41)===========================================================================
table01_id <- "36-10-0109-01" # Inventories
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0109-02" # Inventories
q0 <- filter(table01,Prices=="Current prices",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,Estimates,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
for (i in 1:240) {
  q0[(i-1)*29+ 5,2] <- "Durable goods (1)"
  q0[(i-1)*29+ 6,2] <- "Non-durable goods (1)"
  q0[(i-1)*29+ 9,2] <- "Durable goods (2)"
  q0[(i-1)*29+11,2] <- "Non-durable goods (2)"
  q0[(i-1)*29+13,2] <- "Durable goods (3)"
  q0[(i-1)*29+14,2] <- "Non-durable goods (3)"
}
q0 <- pivot_wider(q0,names_from=Estimates,values_from=VALUE)
# remove lines because not millions of dollars like the rest
q0 <- select(q0,-`Quarterly stock to sales ratio, total economy`,
  -`Quarterly stock to sales ratio, manufacturing`,
  -`Quarterly stock to sales ratio, retail trade`,
  -`Quarterly stock to sales ratio, retail trade - motor vehicles`,
  -`Quarterly stock to sales ratio, wholesale trade`)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(42)===========================================================================
table01_id <- "12-10-0122-01" # Exports
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Trade=="Export",Basis=="Balance of payments",
  `Seasonal adjustment`=="Seasonally adjusted")
q0 <- select(q0,REF_DATE,
  "NAPCS"="North American Product Classification System (NAPCS)",
  "HIER"="Hierarchy for North American Product Classification System (NAPCS)",
  VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- mutate(q0,dots=str_count(q0$HIER,"\\."))
q0 <- filter(q0,dots<=2)
q0 <- select(q0,-HIER,-dots)
q0 <- pivot_wider(q0,names_from=NAPCS,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(43)===========================================================================
table01_id <- "12-10-0122-01" # Imports
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "12-10-0122-02"
q0 <- filter(table01,Trade=="Import",Basis=="Balance of payments",
  `Seasonal adjustment`=="Seasonally adjusted")
q0 <- select(q0,REF_DATE,
  "NAPCS"="North American Product Classification System (NAPCS)",
  "HIER"="Hierarchy for North American Product Classification System (NAPCS)",
  VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- mutate(q0,dots=str_count(q0$HIER,"\\."))
q0 <- filter(q0,dots<=2)
q0 <- select(q0,-HIER,-dots)
q0 <- pivot_wider(q0,names_from=NAPCS,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(44)===========================================================================
table01_id <- "12-10-0124-01" # Exports chained
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Trade=="Export")
q0 <- select(q0,REF_DATE,
  "NAPCS"="North American Product Classification System (NAPCS)",
  "HIER"="Hierarchy for North American Product Classification System (NAPCS)",
  VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- mutate(q0,dots=str_count(q0$HIER,"\\."))
q0 <- filter(q0,dots<=2)
q0 <- select(q0,-HIER,-dots)
q0 <- pivot_wider(q0,names_from=NAPCS,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(45)===========================================================================
table01_id <- "12-10-0124-01" # Imports chained
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "12-10-0124-02" # Imports chained
q0 <- filter(table01,Trade=="Import")
q0 <- select(q0,REF_DATE,
  "NAPCS"="North American Product Classification System (NAPCS)",
  "HIER"="Hierarchy for North American Product Classification System (NAPCS)",
  VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- mutate(q0,dots=str_count(q0$HIER,"\\."))
q0 <- filter(q0,dots<=2)
q0 <- select(q0,-HIER,-dots)
q0 <- pivot_wider(q0,names_from=NAPCS,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(46)===========================================================================
table01_id <- "36-10-0136-01" # Income-based GDP 47-97 archived
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,`Income-based estimates`,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=`Income-based estimates`,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(47)===========================================================================
table01_id <- "36-10-0137-01" # Expenditure-based GDP 47-97 archived
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Prices=="Current prices",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,`Expenditure-based estimates`,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=`Expenditure-based estimates`,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(48)===========================================================================
table01_id <- "36-10-0137-01" # Expenditure-based GDP 47-97 archived $86
table01 <- get_cansim(table01_id,refresh=file_refresh)
table01_id <- "36-10-0137-02" # Expenditure-based GDP 47-97 archived $86
q0 <- filter(table01,Prices=="1986 constant prices",
  `Seasonal adjustment`=="Seasonally adjusted at annual rates")
q0 <- select(q0,REF_DATE,`Expenditure-based estimates`,VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=`Expenditure-based estimates`,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))





