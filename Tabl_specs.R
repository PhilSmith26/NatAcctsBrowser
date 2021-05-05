# Tabl_specs.R
# Contains the specifications or metadata for the national accounts tables
# April 30, 2021

# NOTE: The data for tables are stored and must be updated once per quarter,
# along with GDPdf.rds, GDPNSAdf.rds and the lastdate variable

source("Series_lists.R")
lastdate <- "2020-10-01" # last date for which data are available in the app
numTabs <- 45 # Number of tables available in the app
tcol <- "#D7E3E9"  # table colour

t01 <- list(
  Num = 1,
  Len = 14,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1, # 1 = annual rates, 4 = quarterly rates
  STCno = "36-10-0103-01",
  Titl = "Gross domestic product, income-based",
  Ftnt = "Source: Statistics Canada table 36-10-0103-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_01,
  Indent= c(0,1,1,0,1,1,1,0,1,1,0,0,0,0),
  Idx = FALSE
)
t02 <- list(
  Num = 2,
  Len = 31,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0104-01",
  Titl = "Gross domestic product, expenditure-based",
  Ftnt = "Source: Statistics Canada table 36-10-0104-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_02,
  Indent= c(0,1,2,3,3,3,2,1,1,0,1,2,2,3,3,2,1,1,0,1,2,2,0,1,1,0,1,1,0,0,0),
  Idx = FALSE
)
t03 <- list(
  Num = 3,
  Len = 31,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0104-02",
  Titl = "Gross domestic product, expenditure-based, chained Fisher",
  Ftnt = "Source: Statistics Canada table 36-10-0104-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_03,
  Indent= c(0,1,2,3,3,3,2,1,1,0,1,2,2,3,3,2,1,1,0,1,2,2,0,1,1,0,1,1,0,0,0),
  Idx = TRUE
)
t04 <- list(
  Num = 4,
  Len = 26,
  Strt = as.Date("1961-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0106-01",
  Titl = "Gross domestic product, implicit price indexes",
  Ftnt = "Source: Statistics Canada table 36-10-0106-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_04,
  Indent= c(0,1,2,3,3,3,2,1,1,0,1,2,2,3,3,2,1,1,0,1,1,0,1,1,0,0),
  Idx = TRUE
)
t05 <- list(
  Num = 5,
  Len = 12,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0122-01",
  Titl = "Gross domestic income, gross national income and net national income",
  Ftnt = "Source: Statistics Canada table 36-10-0122-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_05,
  Indent= c(0,0,0,0,0,0,0,0,0,0,0,0),
  Idx = FALSE 
)
t06 <- list(
  Num = 6,
  Len = 47,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0111-01",
  Titl = "Current and capital accounts, national",
  Ftnt = "Source: Statistics Canada table 36-10-0111-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_06,
  Indent= c(0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1),
  Idx = FALSE
)
t07 <- list(
  Num = 7,
  Len = 116,
  Strt = as.Date("1981-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "36-10-0124-01",
  Titl = "Detailed household final consumption expenditure",
  Ftnt = "Source: Statistics Canada table 36-10-0124-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_07,
  Indent= c(0,1,2,2,1,2,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2),
  Idx = FALSE
)
t08 <- list(
  Num = 8,
  Len = 131,
  Strt = as.Date("1981-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "36-10-0124-02",
  Titl = "Detailed household final consumption expenditure, 2012 constant prices",
  Ftnt = "Source: Statistics Canada table 36-10-0124-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_08,
  Indent= c(0,1,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1),
  Idx = TRUE
)
t09 <- list(
  Num = 9,
  Len = 96,
  Strt = as.Date("1997-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0434-01",
  Titl = "Gross domestic product by industry, chained Fisher, part 1",
  Ftnt = "Source: Statistics Canada table 36-10-0434-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_09,
  Indent= c(0,1,1,1,2,2,1,2,2,1,1,1,1,2,2,1,1,1,1,1,1,2,2,1,1,2,3,4,4,4,4,3,2,2,2,1,2,3,3,2,3,3,4,4,4,4,3,4,4,4,4,4,3,1,2,2,2,1,2,2,2,2,1,2,3,3,3,3,3,3,3,3,3,2,3,3,3,3,2,3,3,2,3,3,3,2,3,3,2,2,3,3,2,3,3,3),
  Idx = TRUE
)
t10 <- list(
  Num = 10,
  Len = 96,
  Strt = as.Date("1997-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0434-02",
  Titl = "Gross domestic product by industry, chained Fisher, part 2",
  Ftnt = "Source: Statistics Canada table 36-10-0434-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_10,
  Indent= c(3,3,3,3,2,3,3,2,3,3,2,3,3,3,3,3,2,3,3,3,3,3,3,3,3,2,3,3,3,3,3,3,3,2,3,3,3,3,3,2,3,3,3,3,2,3,4,4,4,3,3,3,3,3,2,3,3,3,2,3,3,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,3),
  Idx = TRUE
)
t11 <- list(
  Num = 11,
  Len = 95,
  Strt = as.Date("1997-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0434-03",
  Titl = "Gross domestic product by industry, chained Fisher, part 3",
  Ftnt = "Source: Statistics Canada table 36-10-0434-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_11,
  Indent= c(3,3,2,3,3,2,2,3,3,2,1,2,2,2,3,3,2,2,2,1,2,2,3,2,2,3,3,2,3,3,2,1,2,3,3,2,2,2,3,3,2,1,2,3,3,2,2,2,2,2,2,2,2,1,1,2,3,3,3,3,3,3,3,3,1,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,1,2,2,1,2,2,2,2,1,2,3,3,2,2,2),
  Idx = TRUE
)
t12 <- list(
  Num = 12,
  Len = 89,
  Strt = as.Date("1981-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "36-10-0018-01",
  Titl = "Balance of international payments, current account",
  Ftnt = "Source: Statistics Canada table 36-10-0018-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_12,
  Indent= c(0,1,2,2,3,3,3,3,1,2,2,3,4,4,5,5,3,4,5,5,4,3,1,2,3,2,3,3,0,1,2,2,3,3,3,3,1,2,2,3,4,4,5,5,3,4,5,5,4,3,1,2,3,4,4,3,3,4,4,0,1,2,2,3,3,3,3,1,2,2,3,4,4,4,4,3,4,4,4,4,3,1,2,3,3,2,3,3,3),
  Idx = FALSE
)
t13 <- list(
  Num = 13,
  Len = 60,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0108-01",
  Titl = "Gross fixed capital formation",
  Ftnt = "Source: Statistics Canada table 36-10-0108-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_13,
  Indent= c(0,1,2,3,4,4,4,3,4,4,2,3,3,3,3,3,3,3,3,3,2,3,3,3,2,1,2,3,3,4,4,2,3,3,3,3,3,3,3,3,3,2,2,3,3,2,1,2,3,3,2,2,2,0,1,2,2,1,1,1),
  Idx = FALSE
)
t14 <- list(
  Num = 14,
  Len = 60,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0108-02",
  Titl = "Gross fixed capital formation, chained Fisher",
  Ftnt = "Source: Statistics Canada table 36-10-0108-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_14,
  Indent= c(0,1,2,3,4,4,4,3,4,4,2,3,3,3,3,3,3,3,3,3,2,3,3,3,2,1,2,3,3,4,4,2,3,3,3,3,3,3,3,3,3,2,2,3,3,2,1,2,3,3,2,2,2,0,1,2,2,1,1,1),
  Idx = TRUE
)
t15 <- list(
  Num = 15,
  Len = 43,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0477-01",
  Titl = "Revenue, expenditure and budgetary balance - general governments",
  Ftnt = "Source: Statistics Canada table 36-10-0477-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_15,
  Indent= c(0,1,2,3,3,2,2,1,1,2,2,1,1,1,1,1,1,2,2,2,2,0,1,1,1,1,2,2,1,1,2,2,2,2,1,0,1,1,2,3,3,2,0),
  Idx = FALSE 
)
t16 <- list(
  Num = 16,
  Len = 128,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0477-02",
  Titl = "Revenue, expenditure and budgetary balance - federal government",
  Ftnt = "Source: Statistics Canada table 36-10-0477-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_16,
  Indent= c(0,1,2,3,3,2,2,1,2,1,2,3,2,3,3,4,3,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,1,1,1,2,3,3,3,2,2,1,1,2,2,2,1,1,2,2,2,2,3,4,4,4,3,3,2,0,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,3,3,2,3,3,1,2,3,4,4,4,4,4,4,4,4,4,3,3,3,2,3,3,3,3,1,2,2,1,2,2,2,2,3,4,4,4,3,3,2,1,0,1,1,1,2,2,1,0),
  Idx = FALSE 
)
t17 <- list(
  Num = 17,
  Len = 56,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0477-03",
  Titl = "Revenue, expenditure and budgetary balance - provincial governments",
  Ftnt = "Source: Statistics Canada table 36-10-0477-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_17,
  Indent= c(0,1,2,3,3,2,2,1,2,2,1,1,1,2,2,2,1,1,1,2,2,2,2,3,3,3,0,1,1,1,1,2,2,1,2,2,2,1,1,2,2,2,2,3,3,3,2,1,0,1,1,2,3,3,2,0),
  Idx = FALSE 
)
t18 <- list(
  Num = 18,
  Len = 25,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0114-01",
  Titl = "Compensation of employees",
  Ftnt = "Source: Statistics Canada table 36-10-0114-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_18,
  Indent= c(0,1,1,0,1,2,3,3,3,3,3,2,3,3,3,3,3,3,3,3,4,4,3,3,1),
  Idx = FALSE 
)
t19 <- list(
  Num = 19,
  Len = 16,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0117-01",
  Titl = "Undistributed corporate profits",
  Ftnt = "Source: Statistics Canada table 36-10-0117-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_19,
  Indent= c(0,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0),
  Idx = FALSE 
)
t20 <- list(
  Num = 20,
  Len = 19,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0126-01",
  Titl = "Property income of households",
  Ftnt = "Source: Statistics Canada table 36-10-0126-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_20,
  Indent= c(0,1,2,2,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1),
  Idx = FALSE
)

t21 <- list(
  Num = 21,
  Len = 23,
  Strt = as.Date("2015-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "36-10-0412-01",
  Titl = "International investment position, market value",
  Ftnt = "Source: Statistics Canada table 36-10-0412-01.",
  Units = "Millions of dollars",
  Seas = "Not seasonally adjusted",
  Nchoices = ser_21,
  Indent= c(0,1,1,2,2,1,1,2,2,2,2,0,1,1,2,2,1,2,2,2,2,2,0),
  Idx = FALSE
)

t22 <- list(
  Num = 22,
  Len = 14,
  Strt = as.Date("1981-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0206-01",
  Titl = "Indexes of business total labour productivity and related measures",
  Ftnt = "Source: Statistics Canada table 36-10-0206-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_22,
  Indent= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Idx = TRUE
)

t23 <- list(
  Num = 23,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-01",
  Titl = "Indexes of business real gross domestic product by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_23,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t24 <- list(
  Num = 24,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-02",
  Titl = "Indexes of business jobs by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_24,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t25 <- list(
  Num = 25,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-03",
  Titl = "Indexes of business average hours worked by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_25,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t26 <- list(
  Num = 26,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-04",
  Titl = "Indexes of business total hours worked by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_26,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t27 <- list(
  Num = 27,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-05",
  Titl = "Indexes of business labour productivity by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_27,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t28 <- list(
  Num = 28,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-06",
  Titl = "Indexes of business labour compensation per hour by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_28,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t29 <- list(
  Num = 29,
  Len = 21,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-07",
  Titl = "Indexes of business unit labour cost by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_29,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t30 <- list(
  Num = 30,
  Len = 20,
  Strt = as.Date("1997-01-01"),
  DecPlac = 1,
  RateFctr = 4,
  STCno = "36-10-0207-08",
  Titl = "Indexes of business unit labour cost in US dollars by industry",
  Ftnt = "Source: Statistics Canada table 36-10-0207-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_30,
  Indent= c(0,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1),
  Idx = TRUE
)

t31 <- list(
  Num = 31,
  Len = 18,
  Strt = as.Date("1961-01-01"),
  DecPlac = 3,
  RateFctr = 4,
  STCno = "36-10-0105-01",
  Titl = "Gross national income and gross domestic income, indexes and related statistics",
  Ftnt = "Source: Statistics Canada table 36-10-0105-01.",
  Units = "Index with 2012 = 100",
  Seas = "Seasonally adjusted",
  Nchoices = ser_31,
  Indent= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Idx = TRUE
)

t32 <- list(
  Num = 32,
  Len = 26,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0109-01",
  Titl = "Investment in inventories, chained Fisher",
  Ftnt = "Source: Statistics Canada table 36-10-0109-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_32,
  Indent= c(0,1,2,3,4,4,3,4,5,5,5,4,5,5,3,3,2,3,3,4,4,3,1,1,0,0),
  Idx = TRUE
)

t33 <- list(
  Num = 33,
  Len = 48,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0112-01",
  Titl = "Current and capital accounts - Households",
  Ftnt = "Source: Statistics Canada table 36-10-0112-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_33,
  Indent= c(0,1,1,0,1,1,1,0,1,1,0,0,0,1,1,1,2,2,2,1,0,1,1,1,2,2,3,1,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t34 <- list(
  Num = 34,
  Len = 33,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0115-01",
  Titl = "Current and capital accounts - Non-profit institutions serving households",
  Ftnt = "Source: Statistics Canada table 36-10-0115-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_34,
  Indent= c(0,1,1,0,0,0,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t35 <- list(
  Num = 35,
  Len = 45,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0116-01",
  Titl = "Current and capital accounts - Corporations",
  Ftnt = "Source: Statistics Canada table 36-10-0116-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_35,
  Indent= c(0,0,1,2,2,2,2,2,1,2,2,2,2,2,2,0,0,0,1,1,1,0,1,1,1,2,1,0,0,0,0,0,0,0,1,1,1,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t36 <- list(
  Num = 36,
  Len = 42,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0118-01",
  Titl = "Current and capital accounts - General government",
  Ftnt = "Source: Statistics Canada table 36-10-0118-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_36,
  Indent= c(0,1,1,1,1,0,1,1,0,0,0,1,2,2,2,1,1,1,2,2,0,1,1,1,0,0,0,0,0,0,0,1,1,1,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t37 <- list(
  Num = 37,
  Len = 44,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0121-01",
  Titl = "Current and capital accounts - Non-residents",
  Ftnt = "Source: Statistics Canada table 36-10-0121-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_37,
  Indent= c(0,1,1,1,1,0,1,2,3,3,2,2,3,3,1,2,3,3,2,2,3,3,2,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0),
  Idx = FALSE
)

t38 <- list(
  Num = 38,
  Len = 56,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0118-02",
  Titl = "Current and capital accounts - Federal general government",
  Ftnt = "Source: Statistics Canada table 36-10-0118-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_38,
  Indent= c(0,1,1,1,1,0,1,1,0,0,0,1,2,2,3,1,1,1,2,2,2,1,2,2,0,1,1,1,2,2,2,1,0,0,0,0,0,0,0,1,1,1,2,2,1,2,2,2,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t39 <- list(
  Num = 39,
  Len = 52,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0118-03",
  Titl = "Current and capital accounts - Provincial and territorial general governments",
  Ftnt = "Source: Statistics Canada table 36-10-0118-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_39,
  Indent= c(0,1,1,1,1,0,1,1,0,0,0,1,2,2,3,1,1,1,2,2,2,1,0,1,1,1,2,2,2,1,0,0,0,0,0,0,0,1,1,1,1,2,2,2,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t40 <- list(
  Num = 40,
  Len = 49,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0118-04",
  Titl = "Current and capital accounts - Local general governments",
  Ftnt = "Source: Statistics Canada table 36-10-0118-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_40,
  Indent= c(0,1,1,1,1,0,1,1,0,0,0,1,1,1,1,2,2,2,1,0,1,1,1,2,2,2,1,0,0,0,000,0,0,0,1,1,1,1,2,2,2,1,0,0,1,2,2,1,0),
  Idx = FALSE
)

t41 <- list(
  Num = 41,
  Len = 24,
  Strt = as.Date("1961-01-01"),
  DecPlac = 0,
  RateFctr = 1,
  STCno = "36-10-0109-02",
  Titl = "Investment in inventories",
  Ftnt = "Source: Statistics Canada table 36-10-0109-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_41,
  Indent= c(0,1,2,3,4,4,3,4,5,5,5,4,5,5,3,3,2,3,3,4,4,3,1,1),
  Idx = FALSE
)

t42 <- list(
  Num = 42,
  Len = 48,
  Strt = as.Date("1988-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "12-10-0122-01",
  Titl = "Merchandise exports by commodity",
  Ftnt = "Source: Statistics Canada table 12-10-0122-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_42,
  Indent= c(0,1,2,2,1,2,2,2,2,2,1,2,2,1,2,2,2,2,1,2,2,2,1,2,2,2,2,1,1,1,2,2,2,1,2,2,2,1,2,2,1,2,2,2,2,2,2,1),
  Idx = FALSE
)

t43 <- list(
  Num = 43,
  Len = 48,
  Strt = as.Date("1988-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "12-10-0122-02",
  Titl = "Merchandise imports by commodity",
  Ftnt = "Source: Statistics Canada table 12-10-0122-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_43,
  Indent= c(0,1,2,2,1,2,2,2,2,2,1,2,2,1,2,2,2,2,1,2,2,2,1,2,2,2,2,1,1,1,2,2,2,1,2,2,2,1,2,2,1,2,2,2,2,2,2,1),
  Idx = FALSE
)

t44 <- list(
  Num = 44,
  Len = 14,
  Strt = as.Date("1997-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "12-10-0124-01",
  Titl = "Merchandise exports by commodity, chain Fisher",
  Ftnt = "Source: Statistics Canada table 12-10-0124-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_44,
  Indent= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Idx = TRUE
)

t45 <- list(
  Num = 45,
  Len = 14,
  Strt = as.Date("1997-01-01"),
  DecPlac = 0,
  RateFctr = 4,
  STCno = "12-10-0124-02",
  Titl = "Merchandise imports by commodity, chain Fisher",
  Ftnt = "Source: Statistics Canada table 12-10-0124-01.",
  Units = "Millions of dollars",
  Seas = "Seasonally adjusted",
  Nchoices = ser_45,
  Indent= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Idx = TRUE
)

TS <- list(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,
  t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,
  t37,t38,t39,t40,t41,t42,t43,t44,t45)
