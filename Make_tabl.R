# Make_tabl() function
# April 26, 2021

#===============================================================================
# Make_tabl - function to create q0 data frame and inputs to Table() function
# tabno - the number of the table to draw (1,2,...numTabs)
# type - the choice of transformation (1,2,3,4,5)
# qtr1 - first quarter for table (a Date variable)
# qtr2 - last quarter for table (a Date variable)
#===============================================================================

# NOTE: The data for tables are stored and must be updated once per quarter,
# along with GDPdf.rds, GDPNSAdf.rds and the lastdate variable

library(gt)
library(tidyverse)
library(lubridate)

lastdate <- "2020-10-01" # last date for which data are available in the app
numTabs <- 20 # Number of tables available in the app
GDPdf <- readRDS("rds/GDPdf.rds") # GDP is used in most tables
GDPNSAdf <- readRDS("rds/GDPNSAdf.rds") # GDP is used in most tables
tcol <- "#D7E3E9"  # table colour

# A list with parametric info about each table
Tables <- list(
  STCno = c("36-10-0103-01","36-10-0104-01","36-10-0104-02","36-10-0106-01",
    "36-10-0122-01","36-10-0111-01","36-10-0124-01","36-10-0124-02",
    "36-10-0434-01","36-10-0434-02","36-10-0434-03","36-10-0016-01",
    "36-10-0108-01","36-10-0108-02","36-10-0477-01","36-10-0477-02",
    "36-10-0477-03","36-10-0114-01","36-10-0117-01","36-10-0126-01"),
  Titl = c("Gross domestic product, income-based",
    "Gross domestic product, expenditure-based",
    "Gross domestic product, expenditure-based, chained Fisher",
    "Gross domestic product, implicit price indexes",
    "Gross domestic income, gross national income and net national income",
    "Current and capital accounts, national",
    "Detailed household final consumption expenditure",
    "Detailed household final consumption expenditure, 2012 constant prices",
    "Gross domestic product by industry, chained Fisher, part 1",
    "Gross domestic product by industry, chained Fisher, part 2",
    "Gross domestic product by industry, chained Fisher, part 3",
    "Balance of international payments, current and capital accounts",
    "Gross fixed capital formation",
    "Gross fixed capital formation, chained Fisher",
    "Revenue, expenditure and budgetary balance - general governments",
    "Revenue, expenditure and budgetary balance - federal government",
    "Revenue, expenditure and budgetary balance - provincial governments",
    "Compensation of employees",
    "Undistributed corporate profits",
    "Property income of households"),
#     1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
    Indent= c(c(0,1,1,0,1,1,1,0,1,1,0,0,0,0), #starts at 1
    c(0,1,2,3,3,3,2,1,1,0,1,2,2,3,3,2,1,1,0,1,2,2,0,1,1,0,1,1,0,0,0), #starts at 15
    c(0,1,2,3,3,3,2,1,1,0,1,2,2,3,3,2,1,1,0,1,2,2,0,1,1,0,1,1,0,0,0), #starts at 46
    c(0,1,2,3,3,3,2,1,1,0,1,2,2,3,3,2,1,1,0,1,1,0,1,1,0,0), #starts at 77
    c(0,0,0,0,0,0,0,0,0,0,0,0), #starts at 103
    c(0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1), #starts at 115
    c(0,1,2,2,1,2,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2),
    c(0,1,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1),
    c(0,1,1,1,2,2,1,2,2,1,1,1,1,2,2,1,1,1,1,1,1,2,2,1,1,2,3,4,4,4,4,3,2,2,2,1,2,3,3,2,3,3,4,4,4,4,3,4,4,4,4,4,3,1,2,2,2,1,2,2,2,2,1,2,3,3,3,3,3,3,3,3,3,2,3,3,3,3,2,3,3,2,3,3,3,2,3,3,2,2,3,3,2,3,3,3),
    c(3,3,3,3,2,3,3,2,3,3,2,3,3,3,3,3,2,3,3,3,3,3,3,3,3,2,3,3,3,3,3,3,3,2,3,3,3,3,3,2,3,3,3,3,2,3,4,4,4,3,3,3,3,3,2,3,3,3,2,3,3,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,3),
    c(3,3,2,3,3,2,2,3,3,2,1,2,2,2,3,3,2,2,2,1,2,2,3,2,2,3,3,2,3,3,2,1,2,3,3,2,2,2,3,3,2,1,2,3,3,2,2,2,2,2,2,2,2,1,1,2,3,3,3,3,3,3,3,3,1,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,1,2,2,1,2,2,2,2,1,2,3,3,2,2,2),
    c(0,1,2,2,3,3,3,4,4,1,2,2,3,3,3,1,2,2,2,3,3,0,0,1,2,2,3,3,3,4,4,1,2,2,3,3,3,1,2,3,4,4,3,2,3,3,0,0,1,2,2,3,3,3,4,4,1,2,2,3,3,3,1,2,3,3,2,3,3,3,0,0),
    c(0,1,2,3,4,4,4,3,4,4,2,3,3,3,3,3,3,3,3,3,2,3,3,3,2,1,2,3,3,4,4,2,3,3,3,3,3,3,3,3,3,2,2,3,3,2,1,2,3,3,2,2,2,0,1,2,2,1,1,1),
    c(0,1,2,3,4,4,4,3,4,4,2,3,3,3,3,3,3,3,3,3,2,3,3,3,2,1,2,3,3,4,4,2,3,3,3,3,3,3,3,3,3,2,2,3,3,2,1,2,3,3,2,2,2,0,1,2,2,1,1,1),
    c(0,1,2,3,3,2,2,1,1,2,2,1,1,1,1,1,1,2,2,2,2,0,1,1,1,1,2,2,1,1,2,2,2,2,1,0,1,1,2,3,3,2,0),
    c(0,1,2,3,3,2,2,1,2,1,2,3,2,3,3,4,3,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,1,1,1,2,3,3,3,2,2,1,1,2,2,2,1,1,2,2,2,2,3,4,4,4,3,3,2,0,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,3,3,2,3,3,1,2,3,4,4,4,4,4,4,4,4,4,3,3,3,2,3,3,3,3,1,2,2,1,2,2,2,2,3,4,4,4,3,3,2,1,0,1,1,1,2,2,1,0),
    c(0,1,2,3,3,2,2,1,2,2,1,1,1,2,2,2,1,1,1,2,2,2,2,3,3,3,0,1,1,1,1,2,2,1,2,2,2,1,1,2,2,2,2,3,3,3,2,1,0,1,1,2,3,3,2,0),
    c(0,1,1,0,1,2,3,3,3,3,3,2,3,3,3,3,3,3,3,3,4,4,3,3,1), # 25 series
    c(0,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0), # 16 series
    c(0,1,2,2,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1)), # 19 series
  Istart=c(1,15,46,77,103,115,162,278,409,505,601,696,768,828,888,931,1059,1115,1140,1156),
  Ftnts=c(paste0("Source: Statistics Canada table 36-10-0103-01."),
          paste0("Source: Statistics Canada table 36-10-0104-01."),
          paste0("Source: Statistics Canada table 36-10-0104-01."),
          paste0("Source: Statistics Canada table 36-10-0106-01."),
          paste0("Source: Statistics Canada table 36-10-0122-01."),
          paste0("Source: Statistics Canada table 36-10-0111-01."),
          paste0("Source: Statistics Canada table 36-10-0124-01."),
          paste0("Source: Statistics Canada table 36-10-0124-01."),
          paste0("Source: Statistics Canada table 36-10-0434-01."),
          paste0("Source: Statistics Canada table 36-10-0434-01."),
          paste0("Source: Statistics Canada table 36-10-0434-01."),
          paste0("Source: Statistics Canada table 36-10-0016-01."),
          paste0("Source: Statistics Canada table 36-10-0108-01."),
          paste0("Source: Statistics Canada table 36-10-0108-01."),
          paste0("Source: Statistics Canada table 36-10-0477-01."),
          paste0("Source: Statistics Canada table 36-10-0477-01."),
          paste0("Source: Statistics Canada table 36-10-0477-01."),
          paste0("Source: Statistics Canada table 36-10-0114-01."),
          paste0("Source: Statistics Canada table 36-10-0117-01."),
          paste0("Source: Statistics Canada table 36-10-0126-01."))
)

#===============================================================================
# Make_tabl - function to create q0 data frame and inputs to Table() function
# tabno - the number of the table to draw (1,2,...numTabs)
# type - the choice of transformation
# qtr1 - first quarter for table
# qtr2 - last quarter for table
#===============================================================================
Make_tabl <- function(tabno,type,qtr1,qtr2) {
  if (tabno==1) { GDP <- filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==2)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==3)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==4)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==5)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==6)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==7)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1981-01-01"))$VALUE
  } else if (tabno==8)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1981-01-01"))$VALUE
  } else if (tabno==9)  { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1997-01-01"))$VALUE
  } else if (tabno==10) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1997-01-01"))$VALUE
  } else if (tabno==11) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1997-01-01"))$VALUE
  } else if (tabno==12) { GDP <-  filter(GDPNSAdf,REF_DATE>=as.Date("1981-01-01"))$VALUE
  } else if (tabno==13) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==14) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==15) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==16) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==17) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==18) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==19) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  } else if (tabno==20) { GDP <-  filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))$VALUE
  }
  # Get ... the data
  q0 <- readRDS(paste0("rds/",Tables$STCno[tabno],".rds"))
  # Get ... the table title
  tbl_nam <- Tables$Titl[tabno]
  # Get ... the table footnote
  ftnt <- Tables$Ftnts[tabno]
  # Get ... the table indent structure
  if (tabno<numTabs) { 
    indt <- c(Tables$Indent[Tables$Istart[tabno]:(Tables$Istart[tabno+1]-1)])
  } else {
    indt <- c(Tables$Indent[Tables$Istart[tabno]:length(Tables$Indent)]) 
  }
  # Get ... the sequence of dates (Date class) for the table columns
  colnam1 <- seq.Date(qtr1,qtr2,by="quarter")
  # Get ... the sequence of dates (character class) for the table columns
  colnam2 <- vector()
  for (i in 1:length(colnam1)) {
    Year <- year(colnam1[i])
    Month <- month(colnam1[i])
    qtr <- case_when(
      Month==1  ~ 1,
      Month==4  ~ 2,
      Month==7  ~ 3,
      Month==10 ~ 4
    )
    colnam2[i] <- paste0(Year," Q",qtr)
  }
  # Transform the data according to the chosen type
  if(type==1) { # Display original data
    q1 <- q0
  } else if (type==2) { # Display indexed data
    q1 <- filter(q0,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*x/x[1],1)}))
  } else if(type==3) { # Display 1-qtr % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/lag(x)-1),1)}))
  } else if(type==4) { # Display 4-qtr % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/lag(x,4)-1),1)}))
  } else if(type==5) { # Display % of GDP
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/GDP),1)}))
  }
  # Transpose the data frame and make it ready for the gt() function
  tbl_df <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
  tbl_df <- as.data.frame(t(tbl_df))
  tbl_df <- mutate(tbl_df,Components=rownames(tbl_df))
  colnames(tbl_df) <- c(colnam2,"Components")
  tbl_df <- tbl_df[2:nrow(tbl_df),]
  rownames(tbl_df) <- NULL
  tbl_df <- select(tbl_df,Components,everything())
  tbl_df <- mutate(tbl_df,across(2:ncol(tbl_df),as.numeric))
  colnam <- colnames(tbl_df)
  colnamx <- colnam[2:length(colnam)]
  ncols <- length(colnamx)
  # set the indentation (maximum 4 indents)
  indt1 <- numeric(); indt2 <- numeric(); indt3 <- numeric(); indt4 <- numeric()
  j1 <- 0; j2 <- 0; j3 <- 0; j4 <- 0
  for(i in 1:length(indt)) {
    if (indt[i]==1) {
      j1 <- j1+1
      indt1[j1] <- i
    }
    if (indt[i]==2) {
      j2 <- j2+1
      indt2[j2] <- i
    }
    if (indt[i]==3) {
      j3 <- j3+1
      indt3[j3] <- i
    }
    if (indt[i]==4) {
      j4 <- j4+1
      indt4[j4] <- i
    }
  }
  # set the type variables
  subtitle1=case_when(
        type==1 ~ paste0("Millions of dollars<br>",
          "Seasonally adjusted at annual rates<br><br>"),
        type==2 ~ paste0("Indexed to ",colnamx[1]," = 100<br>",
          "Seasonally adjusted<br><br>"),
        type==3 ~ paste0("One-quarter percentage change<br>",
          "Seasonally adjusted<br><br>"),
        type==4 ~ paste0("Four-quarter percentage change<br>",
          "Seasonally adjusted<br><br>"),
        type==5 ~ paste0("Percentage of gross domestic product<br>",
          "Seasonally adjusted<br><br>")
  )
  if (tabno==12) { # special case
    subtitle1=case_when(
          type==1 ~ paste0("Millions of dollars<br>",
            "Not seasonally adjusted<br><br>"),
          type==2 ~ paste0("Indexed to ",colnamx[1]," = 100<br>",
            "Not seasonally adjusted<br><br>"),
          type==3 ~ paste0("One-quarter percentage change<br>",
            "Not seasonally adjusted<br><br>"),
          type==4 ~ paste0("Four-quarter percentage change<br>",
            "Not seasonally adjusted<br><br>"),
          type==5 ~ paste0("Percentage of gross domestic product<br>",
            "Not seasonally adjusted<br><br>")
    )    
  }
  if (tabno==7 & type==1) subtitle1 <- paste0("Millions of dollars<br>",
    "Seasonally adjusted at quarterly rates<br><br>") # special case
  if (tabno==7 & type==2) subtitle1 <- paste0("Indexed to ",colnamx[1]," = 100<br>",
    "Seasonally adjusted <br><br>") # special case
  if (tabno==8 & type==1) subtitle1 <- paste0("Millions of dollars<br>",
    "Seasonally adjusted at quarterly rates<br><br>") # special case
  if (tabno==8 & type==2) subtitle1 <- paste0("Indexed to ",colnamx[1]," = 100<br>",
    "Seasonally adjusted at quarterly rates<br><br>") # special case
  decs=case_when(
        type==1 ~ 0,
        type==2 ~ 1,
        type==3 ~ 1,
        type==4 ~ 1,
        type==5 ~ 1
  )
  if (tbl_nam=="Gross domestic product, implicit price indexes" & type==1) {
    decs <- 1 } # Special case: price indexes table
  gt_tbl <- gt(data=tbl_df)
  gt_tbl <- tab_options(gt_tbl,table.font.size=24,
      #container.width = 1800,
      table.background.color=tcol,
      heading.background.color=tcol)
  gt_tbl <- tab_header(gt_tbl,
      title=md(html(paste0("**",tbl_nam,"**"))),
      subtitle=md(html(paste0(subtitle1,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
      source_note=md(html(ftnt))) 
  gt_tbl <- cols_align(gt_tbl,
      align=c("left"),
      columns=vars(`Components`))
  gt_tbl <- cols_label(gt_tbl,
      Components="")
  gt_tbl <- tab_style(gt_tbl,
    style=list(
      cell_text(weight="bold")),
    locations=cells_column_labels(colnamx))
  gt_tbl <- tab_style(gt_tbl,
        style = cell_text(indent=pct(2)),
        locations = cells_body(
          columns = 1,
          rows = indt1))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(4)),
      locations = cells_body(
        columns = 1,
        rows = indt2))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(6)),
      locations = cells_body(
        columns = 1,
        rows = indt3))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(8)),
      locations = cells_body(
        columns = 1,
        rows = indt4))
  gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),
      decimals=decs,
      use_seps=TRUE)
  tbl <- gt_tbl
}
