# Make_tablA() function
# May 11, 2021

#===============================================================================
# Make_tablA - function to generate an annual gt() table
# tabno - the number of the table to draw (1,2,...numTabs)
# type - the choice of transformation (1,2,3,4,5)
# qtr1 - first quarter for table (a Date variable)
# qtr2 - last quarter for table (a Date variable)
#===============================================================================

# NOTE: The data for tables are stored and must be updated once per quarter,
# along with GDPdf.rds and the lastdate variable

library(gt)
library(tidyverse)
library(lubridate)

source("Tabl_specs.R")

GDPdf <- readRDS("rds/GDPdf.rds") # GDP is used in most tables
GDPHdf <- readRDS("rds/GDPHdf.rds") # GDPH is used in historical tables

#===============================================================================
# Make_tabl - function to create q0 data frame and inputs to Table() function
# tabno - the number of the table to draw (1,2,...numTabs)
# type - the choice of transformation
# qtr1 - first quarter for table
# qtr2 - last quarter for table
#===============================================================================
Make_tablA <- function(tabno,type,year1,year2) {
  year1 <- as.numeric(year1)
  year2 <- as.numeric(year2)
  if (TS[[tabno]]$TblType=="Current") {
    eqtr <- quarter(as.Date(GDPdf$REF_DATE[nrow(GDPdf)]))
    eyr <- year(as.Date(GDPdf$REF_DATE[nrow(GDPdf)]))
    if (eqtr!=4) {
      eqtr <- 1
      eyr <- eyr-1
    }
    qtr1 <- as.Date("1961-01-01")
    qtr2 <- as.Date(paste0(eyr,"-",(1+(eqtr-1)*3),"-01"))
    GDPdf1 <- filter(GDPdf,(REF_DATE>=qtr1 & REF_DATE<=qtr2))
    GDPdf1 <- mutate(GDPdf1,Year=year(REF_DATE),Quarter=quarter(REF_DATE))
    GDPdf1 <- GDPdf1 %>% group_by(Year) %>% summarise(GDP=mean(VALUE))
    GDPdf1 <- filter(GDPdf1,Year>=year1 & Year<=year2)
    GDP <- GDPdf1$GDP
  } else {
    eqtr <- quarter(as.Date(GDPHdf$REF_DATE[nrow(GDPHdf)]))
    eyr <- year(as.Date(GDPHdf$REF_DATE[nrow(GDPHdf)]))
    if (eqtr!=4) {
      eqtr <- 1
      eyr <- eyr-1
    }
    qtr1 <- as.Date("1961-01-01")
    qtr2 <- as.Date(paste0(eyr,"-",(1+(eqtr-1)*3),"-01"))
    GDPHdf1 <- filter(GDPHdf,(REF_DATE>=qtr1 & REF_DATE<=qtr2))
    GDPHdf1 <- mutate(GDPHdf1,Year=year(REF_DATE),Quarter=quarter(REF_DATE))
    GDPHdf1 <- GDPHdf1 %>% group_by(Year) %>% summarise(GDP=mean(VALUE))
    GDPHdf1 <- filter(GDPHdf1,Year>=year1 & Year<=year2)
    GDP <- GDPHdf1$GDP
  }
  q0 <- readRDS(paste0("rds/",TS[[tabno]]$STCno,".rds"))
  # Drop hanging quarters
  iqtr <- quarter(q0$REF_DATE[1])
  iyr <- year(q0$REF_DATE[1])
  if (iqtr!=1) {
    iqtr <- 1
    iyr <- iyr+1
  }
  eqtr <- quarter(q0$REF_DATE[nrow(q0)])
  eyr <- year(q0$REF_DATE[nrow(q0)])
  if (eqtr!=4) {
    eqtr <- 4
    eyr <- eyr-1
  }
  qtr1 <- as.Date(paste0(iyr,"-",iqtr,"-01"))
  qtr2 <- as.Date(paste0(eyr,"-",(1+(eqtr-1)*3),"-01"))
  q1 <- filter(q0,(REF_DATE>=qtr1 & REF_DATE<=qtr2))
  q1 <- mutate(q1,Year=year(REF_DATE),Quarter=quarter(REF_DATE))
  if (TS[[tabno]]$RateFctr==1 | (TS[[tabno]]$Idx==TRUE)) {
    q1 <- q1 %>% group_by(Year) %>% summarise(across(2:(ncol(q1)-2),mean))
  } else if (TS[[tabno]]$RateFctr==4) {
    q1 <- q1 %>% group_by(Year) %>% summarise(across(2:(ncol(q1)-2),sum))
  } else if (TS[[tabno]]$RateFctr==0) {
    q1 <- q1 %>% group_by(Year) %>% summarise(across(2:(ncol(q1)-2),
      function(x) {y <- x[4]}))
  }
  yr1 <- q1$Year[1]
  yr2 <- q1$Year[nrow(q1)]
  # set the indentation (maximum 5 indents)
  indt1 <- numeric(); indt2 <- numeric(); indt3 <- numeric(); 
  indt4 <- numeric(); indt5 <- numeric()
  j1 <- 0; j2 <- 0; j3 <- 0; j4 <- 0; j5 <- 0
  indt <- TS[[tabno]]$Indent
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
    if (indt[i]==5) {
      j5 <- j5+1
      indt5[j5] <- i
    }
  }
  if (TS[[tabno]]$RateFctr==1 & type==1)
    subtitle1 <- paste0(TS[[tabno]]$Units,"<br><br>")
  decs <- TS[[tabno]]$DecPlac
  if (type==2 | type==3 | type==4) decs <- 1
  if(type==1) { # Display original data
    q2 <- filter(q1,Year>=year1 & Year<=year2)
  } else if (type==2) { # Display indexed data
    q2 <- filter(q1,Year>=year1 & Year<=year2)
    q2 <- mutate(q2,across(2:ncol(q2),function(x) 
      {y <- round(100*x/x[1],1)}))
  } else if(type==3) { # Display 1-qtr % changes
    q2 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*(x/lag(x)-1),1)}))
    q2 <- filter(q2,Year>=year1 & Year<=year2)
  } else if(type==4) { # Display % of GDP
    q2 <- filter(q1,Year>=year1 & Year<=year2)
    q2 <- mutate(q2,across(2:ncol(q1),function(x) 
      {y <- round(100*(x/GDP),1)}))
  } else if(type==5) { # Display 3-year centred moving average
    q2 <- mutate(q1,across(2:ncol(q1),function(x) {
        y <- round((lag(x,1)+x+lead(x,1))/3,1)
        n <- length(x)
        y[1] <- x[1]
        y[n] <- x[n]
        return(y)
    }))
    q2 <- filter(q1,Year>=year1 & Year<=year2)
  }
  # set the type variables
  subtitle1=case_when(
        type==1 ~ paste0(TS[[tabno]]$Units,"<br><br>"),
        type==2 ~ paste0("Indexed to ",q2$Year[1]," = 100<br><br>"),
        type==3 ~ paste0("One-year percentage change<br><br>"),
        type==4 ~ paste0("Percentage of gross domestic product<br><br>"),
        type==5 ~ paste0(TS[[tabno]]$Units,
          ", three-year centred moving average<br><br>")
  )
  colnam2 <- c(year1:year2)
  # Transpose the data frame and make it ready for the gt() function
  tbl_df <- filter(q2,Year>=year1 & Year<=year2)
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

  gt_tbl <- gt(data=tbl_df)
  gt_tbl <- tab_options(gt_tbl,table.font.size=24,
      #container.width = 1800,
      table.background.color=tcol,
      heading.background.color=tcol)
  gt_tbl <- tab_header(gt_tbl,
      title=md(html(paste0("**",TS[[tabno]]$Titl,"**"))),
      subtitle=md(html(paste0(subtitle1,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
      source_note=md(html(TS[[tabno]]$Ftnt))) 
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
        style = cell_text(indent=pct(1.5)),
        locations = cells_body(
          columns = 1,
          rows = indt1))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(3)),
      locations = cells_body(
        columns = 1,
        rows = indt2))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(4.5)),
      locations = cells_body(
        columns = 1,
        rows = indt3))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(6)),
      locations = cells_body(
        columns = 1,
        rows = indt4))
  gt_tbl <- tab_style(gt_tbl,
      style = cell_text(indent=pct(7.5)),
      locations = cells_body(
        columns = 1,
        rows = indt5))
  gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),
      decimals=decs,
      use_seps=TRUE)
  #tbl <- gt_tbl
  tbl <- list(gt_tbl,tbl_df)
}
