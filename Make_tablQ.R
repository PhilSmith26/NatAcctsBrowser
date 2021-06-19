# Make_tablQ() function
# May 8, 2021

#===============================================================================
# Make_tabl - function to generate a gt() table
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
Make_tablQ <- function(tabno,type,qtr1,qtr2) {
  if (TS[[tabno]]$TblType=="Current") {
      if (TS[[tabno]]$RateFctr==0) {
          GDP <- filter(GDPdf,REF_DATE>=TS[[tabno]]$Strt)$VALUE
      } else {
          GDP <- filter(GDPdf,REF_DATE>=TS[[tabno]]$Strt)$VALUE/TS[[tabno]]$RateFctr
      }
    } else {
      GDP <- filter(GDPHdf,REF_DATE>=TS[[tabno]]$Strt)$VALUE/TS[[tabno]]$RateFctr
    }    
  q0 <- readRDS(paste0("rds/",TS[[tabno]]$STCno,".rds"))
  colnam1 <- seq.Date(qtr1,qtr2,by="quarter")
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
  # set the type variables
  subtitle1=case_when(
        type==1 ~ paste0(TS[[tabno]]$Units,"<br>",
          TS[[tabno]]$Seas,"<br><br>"),
        type==2 ~ paste0("Indexed to ",colnam2[1]," = 100<br>",
          TS[[tabno]]$Seas,"<br><br>"),
        type==3 ~ paste0("One-quarter percentage change<br>",
          TS[[tabno]]$Seas,"<br><br>"),
        type==4 ~ paste0("Four-quarter percentage change<br>",
          TS[[tabno]]$Seas,"<br><br>"),
        type==5 ~ paste0("Percentage of gross domestic product<br>",
          TS[[tabno]]$Seas,"<br><br>"),
        type==6 ~ paste0(TS[[tabno]]$Units,
          ", five-quarter centred moving average<br>",
          TS[[tabno]]$Seas,"<br><br>")
  )
  if (TS[[tabno]]$RateFctr==1 & type==1)
    subtitle1 <- paste0(TS[[tabno]]$Units,"<br>",
          "Seasonally adjusted at annual rates<br><br>")
  decs <- TS[[tabno]]$DecPlac
  if (type==2 | type==3 | type==4 | type==5) decs <- 1
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
  } else if(type==6) { # Display 5-qtr centred moving average
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {
        y <- round((lag(x,2)+lag(x,1)+x+lead(x,1)+lead(x,2))/5,1)
        n <- length(x)
        y[1] <- x[1]; 
        y[2] <- (x[1]+x[2]+x[3])/3
        y[n] <- x[n]
        y[n-1] <- (x[n-2]+x[n-1]+x[n])/3
        return(y)
      }
    ))
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
  for (i in 1:nrow(tbl_df)) {
    tbl_df[i,1] <- paste0(i,". ",tbl_df[i,1])
  }
  colnam <- colnames(tbl_df)
  colnamx <- colnam[2:length(colnam)]
  ncols <- length(colnamx)
  
  gt_tbl <- gt(data=tbl_df)
  gt_tbl <- tab_options(gt_tbl,table.font.size=24,
      table.background.color=tcol,
      heading.background.color=tcol)
  gt_tbl <- tab_header(gt_tbl,
      title=md(html(paste0("**","Table ",tordR[tabno],". ",
        TS[[tabno]]$Titl,"**"))),
      subtitle=md(html(paste0(subtitle1,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
      source_note=md(html(TS[[tabno]]$Ftnt))) 
  gt_tbl <- cols_align(gt_tbl,
      align=c("left"),
      columns=c(`Components`))
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
  if (tabno==51 & (type==1 | type==6)) {
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),
      rows=c(1:22),
      decimals=0,use_seps=TRUE)
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),
      rows=c(23:29),
      decimals=2,use_seps=FALSE)
  } else {
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),
      decimals=decs,
      use_seps=TRUE)  
  }
  tbl <- list(gt_tbl,tbl_df)
}
