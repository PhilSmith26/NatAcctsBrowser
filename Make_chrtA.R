# National accounts annual charts for shiny dashboard
# May 11, 2021

pkgs <- c("tidyverse","scales","tibble","stringr","rlang","lubridate")
inst <- lapply(pkgs,library,character.only=TRUE)

source("Tabl_specs.R")

IDX <- function(x) {y <- round(100*x/x[1],1)}
PC <- function(x) {y <- round(100*(x/lag(x)-1),1)}
Dgdp <- function(x) {y <- round(100*x/GDP,1)}
MA3 <- function(x) {
  y <- round((lag(x,1)+x+lead(x,1))/3,1)
  n <- length(x)
  y[1] <- x[1]; 
  y[n] <- x[n]
  return(y)
}
posNeg <- function(x) {sum(x>0,na.rm=TRUE)>0 & sum(x>0,na.rm=TRUE)<length(x)}

# Standard theme for charts
theme_DB <- function(base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170) {
  theme(
    plot.title = element_text(colour="black",size=14,face="bold",hjust=0),
    plot.subtitle = element_text(colour="black",size=14,hjust=0),
    panel.background = element_rect(fill="aliceblue",colour="black"),
    panel.border = element_rect(fill=NA,colour="black"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.95,size=14),
    axis.text.y = element_text(size=14),
    complete = TRUE
  )
}
GDPdf <- readRDS("rds/GDPdf.rds") # GDP is used in most tables
GDPHdf <- readRDS("rds/GDPHdf.rds") # GDPH is used in historical tables

#===============================================================================
# Make_chrtA - function to draw an annual national accounts chart
#   tabno - the table number
#   type - kind of transformation
#   year1 - first year for the chart
#   year2 - last year for the chart
#   MYtitl - name of a column in the data frame q0
#===============================================================================
Make_chrtA <- function(tabno,type,year1c,year2c,MYtitl,altTitl,interv1c) {
  interv <- as.numeric(interv1c)
  year1 <- as.numeric(year1c)
  year2 <- as.numeric(year2c)
  if (TS[[tabno]]$TblType=="Current") {
    eqtr <- quarter(as.Date(GDPdf$REF_DATE[nrow(GDPdf)]))
    eyr <- year(as.Date(GDPdf$REF_DATE[nrow(GDPdf)]))
    if (eqtr!=4) {
      eqtr <- 4
      eyr <- eyr-1
    }
    qtr1 <- as.Date("1961-01-01")
    qtr2 <- as.Date(paste0(eyr,"-",(1+(eqtr-1)*3),"-01"))
    GDPdf1 <- filter(GDPdf,(REF_DATE>=qtr1 & REF_DATE<=qtr2))
    GDPdf1 <- mutate(GDPdf1,Year=year(REF_DATE),Quarter=quarter(REF_DATE))
    GDPdf1 <- GDPdf1 %>% group_by(Year) %>% summarise(GDP=mean(VALUE))
    GDPdf1 <- filter(GDPdf1,Year>=year1 & Year<=year2)
    GDP <- GDPdf1$GDP
  } else if (TS[[tabno]]$TblType=="Historical") {
    eqtr <- quarter(as.Date(GDPHdf$REF_DATE[nrow(GDPHdf)]))
    eyr <- year(as.Date(GDPHdf$REF_DATE[nrow(GDPHdf)]))
    if (eqtr!=4) {
      eqtr <- 4
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
  # Check for NA's at start of this series
  # and change start date if necessary
  tmp1 <- mutate(q1,val=.data[[MYtitl]])
  tmp1 <- filter(tmp1,Year>=year1 & Year<=year2)
  n <- nrow(tmp1)
  i <- 1
  while (is.na(tmp1$val[i]) & i<n) {i <- i+1}
  year1 <- tmp1$Year[i]
  if (altTitl=="") {ChrtTitl <- MYtitl}
  if (altTitl!="") {ChrtTitl <- altTitl}
  if (type==1) {
    MYsubtitl=paste0(TS[[tabno]]$Units,"\nAnnual, ",year1," to ",year2)
    q2 <- mutate(q1,val=.data[[MYtitl]])
    q2 <- filter(q2,Year>=year1 & Year<=year2)
    c1 <- ggplot(q2,
      aes(x=Year,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"comma")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q2$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==2) {
    MYsubtitl=paste0("Including trend line\nAnnual, ",year1," to ",year2)
    q2 <- mutate(q1,val=.data[[MYtitl]])
    q2 <- filter(q2,Year>=year1 & Year<=year2)
    c1 <- ggplot(q2,
      aes(x=Year,y=val))+
      geom_line(colour="black",size=1.5)+
      geom_smooth(method="lm",se=FALSE,linetype="dashed")+
      scale_y_continuous(labels=scales::"comma")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q2$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==3) {
    MYsubtitl=paste0("Index, starting year = 100\nAnnual, ",year1," to ",year2)
    q2 <- filter(q1,Year>=year1 & Year<=year2)
    q2 <- mutate(q2,val=IDX(.data[[MYtitl]]))
    c1 <- ggplot(q2,
      aes(x=Year,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous()
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q2$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==4) {
    MYsubtitl=paste0("One-year percentage change\nAnnual, ",year1," to ",year2)
    q2 <- mutate(q1,val=PC(.data[[MYtitl]])/100)
    q2 <- filter(q2,Year>=year1 & Year<=year2)
    c1 <- ggplot(q2,aes(x=Year,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q2$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
 } else if (type==5) {
    MYsubtitl=paste0("Percentage of GDP\nAnnual, ",year1," to ",year2)
    q2 <- mutate(q1,val=100*.data[[MYtitl]]/(GDP*100))
    q2 <- filter(q2,Year>=year1 & Year<=year2)
    c1 <- ggplot(q2,aes(x=Year,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"percent")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q2$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==6) {
    MYsubtitl=paste0("Three-year centred moving average (dashed blue line)\nAnnual, ",
      year1," to ",year2)
    q2 <- mutate(q1,val=MA3(.data[[MYtitl]]))
    q2 <- filter(q2,Year>=year1 & Year<=year2)
    c1 <- ggplot(q2,aes(x=Year,y=val))+
      geom_line(colour="blue",size=1.5,linetype="dashed")+
      geom_line(aes(x=Year,y=.data[[MYtitl]]),colour="black",size=1.5)+ 
      scale_y_continuous(labels=scales::"comma")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q2$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  }
  if ((year2-year1)>18 & (interv1c)=="") {
      interv <- 3
    } else if ((year2-year1)>8 & (interv1c)=="") {
      interv <- 1
    }
  c1 <- c1 + scale_x_continuous(breaks=seq(year1,year2,by=interv))+
    labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),
      caption=TS[[tabno]]$Ftnt,x="",y="")+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1
}

