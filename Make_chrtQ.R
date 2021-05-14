# National accounts charts for shiny dashboard
# May 6, 2021

pkgs <- c("tidyverse","scales","tibble","stringr","rlang","lubridate")
inst <- lapply(pkgs,library,character.only=TRUE)

source("Tabl_specs.R")

IDX <- function(x) {y <- round(100*x/x[1],1)}
PC <- function(x) {y <- round(100*(x/lag(x)-1),1)}
PC4 <- function(x) {y <- round(100*(x/lag(x,4)-1),1)}
Dgdp <- function(x) {y <- round(100*x/GDP,1)}
MA5 <- function(x) {
  y <- round((lag(x,2)+lag(x,1)+x+lead(x,1)+lead(x,2))/5,1)
  n <- length(x)
  y[1] <- x[1]; 
  y[2] <- (x[1]+x[2]+x[3])/3
  y[n] <- x[n]
  y[n-1] <- (x[n-2]+x[n-1]+x[n])/3
  return(y)
}
posNeg <- function(x) {sum(x>0,na.rm=TRUE)>0 & sum(x>0,na.rm=TRUE)<length(x)}
datDif <- function(qtr1,qtr2) {
  y1 <- year(qtr1)
  q1 <- quarter(qtr1)
  y2 <- year(qtr2)
  q2 <- quarter(qtr2)
  distance <- as.numeric(y2)-as.numeric(y1)
}

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
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.95,size=10),
    axis.text.y = element_text(size=14),
    complete = TRUE
  )
}

#===============================================================================
# Make_chrt - function to draw a national accounts chart
#   tabno - the table number
#   type - kind of transformation
#   qtr1 - first date for the chart
#   qtr2 - last date for the chart
#   MYtitl - name of a column in the data frame q0
#===============================================================================
Make_chrtQ <- function(tabno,type,qtr1,qtr2,MYtitl,altTitl,interv) {
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
  
  # Check for NA's at start of this series
  # and change start date if necessary
  tmp1 <- mutate(q0,val=.data[[MYtitl]])
  tmp1 <- filter(tmp1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
  n <- nrow(tmp1)
  i <- 1
  while (is.na(tmp1$val[i]) & i<n) {i <- i+1}
  qtr1 <- tmp1$REF_DATE[i]
  
  if (altTitl=="") {ChrtTitl <- MYtitl}
  if (altTitl!="") {ChrtTitl <- altTitl}
  Fqtr <- paste0(year(qtr1)," Q",quarter(qtr1))
  Lqtr <- paste0(year(qtr2)," Q",quarter(qtr2))
  if (type==1) {
    MYsubtitl=paste0(TS[[tabno]]$Units,
      "\nQuarterly, ",Fqtr," to ",Lqtr,", ",TS[[tabno]]$Seas)
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"comma")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==2) {
    MYsubtitl=paste0("Including trend line\nQuarterly, ",Fqtr," to ",
      Lqtr,", ",TS[[tabno]]$Seas)
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      geom_smooth(method="lm",se=FALSE,linetype="dashed")+
      scale_y_continuous(labels=scales::"comma")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==3) {
    MYsubtitl=paste0("Index, starting quarter = 100\nQuarterly, ",Fqtr," to ",
      Lqtr,", ",TS[[tabno]]$Seas)
    q0 <- filter(q0,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    q1 <- mutate(q0,val=IDX(.data[[MYtitl]]))
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous()
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==4) {
    MYsubtitl=paste0("One-quarter percentage change\nQuarterly, ",Fqtr," to ",
      Lqtr,", ",TS[[tabno]]$Seas)
    q1 <- mutate(q0,val=PC(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
 } else if (type==5) {
    MYsubtitl=paste0("Four-quarter percentage change\nQuarterly, ",Fqtr," to ",
      Lqtr,", ",TS[[tabno]]$Seas)
    q1 <- mutate(q0,val=PC4(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==6) {
    MYsubtitl=paste0("Percentage of GDP\nQuarterly, ",Fqtr," to ",
      Lqtr,", ",TS[[tabno]]$Seas)
    q1 <- mutate(q0,val=100*.data[[MYtitl]]/(GDP*100))
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"percent")
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==7) {
    MYsubtitl=paste0("Five-quarter centred moving average (dashed blue line)\nQuarterly, ",
      Fqtr," to ",Lqtr,", ",TS[[tabno]]$Seas)
    q1 <- mutate(q0,val=MA5(.data[[MYtitl]]))
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="blue",size=1.5,linetype="dashed")+
      geom_line(aes(x=REF_DATE,y=.data[[MYtitl]]),colour="black",size=1.5)+ 
      scale_y_continuous(labels=scales::"comma")+
      #labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  }
  if (datDif(qtr1,qtr2)>18 & interv=="") {
    interv <- "5 years"
  } else if (datDif(qtr1,qtr2)>8 & interv=="") {
    interv <- "year"
  } else if (interv=="") {
    interv <- "2 quarters"
  }
  c1 <- c1 + scale_x_date(breaks=seq.Date(qtr1,qtr2,by=interv))+
    labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),
      caption=TS[[tabno]]$Ftnt,x="",y="")+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1
}

#a <- Make_chrt(9,1,qtr1,qtr2,"Business sector industries [T004]")
#a
#b <- Make_chrt(1,1,qtr1,qtr2,"Compensation of employees")
#b
