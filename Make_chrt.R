# National accounts charts for shiny dashboard
# April 27, 2021

pkgs <- c("tidyverse","scales","tibble","stringr","rlang")
inst <- lapply(pkgs,library,character.only=TRUE)

source("Make_tabl.R")
# Simple functions
IDX <- function(x) {y <- round(100*x/x[1],1)}
PC <- function(x) {y <- round(100*(x/lag(x)-1),1)}
PC4 <- function(x) {y <- round(100*(x/lag(x,4)-1),1)}
posNeg <- function(x) {sum(x>0,na.rm=TRUE)>0 & sum(x>0,na.rm=TRUE)<length(x)}

# Standard theme for charts
theme_DB <- function(base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170) {
  theme(
    plot.title = element_text(colour="black",size=14,face="bold",hjust=0),
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
Make_chrt <- function(tabno,type,qtr1,qtr2,MYtitl) {
  q0 <- readRDS(paste0("rds/",Tables$STCno[tabno],".rds"))
  if (tabno==7 | tabno==8 | tabno==12) {
    GDPdf1 <- filter(GDPdf,REF_DATE>=as.Date("1981-01-01"))
    q0$GDP <- GDPdf1$VALUE/4
  } else if (tabno==9 | tabno==10 | tabno==11) {
    GDPdf1 <- filter(GDPdf,REF_DATE>=as.Date("1997-01-01"))
    q0$GDP <- GDPdf1$VALUE
  } else { 
    GDPdf1 <- filter(GDPdf,REF_DATE>=as.Date("1961-01-01"))
    q0$GDP <- GDPdf$VALUE   
  }
  
  if (type==1) {
    MYsubtitl="Original series\n"
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"comma")+
      labs(title=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==2) {
    MYsubtitl="Index, starting quarter = 100\n"
    # Check for and deal with NAs
    j <- 0
    for (i in 1:nrow(q0)) { if (is.na(q0[[MYtitl]][i])) j <- j+1 }
    if (j>0) { q0 <- q0[(j+1):nrow(q0),] }
    q1 <- mutate(q0,val=IDX(.data[[MYtitl]]))
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous()+
      labs(title=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==3) {
    MYsubtitl="One-quarter percentage change\n"
    q1 <- mutate(q0,val=PC(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")+
      labs(title=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
 } else if (type==4) {
    MYsubtitl="Four-quarter percentage change\n"
    q1 <- mutate(q0,val=PC4(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")+
      labs(title=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==5) {
    MYsubtitl="Percentage of GDP\n"
    q1 <- mutate(q0,val=100*.data[[MYtitl]]/(GDP*100))
    q1 <- filter(q1,REF_DATE>=qtr1 & REF_DATE<=qtr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"percent")+
      labs(title=paste0(MYsubtitl),caption="",x="",y="")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } 
  c1 <- c1 + scale_x_date(breaks=seq.Date(qtr1,qtr2,by="year"))+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1 
}