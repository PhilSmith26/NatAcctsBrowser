# Canadian national accounts dashboard
# Started March 31, 2021; updated May 5, 2021

#setwd("/Users/philipsmith/Documents/R/NatAcctsBrowserV7/")

library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)

source("Tabl_specs.R")
source("Make_tabl.R")
source("Make_chrt.R")

firstdate <- "2019-10-01"
lastdate <- "2020-10-01"
strtrang <- c("2019 Q2","2020 Q4")
numqtrs <- 240

tbl <- c(TS[[1]]$Titl, TS[[2]]$Titl, TS[[3]]$Titl, TS[[4]]$Titl,
         TS[[9]]$Titl, TS[[10]]$Titl,TS[[11]]$Titl,TS[[5]]$Titl, 
         TS[[31]]$Titl,TS[[18]]$Titl,TS[[20]]$Titl,TS[[19]]$Titl,
         TS[[7]]$Titl, TS[[8]]$Titl, TS[[13]]$Titl,TS[[14]]$Titl,
         TS[[41]]$Titl,TS[[32]]$Titl,TS[[6]]$Titl, TS[[33]]$Titl,
         TS[[34]]$Titl,TS[[35]]$Titl,TS[[36]]$Titl,TS[[38]]$Titl,
         TS[[39]]$Titl,TS[[40]]$Titl,TS[[37]]$Titl,TS[[15]]$Titl,
         TS[[16]]$Titl,TS[[17]]$Titl,TS[[12]]$Titl,TS[[42]]$Titl,
         TS[[43]]$Titl,TS[[44]]$Titl,TS[[45]]$Titl,TS[[21]]$Titl,
         TS[[22]]$Titl,TS[[23]]$Titl,TS[[24]]$Titl,TS[[25]]$Titl,
         TS[[26]]$Titl,TS[[27]]$Titl,TS[[28]]$Titl,TS[[29]]$Titl,
         TS[[30]]$Titl)
tabn <- c(TS[[1]]$Num,TS[[2]]$Num, TS[[3]]$Num, TS[[4]]$Num,
         TS[[9]]$Num, TS[[10]]$Num,TS[[11]]$Num,TS[[5]]$Num, 
         TS[[31]]$Num,TS[[18]]$Num,TS[[20]]$Num,TS[[19]]$Num,
         TS[[7]]$Num, TS[[8]]$Num, TS[[13]]$Num,TS[[14]]$Num,
         TS[[41]]$Num,TS[[32]]$Num,TS[[6]]$Num, TS[[33]]$Num,
         TS[[34]]$Num,TS[[35]]$Num,TS[[36]]$Num,TS[[38]]$Num,
         TS[[39]]$Num,TS[[40]]$Num,TS[[37]]$Num,TS[[15]]$Num,
         TS[[16]]$Num,TS[[17]]$Num,TS[[12]]$Num,TS[[42]]$Num,
         TS[[43]]$Num,TS[[44]]$Num,TS[[45]]$Num,TS[[21]]$Num,
         TS[[22]]$Num,TS[[23]]$Num,TS[[24]]$Num,TS[[25]]$Num,
         TS[[26]]$Num,TS[[27]]$Num,TS[[28]]$Num,TS[[29]]$Num,
         TS[[30]]$Num)
tn <- setNames(tabn,tbl)
trf1 <- c(
  "Original table",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP"
)
trf2 <-  c(
  "Original series",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP",
  "Include a trend line"
)

qtrsD <- seq.Date(as.Date("1961-01-01"),as.Date(lastdate),by="quarter")
qtrs61 <- character()
for (i in 1:numqtrs) {
  qtrs61[i] <- paste0(year(qtrsD[i])," Q",quarter(qtrsD[i]))
}
qtrs81 <- qtrs61[81:length(qtrs61)]
qtrs97 <- qtrs61[145:length(qtrs61)]
qtrs15 <- qtrs61[217:length(qtrs61)]

ui <- navbarPage(title = tags$b(tags$span(style="color:red", "National accounts")),
  windowTitle = "Canadian national accounts browser",
  selected = "tabPanel01",
  setBackgroundColor(
    color = c("#d7ebfe", "#6aade7"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabPanel(tags$b(tags$span(style="color:blue", 
    "Introduction and explanation")),
    value = "tabPanel01",
    htmlOutput("textInfo")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Tables")),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput("tabl1", tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,width = "100%"),
    prettyRadioButtons("transf1", tags$b(tags$span(style="color:blue", 
      "Choose a transformation:")),choices=trf1,bigger=TRUE,
      outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput("Dates",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices=qtrs61,
      selected=strtrang,
      dragRange = TRUE,
      width="100%"),
    htmlOutput("notabl"),
    gt_output("tabl")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Charts")),
    selectInput("tabl2", tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,width = "100%"),
    selectInput("chrt", tags$b(tags$span(style="color:blue", 
      "Choose a time series to chart:")),choices = ser_01,width = "100%"),
    prettyRadioButtons("transf2", tags$b(tags$span(style="color:blue", 
      "Choose a transformation:")),choices=trf2,bigger=TRUE,outline=TRUE,
      inline=TRUE,shape="round",animation="pulse"),      
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput("ChrtDats",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices=qtrs61,
      selected=strtrang,
      dragRange = TRUE,
      width="100%"),
    htmlOutput("nochart"),
    plotOutput("chart")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Selections 1")),
    fluidPage(
      fluidRow(
        column(4,plotOutput("chart01")),
        column(4,plotOutput("chart02")),
        column(4,plotOutput("chart03"))
      ),
      HTML("<br>"),
      fluidRow(
        column(4,plotOutput("chart04")),
        column(4,plotOutput("chart05")),
        column(4,plotOutput("chart06"))
      ),
      HTML("<br>"),
      fluidRow(
        column(4,plotOutput("chart07")),
        column(4,plotOutput("chart08")),
        column(4,plotOutput("chart09"))
      ),
      HTML("<br>"),
      fluidRow(
        column(4,plotOutput("chart10")),
        column(4,plotOutput("chart11")),
        column(4,plotOutput("chart12"))
      )
    )
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Selections 2")),
    fluidPage(
      fluidRow(
        column(4,plotOutput("chart13")),
        column(4,plotOutput("chart14")),
        column(4,plotOutput("chart15"))
      ),
      HTML("<br>"),
      fluidRow(
        column(4,plotOutput("chart16")),
        column(4,plotOutput("chart17")),
        column(4,plotOutput("chart18"))
      ),
      HTML("<br>"),
      fluidRow(
        column(4,plotOutput("chart19")),
        column(4,plotOutput("chart20")),
        column(4,plotOutput("chart21"))
      ),
      HTML("<br>"),
      fluidRow(
        column(4,plotOutput("chart22")),
        column(4,plotOutput("chart23")),
        column(4,plotOutput("chart24"))
      )
    )
  )
)

server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  tab1 <- reactive(as.numeric(input$tabl1))
  type1 <- reactive(case_when(
    input$transf1=="Original table"~1,
    input$transf1=="Index, starting quarter = 100"~2,
    input$transf1=="One-quarter % change"~3,
    input$transf1=="Four-quarter % change"~4,
    input$transf1=="Percentage of GDP"~5
  ))
  qtr1 <- reactive({
    as.Date(paste0(substr(input$Dates[1],1,4),"-",
      (1+3*(as.numeric(substr(input$Dates[1],7,7))-1)),"-01"))
  })
  qtr2 <- reactive({
    as.Date(paste0(substr(input$Dates[2],1,4),"-",
      (1+3*(as.numeric(substr(input$Dates[2],7,7))-1)),"-01"))
  })
  output$notabl <- renderUI({
    if ((TS[[tab1()]]$Idx & type1()==5) | (tab1()==31 & type1()!=1))
      sendSweetAlert(session,
        title="Transformation not relevant here",
        text=paste0("This transformation is not relevant in this context, ",
          "where there are index numbers and/or contributions to change."),
        type="info",
        btn_labels="Okay",
        btn_colors="#3085d6",
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
        )
  })
  output$tabl <- render_gt({
    if (!(TS[[tab1()]]$Idx & type1()==5) & !(tab1()==31 & type1()!=1))
      expr=Make_tabl(tab1(),type1(),qtr1(),qtr2())
  })
  observe({
    if (TS[[tab1()]]$Strt==as.Date("1981-01-01")) {
      picks <- qtrs81
    } else if (TS[[tab1()]]$Strt==as.Date("1997-01-01")) {
      picks <- qtrs97
    } else if (TS[[tab1()]]$Strt==as.Date("2015-01-01")) {
      picks <- qtrs15
    } else {
      picks <- qtrs61
    }
    updateSliderTextInput(session,"Dates",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices = picks,
      selected=strtrang)
  })
  
######### Charts starts here #########
  tab2 <- reactive(as.numeric(input$tabl2))
  type2 <- reactive(case_when(
    input$transf2=="Original series"~1,
    input$transf2=="Index, starting quarter = 100"~2,
    input$transf2=="One-quarter % change"~3,
    input$transf2=="Four-quarter % change"~4,
    input$transf2=="Percentage of GDP"~5,
    input$transf2=="Include a trend line"~6
  ))
 
  Nchoices <- reactive({TS[[tab2()]]$Nchoices})
  
  observeEvent(input$tabl2,{updateSelectInput(session,"chrt",
    choices=Nchoices())})

  qtr1c <- reactive({
    as.Date(paste0(substr(input$ChrtDats[1],1,4),"-",
      (1+3*(as.numeric(substr(input$ChrtDats[1],7,7))-1)),"-01"))
  })
  qtr2c <- reactive({
    as.Date(paste0(substr(input$ChrtDats[2],1,4),"-",
      (1+3*(as.numeric(substr(input$ChrtDats[2],7,7))-1)),"-01"))
  })
  
  MYtitl <- reactive({input$chrt})

  output$nochart <- renderUI({
    if ((TS[[tab2()]]$Idx & type2()==5) | (tab2()==31 & type2()!=1))
      sendSweetAlert(session,
        title="Transformation not relevant here",
        text=paste0("This transformation is not relevant in this context, ",
          "where there are index numbers and/or contributions to change."),
        type="info",
        btn_labels="Okay",
        btn_colors="#3085d6",
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
        )
  })
  output$chart <- renderPlot({
    if (!(TS[[tab2()]]$Idx & type2()==5) & !(tab2()==31 & type2()!=1))
      Make_chrt(tab2(),type2(),qtr1c(),qtr2c(),MYtitl(),"","year")},height=700)   
  observe({
    if (TS[[tab2()]]$Strt==as.Date("1981-01-01")) {
      picks <- qtrs81
    } else if (TS[[tab2()]]$Strt==as.Date("1997-01-01")) {
      picks <- qtrs97
    } else if (TS[[tab2()]]$Strt==as.Date("2015-01-01")) {
      picks <- qtrs15
    } else {
      picks <- qtrs61
    }
    updateSliderTextInput(session,"ChrtDats",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices = picks,
      selected=strtrang)
  })
  #tab3 <- reactive(as.numeric(input$Page))
  output$chart01 <- renderPlot(
    {Make_chrt(1,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Net operating surplus: corporations","","5 years")})
  output$chart02 <- renderPlot(
    {Make_chrt(2,3,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Household final consumption expenditure","","year")})
  output$chart03 <- renderPlot(
    {Make_chrt(9,4,as.Date("2008-01-01"),as.Date("2020-10-01"),
      "All industries [T001]","Real gross domestic product","year")})
  output$chart04 <- renderPlot(
    {Make_chrt(21,6,as.Date("2015-01-01"),as.Date("2020-10-01"),
      "Canada's net international investment position","","year")})
  output$chart05 <- renderPlot(
    {Make_chrt(13,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: residential structures",
      "Business residential construction investment","5 years")})
  output$chart06 <- renderPlot(
    {Make_chrt(22,4,as.Date("1997-01-01"),as.Date("2020-10-01"),
      "Labour productivity","","year")})
  output$chart07 <- renderPlot(
    {Make_chrt(20,6,as.Date("1981-01-01"),as.Date("2020-10-01"),
      "Dividends","Household dividend income","5 years")})
  output$chart08 <- renderPlot(
    {Make_chrt(7,1,as.Date("2018-10-01"),as.Date("2020-10-01"),
      "Cannabis products for non-medical use (licensed)",
      "Household expenditure on licensed cannabis","year")})
  output$chart09 <- renderPlot(
    {Make_chrt(11,3,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Accommodation and food services [72]",
      "Real GDP, accommodation and food services","year")})
  output$chart10 <- renderPlot(
    {Make_chrt(28,2,as.Date("2015-01-01"),as.Date("2020-10-01"),
      "Total economy","Index of labour compensation per hour","year")})
  output$chart11 <- renderPlot(
    {Make_chrt(16,5,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Equals: net lending or net borrowing",
      "Federal government net lending/borrowing","5 years")})
  output$chart12 <- renderPlot(
    {Make_chrt(18,2,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Employers' social contributions","","5 years")})
  output$chart13 <- renderPlot(
    {Make_chrt(4,4,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Gross domestic product at market prices","GDP implicit price index","5 years")})
  output$chart14 <- renderPlot(
    {Make_chrt(31,6,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Real exchange rate, index 2012=100","","5 years")})
  output$chart15 <- renderPlot(
    {Make_chrt(31,6,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Terms of trade, index 2012=100","","5 years")})
  output$chart16 <- renderPlot(
    {Make_chrt(31,2,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Real household disposable income, volume index 2012=100","","year")})
  output$chart17 <- renderPlot(
    {Make_chrt(19,5,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Less: taxes on income (corporate income tax)",
      "Corporate income tax","5 years")})
  output$chart18 <- renderPlot(
    {Make_chrt(19,1,as.Date("1990-01-01"),as.Date("2020-10-01"),
      "Less: remitted profits of government business enterprises",
      "Remitted profits of government business enterprises","5 years")})
  output$chart19 <- renderPlot(
    {Make_chrt(14,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: non-residential structures and machinery and equipment",
      "Real business plant and equipment investment","5 years")})
  output$chart20 <- renderPlot(
    {Make_chrt(13,5,as.Date("1961-10-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: non-residential structures and machinery and equipment",
      "Business plant and equipment investment","5 years")})
  output$chart21 <- renderPlot(
    {Make_chrt(13,1,as.Date("1990-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: computers and computer peripheral equipment",
      "Business investment in computers","5 years")})
  output$chart22 <- renderPlot(
    {Make_chrt(41,1,as.Date("1990-01-01"),as.Date("2020-10-01"),
      "Investment in inventories","","5 years")})
  output$chart23 <- renderPlot(
    {Make_chrt(33,1,as.Date("1981-01-01"),as.Date("2020-10-01"),
      "Household net saving","","5 years")})
  output$chart24 <- renderPlot(
    {Make_chrt(39,5,as.Date("2001-01-01"),as.Date("2020-10-01"),
      "Equals: general governments net lending or net borrowing",
      "Provincial and territorial net lending or borrowing","5 years")})
 
    #observeEvent(output$textAbt, {
  #  message(paste0("textAbt is ",output$textAbt))
  #})
}

shinyApp(ui, server)
