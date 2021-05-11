# Canadian national accounts dashboard
# Started March 31, 2021; updated May 9, 2021

library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)

source("Tabl_specs.R")
source("Make_tabl.R")
source("Make_chrt.R")

tord <- c(1,2,3,4,9,10,11,5,31,18,20,19,7,8,13,14,41,32,6,33,
  34,35,36,38,39,40,37,15,16,17,12,42,43,44,45,21,22,23,24,25,26,
  27,28,29,30,46,47,48) # the order of the tables
tbl <- character()
tabn <- numeric()
for (i in 1:length(tord)) {
  tbl[i] <- TS[[tord[i]]]$Titl # a vector of table names
  tabn[i] <- TS[[tord[i]]]$Num # a vector of table numbers
}
tn <- setNames(tabn,tbl) # a vector of named table numbers
trf1 <- c( # transformation options for a table
  "Original table",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP",
  "Five-quarter centred moving average"
)
trf2 <-  c( # transformation options for a chart
  "Original series",
  "Include a trend line",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP",
  "Five-quarter centred moving average"
)
# Starting conditions for initial table and chart
# First the full sequence of dates in "Date" format
qtrsD <- seq.Date(TS[[1]]$Strt,TS[[1]]$Endt,by="quarter")
# Now the corresponding sequence of dates in "character" format
qtrsSrt <- character()
for (i in 1:length(qtrsD)) {
  qtrsSrt[i] <- paste0(year(qtrsD[i])," Q",quarter(qtrsD[i]))
}
# Now the starting range for a table - the last six quarters
strtrangT <- c(qtrsSrt[length(qtrsSrt)-5],qtrsSrt[length(qtrsSrt)])
# Now the starting range for a chart - all available quarters
strtrangC <- c(qtrsSrt[1],qtrsSrt[length(qtrsSrt)])

ShowChrt <- function(x) { # Function to display a row of three charts
  fluidRow(
    column(4,plotOutput(paste0("chart",x))),
    column(4,plotOutput(paste0("chart",x+1))),
    column(4,plotOutput(paste0("chart",x+2))),
  )
}

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
  tabPanel(tags$b(tags$span(style="color:blue","List of tables")),
    value = "tabPanel02",
    htmlOutput("tblsInfo")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Tables")),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput("tabl1", tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,selectize=FALSE,width = "100%"),
    fluidRow(
      column(10,prettyRadioButtons("transf1", tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf1,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(2,downloadButton("downloadData1",label="Download table"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(inputId="Dates",label= #tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:",#)),
      choices=qtrsSrt,
      selected=strtrangT,
      dragRange = TRUE,
      width="100%"),
    htmlOutput("notabl"),
    gt_output("tabl")
    
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Charts")),
    selectInput("tabl2", tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,width = "100%"),
    selectInput("chrt", tags$b(tags$span(style="color:blue", 
      "Choose a time series to chart:")),choices = ser_01,selectize=FALSE,width = "100%"),
    fluidRow(
      column(10,prettyRadioButtons("transf2", tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf2,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(2,downloadButton("downloadData2",label="Download chart"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput("ChrtDats",label= #tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:",#)),
      choices=qtrsSrt,
      selected=strtrangC,
      dragRange = TRUE,
      width="100%"),
    htmlOutput("nochart"),
    plotOutput("chart")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Selections 1")),
    fluidPage(
      ShowChrt(1),HTML("<br>"),ShowChrt(4),HTML("<br>"),
      ShowChrt(7),HTML("<br>"),ShowChrt(10),HTML("<br>")
    )
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Selections 2")),
    fluidPage(
      ShowChrt(13),HTML("<br>"),ShowChrt(16),HTML("<br>"),
      ShowChrt(19),HTML("<br>"),ShowChrt(22),HTML("<br>")
    )
  )
)

server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  tbls <- "Tables_list.html"
  output$tblsInfo <- renderUI(includeHTML(tbls))
  tab1 <- reactive(as.numeric(input$tabl1))
  type1 <- reactive(case_when(
    input$transf1=="Original table"~1,
    input$transf1=="Index, starting quarter = 100"~2,
    input$transf1=="One-quarter % change"~3,
    input$transf1=="Four-quarter % change"~4,
    input$transf1=="Percentage of GDP"~5,
    input$transf1=="Five-quarter centred moving average"~6
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
    if (TS[[tab1()]]$Idx & type1()==5)
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
  expr <- reactive({Make_tabl(tab1(),type1(),qtr1(),qtr2())})
  output$tabl <- render_gt({
    if (!(TS[[tab1()]]$Idx & type1()==5))
      expr()[[1]]
  })
  output$downloadData1 <- downloadHandler(
    filename=function() {
      paste0(TS[[tab1()]]$STCno,".csv")
    },
    content=function(file) {
      write.csv(expr()[[2]],file)
    }
  )
  observe({
    qtrsRange <- seq.Date(TS[[tab1()]]$Strt,TS[[tab1()]]$Endt,by="quarter")
    qtrs <- character()
    for (i in 1:length(qtrsRange)) {
      qtrs[i] <- paste0(year(qtrsRange[i])," Q",quarter(qtrsRange[i]))
    }    
    picks <- qtrs
    strtrang1 <- c(qtrs[length(qtrs)-5],qtrs[length(qtrs)])
    updateSliderTextInput(session,inputId="Dates",tags$b(tags$span(style="color:blue", 
      label="Choose starting and ending dates:")),
      choices = picks,
      selected=strtrang1)
  })
  
######### Charts starts here #########
  tab2 <- reactive(as.numeric(input$tabl2))
  type2 <- reactive(case_when(
    input$transf2=="Original series"~1,
    input$transf2=="Include a trend line"~2,
    input$transf2=="Index, starting quarter = 100"~3,
    input$transf2=="One-quarter % change"~4,
    input$transf2=="Four-quarter % change"~5,
    input$transf2=="Percentage of GDP"~6,
    input$transf2=="Five-quarter centred moving average"~7
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
    if (TS[[tab2()]]$Idx & type2()==6)
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
  chart1 <- reactive({if (!(TS[[tab2()]]$Idx & type2()==6))
      Make_chrt(tab2(),type2(),qtr1c(),qtr2c(),MYtitl(),"","year")})   
  output$chart <- renderPlot({chart1()},height=700)
  output$downloadData2 <- downloadHandler(
    filename=function() {
      paste0(TS[[tab2()]]$STCno,".png")
    },
    content=function(file) {
      ggsave(file,chart1(),height=8,width=16,dpi=300)
    }
  )
  observe({
    qtrsRange <- seq.Date(TS[[tab2()]]$Strt,TS[[tab2()]]$Endt,by="quarter")
    qtrs <- character()
    for (i in 1:length(qtrsRange)) {
      qtrs[i] <- paste0(year(qtrsRange[i])," Q",quarter(qtrsRange[i]))
    }    
    picks <- qtrs
    strtrang1 <- c(qtrs[1],qtrs[length(qtrs)])
    updateSliderTextInput(session,"ChrtDats",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices = picks,
      selected=strtrang1)
  })
  output$chart1 <- renderPlot(
    {Make_chrt(1,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Net operating surplus: corporations","","5 years")})
  output$chart2 <- renderPlot(
    {Make_chrt(2,3,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Household final consumption expenditure","","year")})
  output$chart3 <- renderPlot(
    {Make_chrt(9,4,as.Date("2008-01-01"),as.Date("2020-10-01"),
      "All industries [T001]","Real gross domestic product","year")})
  output$chart4 <- renderPlot(
    {Make_chrt(21,6,as.Date("2015-01-01"),as.Date("2020-10-01"),
      "Canada's net international investment position","","year")})
  output$chart5 <- renderPlot(
    {Make_chrt(13,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: residential structures",
      "Business residential construction investment","5 years")})
  output$chart6 <- renderPlot(
    {Make_chrt(22,4,as.Date("1997-01-01"),as.Date("2020-10-01"),
      "Labour productivity","","year")})
  output$chart7 <- renderPlot(
    {Make_chrt(20,6,as.Date("1981-01-01"),as.Date("2020-10-01"),
      "Dividends","Household dividend income","5 years")})
  output$chart8 <- renderPlot(
    {Make_chrt(7,1,as.Date("2018-10-01"),as.Date("2020-10-01"),
      "Cannabis products for non-medical use (licensed)",
      "Household expenditure on licensed cannabis","year")})
  output$chart9 <- renderPlot(
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
