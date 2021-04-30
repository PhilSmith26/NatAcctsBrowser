# Canadian national accounts dashboard
# Started March 31, 2021; updated April 22, 2021

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)

source("Series_lists.R")
source("Make_tabl.R")
source("Make_chrt.R")
tbl <- c(
  "Gross domestic product, income-based, current prices",
  "Gross domestic product, expenditure-based, current prices",
  "Gross domestic product, expenditure-based, chained Fisher volume indexes",
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
  "Property income of households")
tnum <- c(
  "36-10-0103-01",
  "36-10-0104-01",
  "36-10-0104-02",
  "36-10-0106-01",
  "36-10-0122-01",
  "36-10-0111-01",
  "36-10-0124-01",
  "36-10-0124-02",
  "36-10-0434-01",
  "36-10-0434-02",
  "36-10-0434-03",
  "36-10-0016-01",
  "36-10-0108-01",
  "36-10-0108-02",
  "36-10-0477-01",
  "36-10-0477-02",
  "36-10-0477-03",
  "36-10-0114-01",
  "36-10-0117-01",
  "36-10-0126-01")
tn <- setNames(tnum,tbl)
trf1 <- c(
  "Original table",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP"
)

trf2 <- c(
  "Original series",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP"
)

qtrsD <- seq.Date(as.Date("1961-01-01"),as.Date("2020-10-01"),by="quarter")
qtrs61 <- character()
for (i in 1:240) {
  qtrs61[i] <- paste0(year(qtrsD[i])," Q",quarter(qtrsD[i]))
}
qtrs81 <- qtrs61[81:length(qtrs61)]
qtrs97 <- qtrs61[145:length(qtrs61)]

firstdate <- "2019-10-01"
lastdate <- "2020-10-01"

ui <- navbarPage(title = tags$b(tags$span(style="color:red", "National accounts")),
  windowTitle = "Canadian national accounts dashboard",
  selected = "tabPanel01",
  setBackgroundColor(
    color = c("#d7ebfe", "#6aade7"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Introduction and explanation")),
    value = "tabPanel01",
    htmlOutput("textInfo")
  ),
  tabPanel(tags$b(tags$span(style="color:blue", "Tables")),
    tags$style(type='text/css', ".selectize-input { font-size: 24px; line-height: 24px;} .selectize-dropdown { font-size: 20px; line-height: 20px; }"),
    selectInput("tabl1", tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,width = "100%"),
    prettyRadioButtons("transf1", tags$b(tags$span(style="color:blue", 
      "Choose a transformation:")),choices=trf1,bigger=TRUE,outline=TRUE,
      inline=TRUE,shape="round",animation="pulse"),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput("Dates",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices=qtrs61,
      selected=c("2019 Q4","2020 Q4"),
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
      selected=c("2019 Q4","2020 Q4"),
      dragRange = TRUE,
      width="100%"),
    htmlOutput("nochart"),
    plotOutput("chart")
  )
)
server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  tab1 <- reactive(case_when(
    input$tabl1=="36-10-0103-01" ~1,
    input$tabl1=="36-10-0104-01" ~2,
    input$tabl1=="36-10-0104-02" ~3,
    input$tabl1=="36-10-0106-01" ~4,
    input$tabl1=="36-10-0122-01" ~5,
    input$tabl1=="36-10-0111-01" ~6,
    input$tabl1=="36-10-0124-01" ~7,
    input$tabl1=="36-10-0124-02" ~8,
    input$tabl1=="36-10-0434-01" ~9,
    input$tabl1=="36-10-0434-02" ~10,
    input$tabl1=="36-10-0434-03" ~11,
    input$tabl1=="36-10-0016-01" ~12,
    input$tabl1=="36-10-0108-01" ~13,
    input$tabl1=="36-10-0108-02" ~14,
    input$tabl1=="36-10-0477-01" ~15,
    input$tabl1=="36-10-0477-02" ~16,
    input$tabl1=="36-10-0477-03" ~17,
    input$tabl1=="36-10-0114-01" ~18,
    input$tabl1=="36-10-0117-01" ~19,
    input$tabl1=="36-10-0126-01" ~20
  ))
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
  #observeEvent(input$Dates[1], {
  #  message(paste0("input$Dates[1] is ",input$Dates[1]))
  #  message(paste0("class of qtr1 is ",class(qtr1())))
  #})
  output$notabl <- renderUI({
    if ((tab1()==3  & type1()==5) | (tab1()==4  & type1()==5) | 
        (tab1()==8  & type1()==5) | (tab1()==9  & type1()==5) | 
        (tab1()==10 & type1()==5) | (tab1()==11 & type1()==5) |
        (tab1()==14 & type1()==5)
        )
      sendSweetAlert(session,
        title="Transformation not relevant here",
        text=paste0("This transformation is not relevant in this context. ",
        "Shares are not meaningful for price and volume indexes. Only ",
        "relative changes are significant. Shares should be calculated ",
        "with data at current prices."),
        type="info",
        btn_labels="Okay",
        btn_colors="#3085d6",
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
        )
  })
  output$tabl <- render_gt({
    if (!((tab1()==3  & type1()==5) | (tab1()==4  & type1()==5) |
          (tab1()==8  & type1()==5) | (tab1()==9  & type1()==5) |
          (tab1()==10 & type1()==5) | (tab1()==11 & type1()==5) |
          (tab1()==14 & type1()==5)
      ))
    expr=Make_tabl(tab1(),type1(),qtr1(),qtr2())
  })
  observe({
    if (tab1()==7 | tab1()==8 | tab1()==12) {
      picks <- qtrs81
    } else if (tab1()==9 | tab1()==10 | tab1()==11) {
      picks <- qtrs97
    } else {
      picks <- qtrs61
    } 
    updateSliderTextInput(session,"Dates",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices = picks,
      selected=c("2019 Q4","2020 Q4"))
  })
  
######### Charts starts here #########
  
  tab2 <- reactive(case_when(
    input$tabl2=="36-10-0103-01" ~1,
    input$tabl2=="36-10-0104-01" ~2,
    input$tabl2=="36-10-0104-02" ~3,
    input$tabl2=="36-10-0106-01" ~4,
    input$tabl2=="36-10-0122-01" ~5,
    input$tabl2=="36-10-0111-01" ~6,
    input$tabl2=="36-10-0124-01" ~7,
    input$tabl2=="36-10-0124-02" ~8,
    input$tabl2=="36-10-0434-01" ~9,
    input$tabl2=="36-10-0434-02" ~10,
    input$tabl2=="36-10-0434-03" ~11,
    input$tabl2=="36-10-0016-01" ~12,
    input$tabl2=="36-10-0108-01" ~13,
    input$tabl2=="36-10-0108-02" ~14,
    input$tabl2=="36-10-0477-01" ~15,
    input$tabl2=="36-10-0477-02" ~16,
    input$tabl2=="36-10-0477-03" ~17,
    input$tabl2=="36-10-0114-01" ~18,
    input$tabl2=="36-10-0117-01" ~19,
    input$tabl2=="36-10-0126-01" ~20
  ))
  type2 <- reactive(case_when(
    input$transf2=="Original series"~1,
    input$transf2=="Index, starting quarter = 100"~2,
    input$transf2=="One-quarter % change"~3,
    input$transf2=="Four-quarter % change"~4,
    input$transf2=="Percentage of GDP"~5
  ))
 
  Nchoices <- reactive({
    if(tab2()==1) { Nchoices <- ser_01
    } else if (tab2()==2) { Nchoices <- ser_02
    } else if (tab2()==3) { Nchoices <- ser_03
    } else if (tab2()==4) { Nchoices <- ser_04
    } else if (tab2()==5) { Nchoices <- ser_05
    } else if (tab2()==6) { Nchoices <- ser_06
    } else if (tab2()==7) { Nchoices <- ser_07
    } else if (tab2()==8) { Nchoices <- ser_08
    } else if (tab2()==9) { Nchoices <- ser_09
    } else if (tab2()==10) { Nchoices <- ser_10
    } else if (tab2()==11) { Nchoices <- ser_11
    } else if (tab2()==12) { Nchoices <- ser_12
    } else if (tab2()==13) { Nchoices <- ser_13
    } else if (tab2()==14) { Nchoices <- ser_14
    } else if (tab2()==15) { Nchoices <- ser_15
    } else if (tab2()==16) { Nchoices <- ser_16
    } else if (tab2()==17) { Nchoices <- ser_17
    } else if (tab2()==18) { Nchoices <- ser_18
    } else if (tab2()==19) { Nchoices <- ser_19
    } else Nchoices <- ser_20
  })
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
  
  q0 <- reactive({readRDS(paste0("rds/",input$tabl2,".rds"))})
  MYtitl <- reactive({input$chrt})

  output$nochart <- renderUI({
    if ((tab2()==3  & type2()==5) | (tab2()==4  & type2()==5) |
        (tab2()==8  & type2()==5) | (tab2()==9  & type2()==5) |
        (tab2()==10 & type2()==5) | (tab2()==11 & type2()==5) |
        (tab2()==14 & type2()==5))
      sendSweetAlert(session,
        title="Transformation not relevant here",
        text=paste0("This transformation is not relevant in this context. ",
        "Shares are not meaningful for price and volume indexes. Only ",
        "relative changes are significant. Shares should be calculated ",
        "with data at current prices."),
        type="info",
        btn_labels="Okay",
        btn_colors="#3085d6",
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
        )
  })
  output$chart <- renderPlot({
    if (!((tab2()==3  & type2()==5) | (tab2()==4  & type2()==5) |
          (tab2()==8  & type2()==5) | (tab2()==9  & type2()==5) |
          (tab2()==10 & type2()==5) | (tab2()==11 & type2()==5) |
          (tab2()==14 & type2()==5)))
       # & substr(input$chrt,nchar(input$chrt)-4,nchar(input$chrt))!="entry")
    Make_chrt(tab2(),type2(),qtr1c(),qtr2c(),MYtitl())},height=700)   
  
  observe({
    if (tab2()==7 | tab2()==8 | tab2()==12) {
      picks <- qtrs81
    } else if (tab2()==9 | tab2()==10 | tab2()==11) {
      picks <- qtrs97
    } else {
      picks <- qtrs61
    } 
    updateSliderTextInput(session,"ChrtDats",tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:")),
      choices = picks,
      selected=c("2019 Q4","2020 Q4"))
  })
  #observeEvent(output$textAbt, {
  #  message(paste0("textAbt is ",output$textAbt))
  #})
}

shinyApp(ui, server)
