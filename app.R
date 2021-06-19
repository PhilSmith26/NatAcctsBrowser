# Canadian national accounts dashboard
# Started March 31, 2021; updated May 14, 2021

library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)
library(shinythemes)

source("Tabl_specs.R") # Specifications for tables
source("Make_tablQ.R") # quarterly table generator
source("Make_tablA.R") # annual table generator
source("Make_chrtQ.R") # quarterly chart generator
source("Make_chrtA.R") # annual chart generator

CurrDate <- as.Date("2021-01-01") # for selections on 1st release day
CurrDate1 <- as.Date("2021-01-01") # for selections after lab prod release

ShowChrt <- function(x) { # Function to display a row of three charts
  fluidRow(
    column(4,plotOutput(paste0("chart",x))),
    column(4,plotOutput(paste0("chart",x+1))),
    column(4,plotOutput(paste0("chart",x+2))),
  )
}
source("qtFunc.R") # function to display quarterly tables
source("qcFunc.R") # function to display quarterly charts
source("atFunc.R") # function to display annual tables
source("acFunc.R") # function to display annual charts

ui <- navbarPage(#tags$head(tags$style("h3 {font-family:Calibri}")),
    #theme=shinytheme("journal"),
  tags$head(tags$style(HTML('* {font-family: "Optima"};'))),
  title = tags$b(tags$span(style="color:red", "National accounts")),
  windowTitle = "Canadian national accounts browser",
  selected = "tabPanel01",
  setBackgroundColor(
    color = c("#d7ebfe", "#6aade7"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabPanel(tags$b(tags$span(style="color:blue", 
    HTML("Introduction<br>and explanation"))),
    value = "tabPanel01",
    htmlOutput("textInfo")
  ),
  tabPanel(tags$b(tags$span(style="color:blue",
    HTML("List of<br>tables"))),
    value = "tabPanel02",
    htmlOutput("tblsInfo")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", 
    HTML("Quarterly statistics"))),
    qtUI(id="idqt"),
    qcUI(id="idqc")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", 
    HTML("Annual statistics"))),
    atUI(id="idat"),
    acUI(id="idac")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", 
    HTML("Selections"))),
    tabPanel(tags$b(tags$span(style="color:blue", 
      HTML("Quarterly<br>selections 1"))),
      fluidPage(
        ShowChrt(1),HTML("<br>"),ShowChrt(4),HTML("<br>"),
        ShowChrt(7),HTML("<br>"),ShowChrt(10),HTML("<br>"),
        ShowChrt(13),HTML("<br>")
      )
    ),
    tabPanel(tags$b(tags$span(style="color:blue", 
      HTML("Quarterly<br>selections 2"))),
      fluidPage(
        ShowChrt(16),HTML("<br>"),ShowChrt(19),HTML("<br>"),
        ShowChrt(22),HTML("<br>"),ShowChrt(25),HTML("<br>"),
        ShowChrt(28),HTML("<br>")
      )
    ),
    tabPanel(tags$b(tags$span(style="color:blue", 
      HTML("Annual<br>selections"))),
      fluidPage(
        ShowChrt(31),HTML("<br>"),ShowChrt(34),HTML("<br>"),
        ShowChrt(37),HTML("<br>"),ShowChrt(40),HTML("<br>")
      )
    )
  )
)

server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  tbls <- "Tables_list.html"
  output$tblsInfo <- renderUI(includeHTML(tbls))
  qtServer(id="idqt")
  qcServer(id="idqc")
  atServer(id="idat")
  acServer(id="idac")
  output$chart1 <- renderPlot(
    {Make_chrtQ(1,4,as.Date("2016-01-01"),CurrDate,
      "Compensation of employees","","")})
  output$chart2 <- renderPlot(
    {Make_chrtQ(1,4,as.Date("2016-01-01"),CurrDate,
      "Net operating surplus: corporations","","")})
  output$chart3 <- renderPlot(
    {Make_chrtQ(1,4,as.Date("2016-01-01"),CurrDate,
      "Net mixed income","","")})
  output$chart4 <- renderPlot(
    {Make_chrtQ(1,4,as.Date("2016-01-01"),CurrDate,
      "Taxes less subsidies on production","","")})
  output$chart5 <- renderPlot(
    {Make_chrtQ(1,4,as.Date("2016-01-01"),CurrDate,
      "Taxes less subsidies on products and imports","","")})
  output$chart6 <- renderPlot(
    {Make_chrtQ(1,4,as.Date("2016-01-01"),CurrDate,
      "Gross domestic product at market prices","","")})
  output$chart7 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Household final consumption expenditure",
      "Household final consumption expenditure (chained Fisher volume)","")})
  output$chart8 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "General governments final consumption expenditure",
      "Governments final consumption expenditure (chained Fisher volume)","")})
  output$chart9 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Residential structures",
      "Residential investment (chained Fisher volume)","")})
  output$chart10 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Non-residential structures, machinery and equipment",
      "Plant and equipment investment (chained Fisher volume)","")})
  output$chart11 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Intellectual property products",
      "Intellectual property products investment (chained Fisher volume)","")})
  output$chart12 <- renderPlot(
    {Make_chrtQ(3,1,as.Date("2016-01-01"),CurrDate,
      "Investment in inventories",
      "Investment in inventories (chained Fisher volume)","")})
  output$chart13 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Exports of goods and services",
      "Exports of goods and services (chained Fisher volume)","")})
  output$chart14 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Less: imports of goods and services",
      "Imports of goods and services (chained Fisher volume)","")})
  output$chart15 <- renderPlot(
    {Make_chrtQ(3,4,as.Date("2016-01-01"),CurrDate,
      "Gross domestic product at market prices",
      "Gross domestic product at market prices (chained Fisher volume)","")})
  output$chart16 <- renderPlot(
    {Make_chrtQ(1,1,as.Date("1961-01-01"),CurrDate,
      "Net operating surplus: corporations","","5 years")})
  output$chart17 <- renderPlot(
    {Make_chrtQ(2,3,as.Date("2010-01-01"),CurrDate,
      "Household final consumption expenditure","","year")})
  output$chart18 <- renderPlot(
    {Make_chrtQ(9,4,as.Date("2008-01-01"),CurrDate,
      "All industries [T001]","Real gross domestic product","year")})
  output$chart19 <- renderPlot(
    {Make_chrtQ(21,1,as.Date("2015-01-01"),CurrDate1,
      "Canada's net international investment position","","year")})
  output$chart20 <- renderPlot(
    {Make_chrtQ(13,1,as.Date("1961-01-01"),CurrDate,
      "Business gross fixed capital formation: residential structures",
      "Business residential construction investment","5 years")})
  output$chart21 <- renderPlot(
    {Make_chrtQ(22,5,as.Date("1997-01-01"),CurrDate1,
      "Labour productivity","","year")})
  output$chart22 <- renderPlot(
    {Make_chrtQ(20,5,as.Date("1981-01-01"),CurrDate,
      "Dividends","Household dividend income","5 years")})
  output$chart23 <- renderPlot(
    {Make_chrtQ(7,1,as.Date("2017-10-01"),CurrDate,
      "Cannabis products for non-medical use (licensed)",
      "Household expenditure on licensed non-medical cannabis","")})
  output$chart24 <- renderPlot(
    {Make_chrtQ(11,3,as.Date("2010-01-01"),CurrDate,
      "Accommodation and food services [72]",
      "Real GDP, accommodation and food services","year")})
  output$chart25 <- renderPlot(
    {Make_chrtQ(28,3,as.Date("2015-01-01"),CurrDate1,
      "Total economy","Index of labour compensation per hour","year")})
  output$chart26 <- renderPlot(
    {Make_chrtQ(16,6,as.Date("1961-01-01"),CurrDate,
      "Equals: net lending or net borrowing",
      "Federal government net lending/borrowing","5 years")})
  output$chart27 <- renderPlot(
    {Make_chrtQ(27,3,as.Date("1961-01-01"),CurrDate1,
      "Utilities [22]",
      "Index of labour productivity in the utilities industry","5 years")})
  output$chart28 <- renderPlot(
    {Make_chrtQ(4,5,as.Date("1961-01-01"),CurrDate,
      "Gross domestic product at market prices",
      "GDP implicit price index","5 years")})
  output$chart29 <- renderPlot(
    {Make_chrtQ(31,1,as.Date("1961-01-01"),CurrDate,
      "Real exchange rate, index 2012=100","","5 years")})
  output$chart30 <- renderPlot(
    {Make_chrtQ(31,1,as.Date("1961-01-01"),CurrDate,
      "Terms of trade, index 2012=100","","5 years")})
  output$chart31 <- renderPlot(
    {Make_chrtA(48,4,"1947","1996",
      "Gross domestic product (GDP) at market prices",
      "GDP at constant 1986 prices","3")})
  output$chart32 <- renderPlot(
    {Make_chrtA(46,1,"1947","1996",
      "Accrued net income of farm operators from farm production",
      "","5")})
  output$chart33 <- renderPlot(
    {Make_chrtA(47,5,"1947","1996",
      "Business investment, inventories",
      "","5")})
  output$chart34 <- renderPlot(
    {Make_chrtA(50,1,"1990","2020",
      "Net transactions in financial assets",
      "","3")})
  output$chart35 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Total assets",
      "Market value of all Canadian assets","5")})
  output$chart36 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Produced non-financial assets",
      "Market value of produced non-financial assets","3")})
  output$chart37 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Weapons systems",
      "Market value of weapons systems assets","3")})
  output$chart38 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Non-produced non-financial assets",
      "Market value of all non-produced non-financial assets","3")})
  output$chart39 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Natural resources",
      "Market value of natural resource assets","3")})
  output$chart40 <- renderPlot(
    {Make_chrtA(45,1,"1997","2020",
      "Energy products [C12]",
      "Volume of imports of energy products","3")})
  output$chart41 <- renderPlot(
    {Make_chrtA(34,3,"1961","2020",
      "Equals: non-profit institutions serving households' disposable income",
      "Disposable income of non-profit institutions serving households","5")})
  output$chart42 <- renderPlot(
    {Make_chrtA(5,4,"1961","2020",
      "Equals: net national income at basic prices",
      "Net national income at basic prices","5")})
}

shinyApp(ui, server)
