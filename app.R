# Canadian national accounts dashboard
# Started March 31, 2021; updated May 9, 2021

library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)

source("Tabl_specs.R") # Specifications for tables
source("Make_tablQ.R") # quarterly table generator
source("Make_tablA.R") # annual table generator
source("Make_chrtQ.R") # quarterly chart generator
source("Make_chrtA.R") # annual chart generator

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

ui <- navbarPage(title = tags$b(tags$span(style="color:red", "National accounts")),
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
  tabPanel(tags$b(tags$span(style="color:blue",HTML("List of<br>tables"))),
    value = "tabPanel02",
    htmlOutput("tblsInfo")
  ),
  qtUI(id="idqt"),
  qcUI(id="idqc"),
  atUI(id="idat"),
  acUI(id="idac"),
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Quarterly<br>selections 1"))),
    fluidPage(
      ShowChrt(1),HTML("<br>"),ShowChrt(4),HTML("<br>"),
      ShowChrt(7),HTML("<br>"),ShowChrt(10),HTML("<br>")
    )
  ),
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Quarterly<br>selections 2"))),
    fluidPage(
      ShowChrt(13),HTML("<br>"),ShowChrt(16),HTML("<br>"),
      ShowChrt(19),HTML("<br>"),ShowChrt(22),HTML("<br>")
    )
  ),
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Annual<br>selections"))),
    fluidPage(
      ShowChrt(25),HTML("<br>"),ShowChrt(28),HTML("<br>"),
      ShowChrt(31),HTML("<br>"),ShowChrt(34),HTML("<br>")
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
    {Make_chrtQ(1,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Net operating surplus: corporations","","5 years")})
  output$chart2 <- renderPlot(
    {Make_chrtQ(2,3,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Household final consumption expenditure","","year")})
  output$chart3 <- renderPlot(
    {Make_chrtQ(9,4,as.Date("2008-01-01"),as.Date("2020-10-01"),
      "All industries [T001]","Real gross domestic product","year")})
  output$chart4 <- renderPlot(
    {Make_chrtQ(21,6,as.Date("2015-01-01"),as.Date("2020-10-01"),
      "Canada's net international investment position","","year")})
  output$chart5 <- renderPlot(
    {Make_chrtQ(13,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: residential structures",
      "Business residential construction investment","5 years")})
  output$chart6 <- renderPlot(
    {Make_chrtQ(22,5,as.Date("1997-01-01"),as.Date("2020-10-01"),
      "Labour productivity","","year")})
  output$chart7 <- renderPlot(
    {Make_chrtQ(20,5,as.Date("1981-01-01"),as.Date("2020-10-01"),
      "Dividends","Household dividend income","5 years")})
  output$chart8 <- renderPlot(
    {Make_chrtQ(7,1,as.Date("2017-10-01"),as.Date("2020-10-01"),
      "Cannabis products for non-medical use (licensed)",
      "Household expenditure on licensed non-medical cannabis","")})
  output$chart9 <- renderPlot(
    {Make_chrtQ(11,3,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Accommodation and food services [72]",
      "Real GDP, accommodation and food services","year")})
  output$chart10 <- renderPlot(
    {Make_chrtQ(28,3,as.Date("2015-01-01"),as.Date("2020-10-01"),
      "Total economy","Index of labour compensation per hour","year")})
  output$chart11 <- renderPlot(
    {Make_chrtQ(16,6,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Equals: net lending or net borrowing",
      "Federal government net lending/borrowing","5 years")})
  output$chart12 <- renderPlot(
    {Make_chrtQ(27,3,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Utilities [22]","Index of labour productivity in the utilities industry","5 years")})
  output$chart13 <- renderPlot(
    {Make_chrtQ(4,5,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Gross domestic product at market prices","GDP implicit price index","5 years")})
  output$chart14 <- renderPlot(
    {Make_chrtQ(31,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Real exchange rate, index 2012=100","","5 years")})
  output$chart15 <- renderPlot(
    {Make_chrtQ(31,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Terms of trade, index 2012=100","","5 years")})
  output$chart16 <- renderPlot(
    {Make_chrtQ(31,2,as.Date("2010-01-01"),as.Date("2020-10-01"),
      "Real household disposable income, volume index 2012=100","","year")})
  output$chart17 <- renderPlot(
    {Make_chrtQ(19,6,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Less: taxes on income (corporate income tax)",
      "Corporate income tax","5 years")})
  output$chart18 <- renderPlot(
    {Make_chrtQ(19,1,as.Date("1990-01-01"),as.Date("2020-10-01"),
      "Less: remitted profits of government business enterprises",
      "Remitted profits of government business enterprises","5 years")})
  output$chart19 <- renderPlot(
    {Make_chrtQ(14,1,as.Date("1961-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: non-residential structures and machinery and equipment",
      "Real business plant and equipment investment","5 years")})
  output$chart20 <- renderPlot(
    {Make_chrtQ(13,6,as.Date("1961-10-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: non-residential structures and machinery and equipment",
      "Business plant and equipment investment","5 years")})
  output$chart21 <- renderPlot(
    {Make_chrtQ(13,1,as.Date("1990-01-01"),as.Date("2020-10-01"),
      "Business gross fixed capital formation: computers and computer peripheral equipment",
      "Business investment in computers","5 years")})
  output$chart22 <- renderPlot(
    {Make_chrtQ(41,1,as.Date("1990-01-01"),as.Date("2020-10-01"),
      "Investment in inventories","","5 years")})
  output$chart23 <- renderPlot(
    {Make_chrtQ(33,1,as.Date("1981-01-01"),as.Date("2020-10-01"),
      "Household net saving","","5 years")})
  output$chart24 <- renderPlot(
    {Make_chrtQ(39,6,as.Date("2001-01-01"),as.Date("2020-10-01"),
      "Equals: general governments net lending or net borrowing",
      "Provincial and territorial net lending or borrowing","5 years")})
  output$chart25 <- renderPlot(
    {Make_chrtA(48,4,"1947","1996",
      "Gross domestic product (GDP) at market prices",
      "GDP at constant 1986 prices","3")})
  output$chart26 <- renderPlot(
    {Make_chrtA(46,1,"1947","1996",
      "Accrued net income of farm operators from farm production",
      "","5")})
  output$chart27 <- renderPlot(
    {Make_chrtA(47,5,"1947","1996",
      "Business investment, inventories",
      "","5")})
  output$chart28 <- renderPlot(
    {Make_chrtA(50,1,"1990","2020",
      "Net transactions in financial assets",
      "","3")})
  output$chart29 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Total assets",
      "Market value of all Canadian assets","5")})
  output$chart30 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Produced non-financial assets",
      "Market value of produced non-financial assets","3")})
  output$chart31 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Weapons systems",
      "Market value of weapons systems assets","3")})
  output$chart32 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Non-produced non-financial assets",
      "Market value of all non-produced non-financial assets","3")})
  output$chart33 <- renderPlot(
    {Make_chrtA(49,5,"1990","2020",
      "Natural resources",
      "Market value of natural resource assets","3")})
  output$chart34 <- renderPlot(
    {Make_chrtA(45,1,"1997","2020",
      "Energy products [C12]",
      "Volume of imports of energy products","3")})
  output$chart35 <- renderPlot(
    {Make_chrtA(34,3,"1961","2020",
      "Equals: non-profit institutions serving households' disposable income",
      "Disposable income of non-profit institutions serving households","5")})
  output$chart36 <- renderPlot(
    {Make_chrtA(5,4,"1961","2020",
      "Equals: net national income at basic prices",
      "Net national income at basic prices","5")})
    #observeEvent(output$textAbt, {
  #  message(paste0("textAbt is ",output$textAbt))
  #})
}

shinyApp(ui, server)
