# Module for quarterly tables - qtFunc.R
# May 13, 2021

source("Tabl_specs.R")

trf1 <- c( # transformation options for a quarterly table
  "Original table",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP",
  "Five-quarter centred moving average"
)
# Starting conditions for initial quarterly table and chart
# First the full sequence of dates in "Date" format
qtrsD <- seq.Date(TS[[1]]$Strt,TS[[1]]$Endt,by="quarter")
# Now the corresponding sequence of dates in "character" format
qtrsSrt <- character()
for (i in 1:length(qtrsD)) {
  qtrsSrt[i] <- paste0(year(qtrsD[i])," Q",quarter(qtrsD[i]))
}
strtrangT <- c(qtrsSrt[length(qtrsSrt)-5],qtrsSrt[length(qtrsSrt)])

qtUI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Quarterly<br>tables"))),
    tags$style(HTML(".selectize-input, .option {
      color:black; 
      font-size:26px;
      font-family:Optima
    }")),    
    selectInput(NS(id,"tabl1"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a table:")),choices = tn,width = "100%"),
    fluidRow(
      column(10,prettyRadioButtons(NS(id,"transf1"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf1,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(2,downloadButton(NS(id,"downloadData1"),label="Download table"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose starting and ending dates:")),
      choices=qtrsSrt,
      selected=strtrangT,
      dragRange = TRUE,
      width="100%"),
    htmlOutput(NS(id,"notabl")),
    gt_output(NS(id,"tabl")) 
  )
}

qtServer <- function(id) {
  moduleServer(id,function(input,output,session) {
    tab1  <- reactive(as.numeric(input$tabl1))
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
    expr <- reactive({Make_tablQ(tab1(),type1(),qtr1(),qtr2())})
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
    observe({ # quarterly table update
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
  })
}
