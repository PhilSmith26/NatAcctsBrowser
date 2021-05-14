# Module for annual tables - atFunc.R
# May 13, 2021

source("Tabl_specs.R")

trf1A <- c( # transformation options for an annual table
  "Original table",
  "Index, starting year = 100",
  "One-year % change",
  "Percentage of GDP",
  "Three-year centred moving average"
)
annsSrt <- as.character(c(TS[[1]]$StrtA:TS[[1]]$EndtA))
# Now the starting range for a table - the last six years
annsrangT <- c("2015","2020")

atUI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Annual<br> tables"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput(NS(id,"tabl1A"), tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,selectize=FALSE,width = "100%"),
    fluidRow(
      column(10,prettyRadioButtons(NS(id,"transf1A"), tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf1A,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(2,downloadButton(NS(id,"downloadData1A"),label="Download table"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"DatesA"),label= #tags$b(tags$span(style="color:blue", 
      "Choose starting and ending years:",#)),
      choices=annsSrt,
      selected=annsrangT,
      dragRange = TRUE,
      width="100%"),
    htmlOutput(NS(id,"notablA")),
    gt_output(NS(id,"tablA"))
  )
}

atServer <- function(id) {
  moduleServer(id,function(input,output,session) {
    tab1A <- reactive(as.numeric(input$tabl1A))
    type1A <- reactive(case_when(
      input$transf1A=="Original table"~1,
      input$transf1A=="Index, starting year = 100"~2,
      input$transf1A=="One-year % change"~3,
      input$transf1A=="Percentage of GDP"~4,
      input$transf1A=="Three-year centred moving average"~5
    ))
    year1 <- reactive({as.numeric(input$DatesA[1])})
    year2 <- reactive({as.numeric(input$DatesA[2])})
    output$notablA <- renderUI({
      if (TS[[tab1A()]]$Idx & type1A()==4)
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
    exprA <- reactive({Make_tablA(tab1A(),type1A(),year1(),year2())})
    output$tablA <- render_gt({
      if (!(TS[[tab1A()]]$Idx & type1A()==4))
        exprA()[[1]]
    })
    output$downloadData1A <- downloadHandler(
      filename=function() {
        paste0(TS[[tab1A()]]$STCno,".csv")
      },
      content=function(file) {
        write.csv(exprA()[[2]],file)
      }
    )
    observe({ # annual table update
      annsRange <- as.character(c(TS[[tab1A()]]$StrtA:TS[[tab1A()]]$EndtA))
      picksA <- annsRange
      annsrang1 <- c("2015","2020")
      updateSliderTextInput(session,inputId="DatesA",tags$b(tags$span(style="color:blue", 
        label="Choose starting and ending dates:")),
        choices = picksA,
        selected=annsrang1)
    })
    observeEvent(input$DatesA, {
      message(paste0("input$DatesA[1] is ",input$DatesA[1]))
      message(paste0("input$DatesA[2] is ",input$DatesA[2]))
    })
  })
}