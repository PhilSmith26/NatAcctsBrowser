# Module for quarterly charts - qcFunc.R
# May 13, 2021

source("Tabl_specs.R")

trf2 <-  c( # transformation options for a quarterly chart
  "Original series",
  "Include a trend line",
  "Index, starting quarter = 100",
  "One-quarter % change",
  "Four-quarter % change",
  "Percentage of GDP",
  "Five-quarter centred moving average"
)

qcUI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Quarterly<br>charts"))),
    selectInput(NS(id,"tabl2"), tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices = tn,width = "100%"),
    selectInput(NS(id,"chrt"), tags$b(tags$span(style="color:blue", 
      "Choose a time series to chart:")),choices = ser_01,selectize=FALSE,width = "100%"),
    fluidRow(
      column(6,prettyRadioButtons(NS(id,"transf2"), tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf2,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(4,textInput(NS(id,"altTitl"),label="Choose your own chart title (optional):",
        value="",width="90%")),
      column(2,downloadButton(NS(id,"downloadData2"),label="Download chart"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"ChrtDats"),label= #tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:",#)),
      choices=qtrsSrt,
      selected=qtrsSrt,
      dragRange = TRUE,
      width="100%"),
    htmlOutput(NS(id,"nochart")),
    plotOutput(NS(id,"chart"))
  )
}

qcServer <- function(id) {
  moduleServer(id,function(input,output,session) {  
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
    altTitle <- reactive({input$altTitl})
    chartP <- reactive({if (!(TS[[tab2()]]$Idx & type2()==6))
        Make_chrtQ(tab2(),type2(),qtr1c(),qtr2c(),MYtitl(),altTitle(),"year")})   
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData2 <- downloadHandler(
      filename=function() {
        paste0(TS[[tab2()]]$STCno,".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
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
  })
}

  