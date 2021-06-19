# Module for annual charts - acFunc.R
# May 13, 2021

source("Tabl_specs.R")

tac <-  c( # transformation options for an annual chart
  "Original series",
  "Include a trend line",
  "Index, starting year = 100",
  "One-year % change",
  "Percentage of GDP",
  "Three-year centred moving average"
)

strtrangCA <- c("2015","2020")

acUI <- function(id) {
  tabPanel(theme=shinytheme("journal"),
    tags$b(tags$span(style="color:blue", HTML("Annual<br>charts"))),
    tags$style(HTML(".selectize-input, .option {
      color:black; 
      font-size:26px;
      font-family:Optima
    }")),    
    selectInput(NS(id,"tabl"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a table:")),choices = tn,width = "100%"),
    selectInput(NS(id,"chrtA"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a time series to chart:")),choices = ser_01,width = "100%"),
    fluidRow(
      column(6,prettyRadioButtons(NS(id,"transfm"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=tac,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(4,textInput(NS(id,"altTitl"),label="Choose your own chart title (optional):",
        value="",width="90%")),
      column(2,downloadButton(NS(id,"downloadAchart"),label="Download chart"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"ChrtDates"),label= 
      "Choose starting and ending dates:",
      choices=annsSrt,
      selected=strtrangCA,
      dragRange = TRUE,
      width="100%"),
    htmlOutput(NS(id,"nochartA")),
    plotOutput(NS(id,"chartA"))
  )
}

acServer <- function(id) {
  moduleServer(id,function(input,output,session) {
    tabno <- reactive(as.numeric(input$tabl))
    type <- reactive(case_when(
      input$transfm=="Original series"~1,
      input$transfm=="Include a trend line"~2,
      input$transfm=="Index, starting year = 100"~3,
      input$transfm=="One-year % change"~4,
      input$transfm=="Percentage of GDP"~5,
      input$transfm=="Three-year centred moving average"~6
    ))
    Nchoices <- reactive({TS[[tabno()]]$Nchoices})
    observeEvent(input$tabl,{updateSelectInput(session,"chrtA",
      choices=Nchoices())})
    year1A <- reactive({as.numeric(input$ChrtDates[1])})
    year2A <- reactive({as.numeric(input$ChrtDates[2])})
    MYtitlA <- reactive({input$chrtA})
    output$nochartA <- renderUI({
      if (TS[[tabno()]]$Idx & type()==5)
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
    chartPA <- reactive({if (!(TS[[tabno()]]$Idx & type()==5))
        Make_chrtA(tabno(),type(),year1A(),year2A(),MYtitlA(),altTitle(),"")})   
    output$chartA <- renderPlot({chartPA()},height=700)
    output$downloadAchart <- downloadHandler(
      filename=function() {
        paste0(TS[[tabno()]]$STCno,".png")
      },
      content=function(file) {
        ggsave(file,chartPA(),height=8,width=14,dpi=300)
      }
    )
    observe({
      annsRange <- as.character(c(TS[[tabno()]]$StrtA:TS[[tabno()]]$EndtA))
      picks <- annsRange
      n <- length(annsRange)
      annsRang1 <- c(annsRange[1],annsRange[n])
      updateSliderTextInput(session,"ChrtDates",tags$b(tags$span(style="color:blue", 
        "Choose starting and ending dates:")),
        choices = picks,
        selected=annsRang1)
    })
  })
}
