library(shiny)
library(shinythemes)
library(openxlsx)
library(shinyBS)
library(shinyWidgets)
library(openxlsx)
library(writexl)
library(readxl)
library(DT)

ui <- fluidPage(
  
  ui <- shiny::navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                          br(),
                          
                          tabPanel("",
                                   fileInput("file", "Please upload a file", accept = c(".xlsx")),
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       uiOutput("date"),
                                       selectInput("d1", label = h4("D1"),""),
                                       selectInput("d2", label = h4("D2"),""),
                                       br(),
                                       
                                     ),
                                     
                                     mainPanel( 
                                     ))
                          )))


server <- function(input, output, session) {
  data <- eventReactive(input$file, {
    if (is.null(input$file)) {
      return(NULL)
    }
    else {
      df <- read_excel(input$file$datapath)
      return(df)
    }
  })
  
  output$date <- renderUI({
    if (!is.null(input$file)) {
      
      all_dates <- seq(as.Date(min(data()$date)), as.Date(max(data()$date)), by = "day")
      disabled <- as.Date(setdiff(all_dates, as.Date(data()$date)), origin = "1970-01-01")
      
      dateInput(input = "date", 
                label = "Select Date",
                min = min(data()$date),
                max = max(data()$date),
                value = max(data()$date),
                format = "dd-mm-yyyy",
                datesdisabled = disabled)
    }
    else {
      dateInput(input = "date", 
                label = "Select Date",
                min = min("1970-01-01"),
                max = max(Sys.Date()),
                format = "dd-mm-yyyy")
    }
  })
  
  observeEvent(input$file, {
    
    req(data)
    data <- data()
    data$date <- as.Date(data$date)
    print(data)
    updateSelectInput(session, "d1", label = "D1", unique(data$d1))
    updateSelectInput(session, "d2", label = "D2", unique(data$d2))
  })
}

shinyApp(ui = ui, server = server)