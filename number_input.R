library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)

specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(numericInput("multiplier", label = "Enter multiplier", value=2)),
      box(plotlyOutput("plot_multiplier", height = 250))
    )
    
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    data %>% mutate(value2=value*input$multiplier) %>% 
      pivot_longer(starts_with("value"), names_to = "col", values_to = "val")
  })  
  
  output$plot_multiplier <-plotly::renderPlotly({
    plotly::ggplotly(ggplot(dat(), aes(fill=col, y=val, x=specie)) + 
                       geom_bar(position="dodge", stat="identity"))
  })
  
}


shinyApp(ui, server)