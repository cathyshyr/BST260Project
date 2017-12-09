# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)
library(ggrepel)
library(tidyverse)
data_long <- read.csv("https://raw.githubusercontent.com/wangcathy/BST260Project/master/testing_data.csv", header = TRUE)
server = function(input, output) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data_long
    if (input$year != "All") {
      data <- data[data$yearID == input$year,]
    }
    if (input$team != "All") {
      data <- data[data$franchID == input$team,]
    }
    if (input$pred != "All") {
      data <- data[data$Pred == input$pred,]
    }
    data2 <- data %>%
      select(yearID, franchID, Pred, Truth, Val) %>%
      rename(Year = yearID, Team = franchID, Statistic = Pred, Predicted = Val)
    rownames(data2) <- NULL
    data2
  }))
  output$prediction <- renderPlot({ 
    data <- data_long
    if(input$team == "All"){
      A = data$franchID
    }else A = input$team
    if(input$year == "All"){
      B = data$yearID
    }else B = input$year
    if(input$pred == "All"){
      C = data$Pred
    }else C = input$pred
   
     data %>% 
      filter(franchID == A, Pred == C, yearID == B) %>%
        ggplot(aes(x = Truth, y = Val)) +
        geom_point(aes(color = Pred)) +
        geom_abline(slope = 1, intercept = 0) +
        ggtitle("True v Predicted Wins") +
        xlab("True Wins") +
        ylab("Predicted Wins") +
        xlim(40, 125) +
        ylim(40, 125)
    
  })
}

# Load the ggplot2 package which provides
# the 'mpg' dataset.

ui = fluidPage(
  titlePanel("WAR Database by Team and Year"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("year",
                       "Year:",
                       c("All",
                         unique(as.character(data_long$yearID))))
    ),
    column(4,
           selectInput("team",
                       "Team:",
                       c("All",
                         unique(as.character(data_long$franchID))))
    ),
    column(4,
           selectInput("pred",
                       "WAR Statistic:",
                       c("All",
                         unique(as.character(data_long$Pred))))
    )
  ),
  # Create a new row for the table.
    DT::dataTableOutput("table"),
    plotOutput("prediction")
  )


shinyApp(ui = ui, server = server)
