library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

# data.all <- read_csv("https://www.stat2games.sites.grinnell.edu/data/248/getdata.php") 
sample1 <- read_csv("~/Desktop/Kuiper MAP/248/248sample1.csv")

# data.all <- rbind(data.all, sample1)
data.all <- sample1

data.all$GroupID <- tolower(data.all$GroupID)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))

# UI
ui <- fluidPage(
  titlePanel("248 Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "GroupID",
                  label = "Group ID:", 
                  choices = c("all", all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "PlayerID",
                  label = "Player ID:",
                  choices =  c("all", all_players),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "xvar",
                  label = "X-Variable:", 
                  choices = c("Number of Success", "Number of Guesses", "Success Rate"),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "Number of Success"),
      
      selectInput(inputId = "yvar",
                  label = "Y-Variable:", 
                  choices = c("Count"),
                  multiple = FALSE,
                  selected = "Count"),
      
      #percentage or count
      #playerid as x-var; y: num of trials
      #color by right/wrong
      
      #checkbox: it's the first time?
      checkboxInput("firstPlay", 
                    "First time playing the game?", 
                    value = FALSE),
      
      downloadButton('downloadData', label = "Download Data")
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter game data based on selected group ID and playerID
  filteredData <- reactive({
    req(data.all)  # Ensure the dataset is loaded
    
    if("all" %in% input$GroupID){
      data <- data.all
    } else {
      if("all" %in% input$PlayerID){
        data <- filter(data.all, GroupID %in% input$GroupID)
      } else {
        data <- filter(data.all, GroupID %in% input$GroupID, PlayerID %in% input$PlayerID)
      } 
    }
    return(data)
  }) # plotDataR
  
  #Dynamic PlayerID Input
  observe({
    data <- filter(data.all, GroupID %in% input$GroupID)
    # Update player IDs based on selected group ID
    updateSelectInput(session, 
                      "PlayerID",
                      choices = c("all", sort(unique(data$PlayerID))),
                      selected = "all")
  }) # observe
  
  
  # Generate plot
  output$barPlot <- renderPlot({
    plotData <- filteredData()
    
    plotData <- plotData %>%
      group_by(PlayerID) %>%
      mutate(SuccessRate = sum(Result == TRUE) / n()) %>%
      mutate(NumofSuccess = sum(Result == TRUE)) %>%
      mutate(NumofGuesses = n())
    plotData <- na.omit(plotData)
    
    if (input$xvar == "Number of Success") {
      plot <- ggplot(plotData, aes(x = NumofSuccess, fill = guessCorrect)) + 
              geom_bar(position = "stack")
    } else if (input$xvar == "Number of Guesses") {
      plot <- ggplot(plotData, aes(x = NumofGuesses, fill = guessCorrect)) + 
              geom_bar(position = "stack")
    } else if (input$xvar == "Success Rate") {
      plot <- ggplot(plotData, aes(x = SuccessRate, fill = guessCorrect)) + 
              geom_bar(position = "stack")
    }
    
    plot
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(filteredData(), con)
    }
  )
}

# Running Shiny App
shinyApp(ui = ui, server = server)
