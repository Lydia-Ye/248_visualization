library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

# data.all <- read_csv("https://www.stat2games.sites.grinnell.edu/data/248/getdata.php") 
sample1 <- read.csv("248sample1.csv")

#data.all <- rbind(data.all, sample1)
data.all <- sample1

data.all$GroupID <- tolower(data.all$GroupID)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))

# UI
ui <- fluidPage(
  
  titlePanel("248 Data Visualization"),
  mainPanel(
    
    selectInput(inputId = "groupID",
                label = "Group ID:", 
                choices = c("all", all_groups),
                multiple = TRUE,
                selectize = TRUE,
                selected = "all"),
    
    downloadButton('downloadData', label = "Download Data")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to store filtered data
  filteredData <- reactive({
    req(data.all)  # Ensure the dataset is loaded
    
    # Filter by Group ID
    if (length(input$groupID) > 0) {
      if ("all" %in% input$groupID) {
        data <- data.all
      } else {
        data <- data.all[data.all$GroupID %in% input$groupID, ]
      }
    } else {
      data <- data.all  # No filtering if no group selected
    }
    
    data
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