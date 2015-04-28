library(shiny)

#source('environment.R')

# Define page, able to be included in other shiny application
ui_get_page <- function() {
  fluidPage(
    tags$head(tags$script(src = "message-handler.js")),
    
    img(src='potato.gif', align = "center"),
    
    hr(),
    
    textOutput('status'),
    
    hr(),
    
    fluidRow(
      column(12,
             
             h1("PhotoAnnotator"),
             
             textInput('experiment_name', label = 'Experiment'),
             
             selectInput('organism', label = 'Organism', choices = get_organisms()),
             
             textInput('author', label = 'Author'),
             
             textInput('year', label = 'Year'),
             
             textInput('directory', label = 'Image directory'),
             #fileInput('files', label = 'Files', multiple = TRUE, accept = 'image/*'),
             
             actionButton('start_annotating', 'Start annotating')
      )
    )
  )
}

# Define UI
shinyUI(ui_get_page())
