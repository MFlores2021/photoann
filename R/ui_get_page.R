library(shiny)

#' ui_get_page
#'
#' To load a shiny page into a shiny application.
#'
#' @return A UI defintion that can be passed to the shinyUI function.
#' @author Alexander Vowinkel
#' @export 
#' 

ui_get_page <- function() {
  fluidPage(
    
    fluidRow(
      column(6,
             
             h1("PhotoAnnotator"),
             
             textInput('experiment_name', label = 'Experiment', value = 'test'),
             
             selectInput('organism', label = 'Organism', choices = get_organisms()),
             
             textInput('author', label = 'Author', value = 'me'),
             
             textInput('year', label = 'Year', value = '2015'),
             
             textInput('directory', label = 'Image directory', value = '/home/kaktus42/Schreibtisch/test'),
             #fileInput('files', label = 'Files', multiple = TRUE, accept = 'image/*'),
             
             uiOutput("control_annotating")
             
      ),
      column(6,
             
             uiOutput("picture"),
             
             htmlOutput("log")
      )
      
    )
  )
}
