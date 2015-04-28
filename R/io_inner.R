#' io_inner
#'
#' The inner server function, loaded inside a shiny server
#'
#' @return The inner server function for this application.
#' @author Alexander Vowinkel
#' @seealso io_outer
#' @export 
#' 

io_inner <- function(input, output, session) {

  observeEvent(input$start_annotating, handlerExpr = {
    
    progress <- Progress$new()
    progress$set(message = "Renaming files", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    menu.rename(dir_initial = input$directory,
                fbp = input$experiment_name,
                befCIP = 'CIP',
                ppl = input$organism,
                yeardate = input$year,
                autor = input$author,
                updateProgress)
    
  })
}
