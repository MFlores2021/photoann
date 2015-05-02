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
  
  rvRename <- reactiveValues(log = '', isWorking = FALSE, stage = 0, image = NULL, i = 0, n = 1, files = list())
  
  output$control_annotating <- renderUI({
    if(rvRename$isWorking == FALSE) {
      actionButton('start_annotating', 'Start Annotating')
    } else {
      return('')
    }
  })
  
  output$picture <- renderUI({
    #     print('pic')
    #     if(!rvRename$isWorking || is.null(rvRename$files) || length(rvRename$files) < 1) {
    if(!rvRename$isWorking) {
      img(src = 'photoann/potato.gif', align = "center")
    }
    #     } else {
    #       file = rvRename$files[[length(rvRename$files)]]
    #       print(paste0('photoann/', file))
    #       img(src = paste0('photoann/', file), align = "center")
    #     }
  })
  
  observeEvent(input$start_annotating, {
    print("fire")
    rvRename$isWorking <- TRUE
    rvRename$stage <- 0
    rvRename$i <- 0
  })
  
  # worker - wrapped by observe internally
  #menu.rename.prepare <- menu.rename(input, output, session, rvRename)
  menu.rename(input, output, session, rvRename)
  
  # loop
  observe({
    print("looper")
    if(rvRename$isWorking) {
      isolate({
        if(rvRename$i >= rvRename$n) {
          print('invalidate isWorking in looper')
          rvRename$isWorking <- FALSE
          rvRename$log <- paste(rvRename$log,
                                'Finished renaming.',
                                sep = '<br />')
        } else {
          print('invalidate stage in looper')
          if(rvRename$stage == 3 || rvRename$stage == 1) {
            print('invalidate i in looper')
            rvRename$i <- rvRename$i + 1
            rvRename$stage <- 2
          } else if(rvRename$stage == 2) {
            rvRename$stage <- 3
          } else if(rvRename$stage == 0) {
            rvRename$stage <- 1
          }
        }
      })
      invalidateLater(1, session)
    }
  })
  
  output$log <- renderText({
    print("text")
    rvRename$log
  })
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    for(x in a) {
      file.remove(x)
    }
  })
  
}
