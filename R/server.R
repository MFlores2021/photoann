library(shiny)

#io_main <- function(input, output) {
#  
#  observeEvent(input$start_annotating, {
#    #menu.rename(dir_initial = input$directory,
#    #            fbp = input$experiment_name,
#    #            befCIP = 'CIP',
#    #            ppl = input$organism,
#    #            yeardate = input$year,
#    #            autor = input$author)
#    session$sendCustomMessage(type = 'testmessage',
#                              message = 'Thank you for clicking')
#  })
#}

# Define server logic
#shinyServer(io_main())
shinyServer(function(input, output, session) {
  
  observeEvent(input$start_annotating, {
    #menu.rename(dir_initial = input$directory,
    #            fbp = input$experiment_name,
    #            befCIP = 'CIP',
    #            ppl = input$organism,
    #            yeardate = input$year,
    #            autor = input$author)
    #session$sendCustomMessage(type = 'testmessage',
    #                          message = 'Thank you for clicking')
    output$status <- renderText({input$experiment_name})
  })
  
  
  
  
})