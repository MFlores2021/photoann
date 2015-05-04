# library(shiny)
# 
# ui <- shinyUI(
#   ui_get_page()
# )
# 
# load_external_shiny_ressources <- function(packageName) {
#   
#   source_dir <- get('get_www_dir', envir = getNamespace(packageName))()
#   target_dir <- file.path('www', packageName)
#   
#   if(dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)
#   dir.create(target_dir)
#   file.copy(
#     dir(source_dir,
#         full.names = TRUE,
#         include.dirs = TRUE),
#     target_dir,
#     recursive = TRUE)
#   
#   ## (sym)links don't work with shiny ?!
#   #######################################
#   # if(.Platform$OS.type == 'unix') {
#   #     file.symlink(source_dir, target_dir)
#   # } else {
#   #     file.link(source_dir, target_dir)
#   # }
# }
# 
# 
# 
# load_external_shiny_ressources('photoann')
# 
# io_outer()
# 
# server <- shinyServer(function(input, output, session) {
#   
#   io_inner(input, output, session)
#   
# })
# 
# shinyApp(ui = ui, server = server)
