#' menu.rename
#'
#' The typical hello world example.
#'
#' @param dir_initial
#' @param fbp
#' @param befCIP
#' @param ppl
#' @param yeardate
#' @param autor
#' @return character a string; default is 'Hello, world!'
#' @author Mirella Flores
#' @author Alexander Vowinkel
#' @export 
#' 

menu.rename <- function(input, output, session, rvRename) {
  
  dir_final <- NULL
  oldfiles <- NULL
  befCIP <- 'CIP'
  dir_initial <- NULL
  fbp <- NULL
  ppl <- NULL
  yeardate <- NULL
  autor <- NULL
  part_plant <- NULL
  listcipn <- NULL
  
  shiny::observe({
    
    if(rvRename$stage != 1) {
      return()
    }
    print('prepare')
    
    rvRename$log <- "Start renaming files\n"
    
    isolate({
      
      dir_initial <<- isolate(input$directory)
      fbp <<- isolate(input$experiment_name)
      ppl <<- isolate(input$organism)
      yeardate <<- isolate(input$year)
      autor <<- isolate(input$author)
      part_plant <<- part.plant(ppl)
      
      initialize()
      
      # create folder
      dir_final <<- file.path(dir_initial, paste('renamed_', part_plant, sep = ''))
      #unlink(dir_final, recursive = TRUE)
      if(!file.exists(dir_final)) {
        dir.create(dir_final, recursive = TRUE)
      } else {
        isolate({
          rvRename$log <- paste(rvRename$log,
                                paste('output directory "', dir_final, '" already exists. Stopping.', sep = ''),
                                sep = '\n')
        })
        return()
      }
      
      # copy files to new directory
      oldfiles <<- list.files(dir_initial, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
      dput("List", file = paste(dir_final,"log.txt"))
      
      # progress bar
      #my_progress <- Progress$new()
      #my_progress$initialize(session, min = 0, max = length(oldfiles))
      
      if (length(oldfiles)) {
        file.copy(file.path(dir_initial,oldfiles), dir_final)
        listcipn <<- file.path(dir_initial,'cipnumber.txt')
        if(file.exists(listcipn)) listcipn <<- readLines(listcipn) else listcipn <<- ''
        
        # list into new directory
        oldfiles <<- list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
        oldfiles <<- file.path(dir_final,oldfiles)
        
        #### loop over files reactive
        rvRename$n <- length(oldfiles)
      }
    })
  })
  
  # worker step 2
  shiny::observe({
    
    if(rvRename$stage != 2) {
      return()
    }
    print("worker1")
    isolate({ 
#       prev_file_path <- file.path(getwd(), 'www', 'photoann')
#       prev_file_name <- paste0('prev', basename(oldfiles[rvRename$i]))
#       print(file.path(prev_file_path, prev_file_name))
#       rvRename$files <- c(rvRename$files, prev_file_name)
#       file.copy(oldfiles[rvRename$i], file.path(prev_file_path, prev_file_name))
      
      #my_progress$set(value = rvRename$i, detail = basename(oldfiles[rvRename$i]))
      #append_log(rvRename, paste('working on file: ', basename(oldfiles[rvRename$i])))
      
      Sys.sleep(0.5)
      rvRename$log <- paste(rvRename$log,
                            paste0('working on file (', rvRename$i, '/', rvRename$n, '): ', basename(oldfiles[rvRename$i])),
                            sep = '\n')
      
      oldfiles[rvRename$i] <<- file.verify.name(oldfiles[rvRename$i])
      write(basename(oldfiles[rvRename$i]),file=paste(dir_final,"log.txt"), append = TRUE)
    })
    
  })
  
  # worker step 2
  shiny::observe({
    if(rvRename$stage != 3) {
      return()
    }
    print("worker2")
    
    isolate({
      CIPN <- image.rename(oldfiles[rvRename$i],listcipn)
      #Sys.sleep(1)
      #CIPN <- c()
      
      if (length(CIPN)){  
        file_exists = file.path(dir_final,list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp"))
        count=2
        new_name = file.path(dir_final,paste(befCIP,CIPN,yeardate,part_plant,'.jpg',sep=''))
        
        while(new_name %in% file_exists) { 		          
          new_name = file.path(dir_final,paste(befCIP,CIPN,yeardate,part_plant,count,'.jpg',sep=''))
          count=count+1
        }
        
        file.rename(oldfiles[rvRename$i],new_name)
        write(basename(new_name),file=paste(dir_final,"log.txt"),append=TRUE)
        rvRename$log <- paste(rvRename$log,
                              paste0('suggested CIP number: ', paste(CIPN, collapse = ', ')),
                              paste0('new name: ', basename(new_name)),
                              sep = '\n')
      } else {
        rvRename$log <- paste(rvRename$log,
                              paste('could not identify CIP number.'),
                              sep = '\n')
      }
      
      write("--------*--------",file=paste(dir_final,"log.txt"),append=TRUE)
    })
    
  })
  
  #return(menu.rename.prepare)
}
