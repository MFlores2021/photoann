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
#' @export 
#' 

menu.rename <- function(dir_initial,fbp,befCIP,ppl,yeardate,autor){
  
  part_plant = part.plant(ppl)
  
  # create folder
  
  dir_final <- file.path(fbp,part_plant) 
  
  if(!file.exists(dir_final)) dir.create(dir_final, recursive = TRUE)
  
  
  # copy files to new directory
  
  oldfiles=list.files(dir_initial, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
  
  contador <- 0
  
  dput("List",file=paste(dir_final,"log.txt"))
  
    
  if (length(oldfiles)){
    
    pb <- winProgressBar("Renaming", "Progress in %",0, 100, 1)
    
    file.copy(file.path(dir_initial,oldfiles), dir_final)
    
    listcipn = file.path(dir_initial,'cipnumber.txt')
    
    if(file.exists(listcipn)) listcipn = readLines(listcipn) else listcipn = ''
    
    
    # list into new directory
    
    oldfiles = list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
    
    oldfiles = file.path(dir_final,oldfiles)
    
    keys <- paste(ppl,';',"Renaming")
    
    
    for(i in 1:length(oldfiles)) {
      
      # main function: recursive rename
      
      oldfiles[i] = file.verify.name(oldfiles[i])
      
      print(oldfiles[i])		
      
      write(basename(oldfiles[i]),file=paste(dir_final,"log.txt"),append=TRUE)
      
      CIPN <- image.rename(oldfiles[i],listcipn)
      
      
      if (length(CIPN)){				
        
        #CIPN	= image.name(CIPN)
        
        file_exists = file.path(dir_final,list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp"))
        
        count=2
        
        new_name = file.path(dir_final,paste(befCIP,CIPN,yeardate,part_plant,'.jpg',sep=''))
        
        while(new_name %in% file_exists){ 		
          
          new_name = file.path(dir_final,paste(befCIP,CIPN,yeardate,part_plant,count,'.jpg',sep=''))
          
          count=count+1
        }
        
        file.rename(oldfiles[i],new_name)
        
        write(basename(new_name),file=paste(dir_final,"log.txt"),append=TRUE)
        
        #add.legend(yeardate,autor,new_name)
        
        contador= contador+1
        
      } 
      
      write("--------*--------",file=paste(dir_final,"log.txt"),append=TRUE)
      
      setWinProgressBar(pb, round(i/length(oldfiles)*100, 0), label=paste(round(i/length(oldfiles)*100, 0),"% done"))
   
    }
    
    close(pb)
    
  }
  
  msg = paste("Renamed:",contador,"files", sep = " ")
  
 # gmessage(msg,ico="info")
  
}
