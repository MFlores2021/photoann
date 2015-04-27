
image.rename <- function(oldfilesi,filecip){
  
  tmpfile     =  file.path('temp','unsharped.jpg')
  
  file_exists = list.files(".", pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
  
  tryCatch(image.crop(oldfilesi), error = function(err) {return(NULL)})
  
  labelfiles = file.path('temp',list.files('temp', pattern = ".new.jpg"))
  
  if(!length(labelfiles)) labelfiles=oldfilesi
  
  x <- 1
  
  CIPN=NULL
  CIPT=NULL
  CIPZ=NULL
  CIPN1=NULL
  cipnumber=NULL
  
  for(x in 1:length(labelfiles)){
    
    tryCatch({		
      
      if(is.null(CIPZ)){
        
        Z <- image.zbar(labelfiles[x])
        
        if (Z != '' && length(Z)) {
          
          CIPZ = c(Z,CIPZ) 					
          
          file.remove(list.files("temp", pattern = ".new.jpg", full.names = TRUE))
          
          break
        }
        
        T <- read.cipnumber.from.image(labelfiles[x])
        
        if (T != '' && length(T)){
          
          CIPT = c(CIPT,T) 	
          
        }					
        
        if(is.null(CIPZ)){
          
          params = "-unsharp 6x3"
          
          image.unsharp(labelfiles[x],params)
          
          if(file.exists(tmpfile)){			
            
            Z <- image.zbar(tmpfile)
            
            if (Z != '' && length(Z)) {
              
              CIPZ = c(Z,CIPZ) 					
              
              file.remove(list.files("temp", pattern = ".new.jpg", full.names = TRUE))
             
              break
              
            }
            
            T <- read.cipnumber.from.image(tmpfile)
            
            if (T != '' && length(T))	CIPT = c(CIPT,T) 	
            
            file.remove(tmpfile)
            
          }				
          
          if(is.null(CIPZ)){
            
            params = "-unsharp 3x1+7"
            
            image.unsharp(labelfiles[x],params)
            
            if(file.exists(tmpfile)){						
              
              Z <- image.zbar(tmpfile)	
              
              if (Z != '' && length(Z)) {
                
                CIPZ = c(Z,CIPZ) 		
                
                file.remove(list.files("temp", pattern = ".new.jpg", full.names = TRUE))
                
                break
                
              } 							
              
              T <- read.cipnumber.from.image(tmpfile)
              
              if (T != '' && length(T))	CIPT = c(CIPT,T) 		
              
              file.remove(tmpfile)
              
            }
            
          }}
      }						
      
    }, error=function(cond) NA )
    
    if(labelfiles[x]!=oldfilesi)	file.remove(labelfiles[x])
    
  } 
  
  
  tryCatch({
    
    CIPN = CIPZ
    
    if (!length(CIPN) || is.null(CIPN)){
      
      Z <- image.zbar(oldfilesi)		
      
      if (Z != '' && length(Z)){
        CIPN = Z 				
      }
      
      if (!length(CIPN) || is.null(CIPN)){				
        
        T <- read.cipnumber.from.image(oldfilesi)		
        
        if (T != '' && length(T)){
          CIPT = c(CIPT,T)							
        }
        
        CIPN = compare.result(CIPZ,CIPT,filecip)						
        
      }
    }
    
    if(length(CIPN) || !(is.null(CIPN))){
      
      CIPN = str_replace_all(CIPN,"CIP","")
      
      cipnumber <-paste("CIP", CIPN, sep="")
      
    }
    
  }, error=function(cond) NA )
  
  if(file.exists(tmpfile)) file.remove(tmpfile)
  
  file.remove(list.files("temp", pattern = ".jpg", full.names = TRUE))
  
  cipnumber = str_replace_all(cipnumber,"CIP","")
  
  return (cipnumber)
  
}