#' add.legend
#' 
#' algo
#' 
#' @param year
#' @param author
#' @param im
#' @export
#' 

add.legend <- function (year,autor,im){
  
  
  h=as.numeric(image.identify(im,'%h')) 
  
  w=as.numeric(image.identify(im,'%w')) 
  
  numberautor <- do.call(rbind, strsplit(autor,"\\,"))
  
  if ((length(numberautor))>1){
    
    str_autor = "Authors:"
    
  }	
  
  else str_autor = "Author:"
  
  if(w>h){
    
    point_size = round(as.numeric(w*0.01))
    
  }
  
  else{
    
    point_size = round(as.numeric(h*0.01))
    
  }
  
  point = h*0.96
  
  tfn = file.path ("bin","ImageMagick","convert")
  
  txt = paste('" ?Copyright 2015 International Potato Center.\n ',str_autor,autor,' "',sep='')
  
  tryCatch(system2(tfn,args=paste("-pointsize ", point_size," -annotate +1+",point," ",txt," ",im," ",im, sep = ""), stdout = TRUE), error = function(cond)NA ) 
  
}
