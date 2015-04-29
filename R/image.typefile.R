
image.typefile <- function (namefile){
  
  typefile = ""
  
  typefile <- do.call(rbind, strsplit(namefile,"\\."))
  
  typefile <- typefile[,length(typefile)]	
  
  return(typefile)
  
}