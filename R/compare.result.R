
compare.result <- function(cipz,cipt,list.cip) {
  
  cip = NULL
  strcip = NULL
  cipnumber = NULL

  ciptmp=c(cipt,cipz)
  ciptmp=ciptmp[ciptmp != ""]
    
  if(is.null(cip)) {
    
    cip = moda.cipnumber(ciptmp)
    
    if (!is.null(cip)){
      if (length(cip)>1) strcip = 'CIP' 
      else {
        if (cip != '') strcip = 'CIP' #
      }}
  }
  else {
    cip = moda.cipnumber(cip)
    strcip = 'CIP'
  }	
  
  if(!is.null(strcip)) {
    
    if (length(cip)>1) {
      cip = unique(cip)
    }
    
    cipnumber = paste(strcip,paste(cip, collapse='-'), sep = "")
  }
  return(cipnumber)
}