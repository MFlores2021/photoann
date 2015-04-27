
compare.result <- function(cipz,cipt,list.cip) {
  
  cip = NULL
  strcip = NULL
  cipnumber = NULL
  #list.cip = str_replace_all(list.cip,"CIP","")
  ciptmp=c(cipt,cipz)
  ciptmp=ciptmp[ciptmp != ""]
  
  # if (length(ciptmp)){
  # for (i in 1:length(ciptmp)) {
  # if (ciptmp[i] %in% list.cip) cip = c(cip,ciptmp[i])
  # }}
  
  #if (!is.null(cipz)) if (cipz %in% list.cip) cip = cipz
  #if (!is.null(cipt)) if (cipt %in% list.cip) cip = c(cipt,cip)
  
  if(is.null(cip)) {
    
    cip = moda.cipnumber(ciptmp)
    
    if (!is.null(cip)){
      if (length(cip)>1) strcip = 'CIP' #'Please-check-CIP'
      else {
        if (cip != '') strcip = 'CIP' #'Please-check-CIP'
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