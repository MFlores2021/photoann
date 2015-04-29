
read.cipnumber.from.image <- function(fp){
  
  #CIP number pattern/regular expression
  
  txt = read.txt.from.image(fp)
  
  cip = get.cipnumber.from.txt(txt)
  
  if (cip == '111111' || cip == '777777' || stringr::str_detect(cip,'12345')) cip = ''
  #cip =  NULL
  return(cip)
  
}