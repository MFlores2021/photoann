image.zbar <- function (imagei) {
  
  cpn = "(CIP)?[0-9@yDZO/]{6}(\\.[0-9@yDZO]{1,4})?" 
  
  tfn = paste(file.path("bin", "zxing", 'javase-3.1.0.jar'), file.path("bin", "zxing", 'core-3.1.0.jar'), sep = ':')
  
  param = paste('-cp', tfn, 'com.google.zxing.client.j2se.CommandLineRunner', imagei)
  
  cip <- system2('java', param)
  
  cip = stringr::str_extract(cip,cpn)
  cip = cip[!is.na(cip)]
  cip = moda.cipnumber(cip)
  print(cip)
  stop()
  return(cip)
}