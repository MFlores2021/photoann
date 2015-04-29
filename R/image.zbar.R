image.zbar <- function (imagei) {
  
  cpn = "(CIP)?[0-9@yDZO/]{6}(\\.[0-9@yDZO]{1,4})?" 
  
  tfn = paste(file.path(get_package_root(), 'bin', "zxing", 'javase-3.1.0.jar'), file.path(get_package_root(), 'bin', "zxing", 'core-3.1.0.jar'), sep = ':')
  
  param = paste('-cp', tfn, 'com.google.zxing.client.j2se.CommandLineRunner', imagei)
  
  cip <- system2('java', param, stdout = TRUE)
  
  cip = stringr::str_extract(cip,cpn)
  cip = cip[!is.na(cip)]
  if(length(cip[!is.na(cip)]) > 0) {
    cip = moda.cipnumber(cip)
  }
  cip
}

get_package_root <- function() {
  pck_data <- installed.packages()[, c("Package", "Version", "LibPath")]
  pck <- pck_data[pck_data[,1] == 'photoann']
  if(!is.null(dim(pck))) {
    pck_version <- sessionInfo()[[6]][['photoann']][['Version']]
    pck <- pck[pck[,2] == pck_version,]
    pck <- pck[1,]
  }
  file.path(pck[3], 'photoann')
}