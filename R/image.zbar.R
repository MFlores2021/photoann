image.zbar <- function (imagei){
  
  cpn = "(CIP)?[0-9@yDZO/]{6}(\\.[0-9@yDZO]{1,4})?" 
  
  tfn = file.path ("bin","zxing")
  #tfn = file.path ("bin","zbar","zbarimg")
  tfna = ("java -cp bin\\zxing\\javase-3.1.0.jar;bin\\zxing\\core-3.1.0.jar com.google.zxing.client.j2se.CommandLineRunner ")
  
  
  cip <- system(paste(tfna,imagei), intern = TRUE) #, stdout = TRUE, stderr = FALSE) , show.output.on.console=FALSE
  #try(A <- system2("C:\\Program Files\\ZBar\\bin\\zbarimg.exe", args=paste("-q --raw",imagei, sep = " "), stdout = TRUE, stderr = FALSE), silent = TRUE)
  cip = str_extract(cip,cpn)
  cip = cip[!is.na(cip)]
  cip = moda.cipnumber(cip)
  
  return(cip)
}