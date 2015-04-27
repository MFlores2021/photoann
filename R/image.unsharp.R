
image.unsharp <- function (imagei,params){
  
  tmpfile <- file.path('temp',"unsharped.jpg")
  
  tfn = file.path ("bin","ImageMagick","convert")
  
  system2(tfn, args=paste(params,imagei, tmpfile, sep = " "), stdout = TRUE, stderr = FALSE)
  
}
