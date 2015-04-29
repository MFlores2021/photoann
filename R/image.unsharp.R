
image.unsharp <- function (imagei,params){
  
  tmpfile <- file.path(get_my_tempdir(), "unsharped.jpg")
  
  if(.Platform$OS.type == "unix") {
    tfn = 'convert'
  } else {
    tfn = file.path (get_package_root(), "bin", "ImageMagick", "convert")
  }
  
  system2(tfn, args=paste(params,imagei, tmpfile, sep = " "), stdout = TRUE, stderr = FALSE)
  
}
