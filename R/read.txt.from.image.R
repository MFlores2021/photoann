
read.txt.from.image <- function(fp){
  
  tmpfile <- file.path(get_my_tempdir(), 'unsharped.jpg.txt')
  
  if(file.exists(tmpfile)) file.remove(tmpfile)
  
  #call tesseract via command line from R
  if(.Platform$OS.type == "unix") {
    tfn = 'tesseract'
  } else {
    tfn = file.path (get_package_root(), "bin","tesseract","tesseract")
  }
  
  cmd = paste(tfn,fp,fp)
  
  system(cmd,ignore.stdout = TRUE,show.output.on.console = FALSE)
  
  #read archive (several lines)
  
  txt = readLines(paste(fp,".txt",sep=""))
  
  #just make one string out of it separating former lines with ;
  
  txt = paste(txt,collapse=";")
  
  file.remove(paste(fp,".txt", sep = ""))

  return(txt)  
}