file.verify.name <- function (namefile){
  
  N			=	stringr::str_replace_all(string=namefile, pattern=" ", repl="")
  
  file.rename(namefile, N)
  
  namefile	=	N
  
  return(namefile)
}