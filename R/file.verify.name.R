file.verify.name <- function (namefile){
  
  N			=	str_replace_all(string=namefile, pattern=" ", repl="")
  
  file.rename(namefile, N)
  
  namefile	=	N
  
  return(namefile)
}