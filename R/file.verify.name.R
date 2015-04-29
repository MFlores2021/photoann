file.verify.name <- function (namefile){
  
  N			=	stringr::str_replace_all(string = basename(namefile), pattern = stringr::perl("[^a-zA-Z0-9._-]"), repl="-")
  P = dirname(namefile)
  
  file.rename(namefile, file.path(P, N))
  
  namefile	=	file.path(P, N)
  
  return(namefile)
}