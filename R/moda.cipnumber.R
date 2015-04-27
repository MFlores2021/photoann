moda.cipnumber <- function(x) {
  z <- table(x)
  n <- names(z)[z == max(z)]
  
  return(n)
}