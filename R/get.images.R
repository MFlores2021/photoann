get.images <- function (w){
  
  #tryCatch({
  
  res = create.img.Dlg(w)
  
  if ( length(res) == '6'){
    
    fbp   = file.path("Renamed",res[1])
    local = res[5] #localities.list(res[5])
    menu.rename(res[3],fbp,local,res[2],res[6],res[4])
  }
  #}, error=function(cond) NA )
} 
