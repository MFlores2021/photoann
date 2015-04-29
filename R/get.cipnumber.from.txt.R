get.cipnumber.from.txt <-function(txt){
	
	cpn = "(CIP)?[1-9]{1}[0-9@yDZO/]{5}(\\.[0-9@yDZO]{1,4})?" 
	
	#make sure to preced cip number with CIP 
	
	txt = stringr::str_replace_all(txt," ","")
	txt = stringr::str_replace_all(txt,"@","0")
	txt = stringr::str_replace_all(txt,"y","1")
	txt = stringr::str_replace_all(txt,"D","0")
	txt = stringr::str_replace_all(txt,"Z","2")
	txt = stringr::str_replace_all(txt,"A","4")
	txt = stringr::str_replace_all(txt,"/","7")
	txt = stringr::str_replace_all(txt,"B","8")
	txt = stringr::str_replace_all(txt,"O","0")
	txt = stringr::str_replace_all(txt,"é","6")
	txt = stringr::str_replace_all(txt,"ó","6")
	txt = stringr::str_replace_all(txt,"®","0")
	txt = stringr::str_replace_all(txt,"I","1")
	txt = stringr::str_replace_all(txt,"Â","")
	txt = stringr::str_replace_all(txt,"\\]","1")
	txt = stringr::str_replace_all(txt,"\\[","1")
	txt = stringr::str_replace_all(txt,",",".")
	txt = stringr::str_replace_all(txt,"U","0")
	txt = stringr::str_replace_all(txt,"©","0")
	txt = stringr::str_replace_all(txt,"Â®","0")
	
	cip = stringr::str_extract(txt,cpn)
	
	if(is.na(cip)) cip = ''
	
	return(cip)
}
