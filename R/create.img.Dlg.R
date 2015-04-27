create.img.Dlg = function(w){
  
  dput("cancel",file="bin/temp.txt")
  
  win <- gbasicdialog(title='Add images to experiment', handler = function(h,...) {
    
    if(!is.na(svalue(gl[8,1])) && !is.na(svalue(gl[1,1]))){
      
      result = c(svalue(gl[1,2]),svalue(gl[2,2]),svalue(gl[8,1]),svalue(gl[3,2]),svalue(gl[5,2]),svalue(gl[6,2]))
      
      dput(result,file="bin/temp.txt")
    }
  }, parent=w)
  
  gl = glayout(cont=win)
  gl[1,1]=glabel("Experiment:", cont=gl)
  gl[1,2]=gedit("ExperimentName", cont=gl)
  gl[2,1]=glabel("Types of picture:", cont=gl)
  gl[2,2]=gcombobox(get.org.list(), cont=gl)
  gl[3,1]=glabel("Author(s):", cont=gl)
  gl[3,2]=gedit("Author", cont=gl)
  gl[5,1]=glabel("Before:", cont=gl)
  gl[5,2]=gedit("CIP", cont=gl) 
  gl[6,1]=glabel("After:", cont=gl)
  gl[6,2]=gedit("", cont=gl)
  gl[7,1]=gbutton("Select ...", container=gl,
                  handler = function(h,...) {
                    image.dir = choose.dir()
                    
                    if(!is.na(image.dir)){
                      
                      gl[8,1]=glabel(image.dir, cont=gl)
                      svalue(gl[8,1])= image.dir
                      gl[9,1]=glabel("                                    ", cont=gl)
                    }
                    else gl[9,1]=glabel("Enter images directory", cont=gl)
                    
                  })
  
  visible(win, set=TRUE) ## show dialog
  fn=dget("bin/temp.txt")
  dput("cancel",file="bin/temp.txt")
  fn
}