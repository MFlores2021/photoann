###############################################################################
# June 20, 2012
# 7:59:34 AM
# Author: Mirella Flores
# (c) International Potato Center
#
###############################################################################

morpho <- function(i){
	
	tryCatch({
    
  # deteccion de objetos
    
	pth = EBImage::thresh(i[,,1],5,5,0.0175) 	#compara intensidad de pixeles
	kern= EBImage::makeBrush(5, shape=c('disc'), step=FALSE)^2 
	pf  = EBImage:::closing(EBImage:dilate(pth, kern), kern)  #pf=closing(erode(dilate(pth, kern), kern), kern)
	pfh = EBImage::fillHull(pf)	
	nobj= EBImage::bwlabel(pfh)
	
  # deleting smaller objects
	
	pp  = EBImage::computeFeatures.shape(nobj)[,'s.area']	
	id  = which(pp<50) #mayores k
	rm  = EBImage::rmObjects(nobj,id)
	rm2 = rm
	
	if(max(rm2)==0){
		res='no se detectaron objetos'
		return(res)
	}else{
		
		# Mark objects
		colorMode(rm2) = EBImage::Grayscale
		sto   = EBImage::stackObjects(rm2,i)
		obj   = EBImage::paintObjects(rm2, i, col='red')
		
		#objects contour
		oc  = EBImage::ocontour(rm2)
		pp  = EBImage::computeFeatures.moment(rm2)[,c('m.cx','m.cy')]
		pp  = cbind(pp, EBImage::computeFeatures.shape(rm)[,c('s.area','s.perimeter')] , pp)
  	#display(obj)
		
		if(class(pp)=="numeric"){
      
			ch  = pp[1:2]
			ch  = round(ch)
			ch1 = pp[3:4]
			ch2 = pp[4:6] 			
			
			#output
			names(ch2)=c('s.perimeter','m.cx','m.cy')
			res=c(ch,ch1,ch2)		
			res=round(res,4)
			n=1			
			res=list(res,sto,sto,n)
			
			return(res)
			
		}
		else{
			ch=pp[,1:2]
			ch=round(ch)
			ch1=pp[,3:4]
			ch2=pp[,4:6] 

			#output
			colnames(ch2)=c('s.perimeter','m.cx','m.cy')
			res=cbind(ch,ch1,ch2)
			n=nrow(ch)
			res=list(res,sto,sto,n)
			
			return(res)
		}
	}
	})
	
}
imagen.pto.initial <- function(oc,pto2,ch,im,dimens,i,coord){

	tryCatch({
	
		pto   	= (oc[[1]][coord,])
		
		pto.min = min(pto[,i])
		pto.max = max(pto[,i])
		distn 	= (pto.max-pto.min)
		eje   	= pto2[i]-(ch-pto.min)
		
		if(i==1){			
			dim.init = as.numeric(image.identify(im,'%w')) 
		}		
		else if(i==2){      			
			dim.init = as.numeric(image.identify(im,'%h')) 
		}
		
		pto.real 	= abs(eje*dim.init/dimens)
		distn.real= abs(distn*dim.init/dimens)		
		result 		= list(pto.real, distn.real)
		
		return(result)
		
	}, error = function(err) {return(NULL)})
}

image.coordenada<-function(img,vect){
  
	#object selected analysis
  
	tryCatch({
    
	dist = matrix(0,length(vect),11)
	go   = 1
  
  	for(k in vect){
      
  		#detection
      
  		x   = img[[1]][,,,k]
  		pth1= EBImage::thresh(x[,,1],15,15,0.005)
  		kern= EBImage::makeBrush(13,shape='disc')
  		pf1 = EBImage::closing(pth1,kern)
  		pf1 = EBImage::fillHull(pf1)
      
  		colorMode(pf1) = EBImage::Grayscale
  		ob1 = EBImage::bwlabel(pf1)
  		oc  = EBImage::ocontour(pf1)
      ch  = EBImage::computeFeatures.moment(pf1)[,c('m.cx','m.cy')]
  		ch=round(ch)
  		
  		h1 = which(oc[[1]][,2]==ch[2]) # devuelve la coordenada
  		v1 = which(oc[[1]][,1]==ch[1])
  		
  		result=list(oc,ch,h1,v1)
  		
  		return(result)
  	}
	})
}

image.identify<-function(ima,prop){
  
  if(.Platform$OS.type == "unix") {
    tfn = 'identify'
  } else {
    tfn = file.path (get_package_root(), "bin","ImageMagick","identify")
  }
	
	dim1 <- system2(tfn,args=paste("-format", prop, ima,sep = " "), stdout = TRUE)
	
	return(dim1)
}

image.crop <-function(im){
	
	tryCatch({
	
  	out <- file.path (get_my_tempdir(),"output.jpg")
  	
  	# resize image < 300 kb
  	
  	if(.Platform$OS.type == "unix") {
  	  tfn = 'convert'
  	} else {
  	  tfn = file.path (get_package_root(), "bin","ImageMagick","convert")
  	}
  	
  	system2(tfn,args=paste("-resize 400000@",im,out, sep = " "), stdout = TRUE)
  	
    if (file.exists(out)){
      
    	i = EBImage::readImage(out)
    
    	# call to main function for detect objects
      
    	morp = morpho(i)
    	
    	# number of objects
      
    	n=morp[[4]]
    
    	dimens <- dim(i)
      
    	tamanoh = as.numeric(image.identify(im,'%h'))
    	
    	if(n > 1){
        
    		k=1
        
    		for(k in 1:n){
    
    			# get initial points in objects
          
    			xy <- morp[[1]][k,c('m.cx','m.cy')]
    			
    			# get initial points in objects in image resized
          
    			resu=image.coordenada(morp[2],vect=(k))
    			
    			# get initial points in objects in original image 
          
    			ejex <- imagen.pto.initial(resu[[1]][1],xy,resu[[2]][1],im,dimens[1],1,resu[[3]])
    			ejey <- imagen.pto.initial(resu[[1]][1],xy,resu[[2]][2],im,dimens[2],2,resu[[4]])
    
    			#convert -crop WxH {+-}x{+-}y {%} {!} input output
    
    			if (!(is.null(ejex)) && !(is.null(ejey))){
    
      			if (ejey[[2]][1]/ejex[[2]][1] < 1.2) {
      				
      				# split in 2 parts: 'corbata' labels
      				
      				if (ejex[[2]][1]/ejey[[2]][1] > 5.5) {
      							
      					outimage  <- paste(out,".",k,"2.new.jpg",sep = "")
      					outimage1 <- paste(out,".",k,"3.new.jpg",sep = "")
      
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.7),"+",ejey[[1]][1],sep=""),im, outimage1, sep = " "), stdout = TRUE)
      				}
      
      				else if (ejex[[2]][1]/ejey[[2]][1] > 5) { 
      								
      					outimage  <- paste(out,".",k,"1.new.jpg",sep = "")
      					outimage1 <- paste(out,".",k,"2.new.jpg",sep = "")
      
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage1, sep = " "), stdout = TRUE)
      				
      				}
      				else if (ejex[[2]][1]/ejey[[2]][1] > 2) { 
      			
      					outimage  <- paste(out,".",k,"1.new.jpg",sep = "")
      					outimage1 <- paste(out,".",k,"2.new.jpg",sep = "")		
      					
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1],"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",(ejex[[1]][1]+(ejex[[2]][1]*0.45)),"+",ejey[[1]][1],sep=""),im, outimage1, sep = " "), stdout = TRUE)
      				
      				}
      				else {
      					outimage  <- paste(out,".",k,".new.jpg",sep = "")
      					outimage1 <- paste(out,".",k,"1.new.jpg",sep = "")
      				
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1],"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
      					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]/2,"x",ejey[[2]][1]/2,"+",ejex[[1]][1],"+",(ejey[[1]][1]+ejey[[2]][1]/2),sep=""),im, outimage1, sep = " "), stdout = TRUE)
      				
      				}
      			}
      		} 
    			k=k+1
    		}
    	}
  	}
    
  	if(file.exists(file.path (get_my_tempdir(), 'output.jpg'))) file.remove(file.path (get_my_tempdir(),'output.jpg'))
  	
	}, error = function(err) {return(NULL)})
}

