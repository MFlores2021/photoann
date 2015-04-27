###############################################################################
# June 20, 2012
# 7:59:34 AM
# Author: Mirella Flores
# (c) International Potato Center
#
###############################################################################
if(!require('EBImage', character.only=TRUE)) install.packages('bin/EBImage.zip',repos=NULL)
library(EBImage)

morpho <- function(i){
	
	tryCatch({
# deteccion de objetos
	
	#pth=thresh(i[,,1],15,15,0.05175) 	#compara intensidad de pixeles
	#kern=makeBrush(5, shape=c('disc'), step=FALSE)^2 
	pth=thresh(i[,,1],5,5,0.0175) 	#compara intensidad de pixeles
	kern=makeBrush(5, shape=c('disc'), step=FALSE)^2 
	pf=closing(dilate(pth, kern), kern)  #pf=closing(erode(dilate(pth, kern), kern), kern)
	pfh=fillHull(pf)
	##

#display(pfh)
#conteo de objetos
	
	nobj=bwlabel(pfh)
	
#eliminacion de objetos insignificantes
	
	pp=computeFeatures.shape(nobj)[,'s.area']
	
	id = which(pp<50) #mayores k
	rm=rmObjects(nobj,id)
	# id = which(pp<2000) #mayores k
	# rm=rmObjects(nobj,id)
	
	# pp1=computeFeatures.shape(rm)[,'s.perimeter']
	
	# id1 = which(pp1<50)     		#puntos menores que
	# rm1=rmObjects(rm,id1)
	
	# pp2=computeFeatures.shape(rm1)[,'s.perimeter']
	# id2 = which(pp2>50)
	# rm2=rmObjects(rm1,id2)
	rm2=rm
	#display(rm2)
	
	if(max(rm2)==0){
		res='no se detectaron objetos'
		return(res)
	}else{
		
		#marcado de objetos reconocidos
		colorMode(rm2)=Grayscale
		sto=stackObjects(rm2,i)
		obj= paintObjects(rm2, i, col='red')
		
		#caracterizacion de objetos
		oc=ocontour(rm2)
		#pp=computeFeatures.shape(rm)[,c('m.cx','m.cy','s.area','s.perimeter','m.cx','m.cy')] #puntos de los obj reconocidos
		pp=computeFeatures.moment(rm2)[,c('m.cx','m.cy')]
		pp=cbind(pp, computeFeatures.shape(rm)[,c('s.area','s.perimeter')] , pp)
	#display(obj)
		#font=drawfont(weight=600, size=16)
		
		if(class(pp)=="numeric"){
			ch=pp[1:2]
			ch=round(ch)
			ch1=pp[3:4]
			ch2=pp[4:6] 
			
			#numeracion de objetos
			#ptx=drawtext(obj,xy=ch,labels=as.character(1),font=font,col="red")
			
			#salidas
			names(ch2)=c('s.perimeter','m.cx','m.cy')
			res=c(ch,ch1,ch2)
		
			res=round(res,4)
			n=1
			
			
			#display(ptx)
			
			res=list(res,sto,sto,n)
			
			return(res)
			
		}
		else{
			ch=pp[,1:2]
			ch=round(ch)
			ch1=pp[,3:4]
			ch2=pp[,4:6] 
			
			#numeracion de objetos
			#ptx=drawtext(obj,xy=ch,labels=as.character(1:nrow(ch1)),font=font,col="red")
			
			#salidas
			colnames(ch2)=c('s.perimeter','m.cx','m.cy')
			res=cbind(ch,ch1,ch2)
			n=nrow(ch)
			#cat('number of objects=',n,'\n')
			#cat('features:','\n')
			#display(ptx)
			#print(n)
			res=list(res,sto,sto,n)
			
			return(res)
		}
	}
	})
	
}
imagen.pto.initial <- function(oc,pto2,ch,im,dimens,i,coord){

	tryCatch({
	
		pto 	= (oc[[1]][coord,])
		
		pto.min = min(pto[,i])
		pto.max = max(pto[,i])
		distn 	= (pto.max-pto.min)
		eje 	= pto2[i]-(ch-pto.min)
		
		if(i==1){
			
			dim.init<-as.numeric(image.identify(im,'%w')) }
		
		else if(i==2){
			
			dim.init<-as.numeric(image.identify(im,'%h')) 
		}
		
		pto.real 	= abs(eje*dim.init/dimens)
		distn.real 	= abs(distn*dim.init/dimens)
		
		result 		= list(pto.real, distn.real)
		
		return(result)
		
	}, error = function(err) {return(NULL)})
}

image.coordenada<-function(img,vect){
	#analisis del objeto seleccionado
	tryCatch({
	dist=matrix(0,length(vect),11)
	go=1
	for(k in vect){
		#deteccion
		x=img[[1]][,,,k]
		pth1=thresh(x[,,1],15,15,0.005)
		kern=makeBrush(13,shape='disc')
		pf1=closing(pth1,kern)
		pf1=fillHull(pf1)
		colorMode(pf1)=Grayscale
		ob1=bwlabel(pf1)
		oc=ocontour(pf1)
	
		ch=computeFeatures.moment(pf1)[,c('m.cx','m.cy')]
		
		ch=round(ch)
		
		h1 = which(oc[[1]][,2]==ch[2]) # devuelve la coordenada
		v1 = which(oc[[1]][,1]==ch[1])
		
		result=list(oc,ch,h1,v1)
		
		return(result)
	}
	})
}
image.identify<-function(ima,prop){
	
	tfn = file.path ("bin","ImageMagick","identify")
	
	dim1 <- system2(tfn,args=paste("-format", prop, ima,sep = " "), stdout = TRUE)
	#dim1<-system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\identify.exe",args=paste("-format", prop, ima,sep = " "), stdout = TRUE)
	return(dim1)
}

image.crop <-function(im){
	
	tryCatch({
	
	out <- file.path ("temp","output.jpg")
	out1 <- file.path ("temp","output1.jpg")
	#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-resample 20",im,o, sep = " "), stdout = TRUE)
	
	# resize image < 300 kb
	
	tfn = file.path ("bin","ImageMagick","convert")
	#tryCatch(system2(tfn,args=paste("-crop 0x1500+0+4000",im,out1, sep = " "), stdout = TRUE), error = function(cond)NA ) 
	#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-resize 400000@",im,o, sep = " "), stdout = TRUE)
	
	
	system2(tfn,args=paste("-resize 400000@",im,out, sep = " "), stdout = TRUE)
	#tryCatch(system2(tfn,args=paste("-resize 400000@",im,out, sep = " "), stdout = TRUE), error = function(cond)NA ) 
	if (file.exists(out)){
	i = readImage(out)

	# call to main function for detect objects
	morp = morpho(i)
	#print(morp)
	# for display image: display(morp[[2]])
	
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

			# only for look for labels for size
			#convert -crop WxH {+-}x{+-}y {%} {!} input output

			if (!(is.null(ejex)) && !(is.null(ejey))){

			#	if (ejey[[1]][1] > (tamanoh/2)){
			if (ejey[[2]][1]/ejex[[2]][1] < 1.2) {
				
				# split in 2 parts labels 'corbata'
				
				if (ejex[[2]][1]/ejey[[2]][1] > 5.5) {
							
					outimage <- paste(out,".",k,"2.new.jpg",sep = "")
					outimage1 <- paste(out,".",k,"3.new.jpg",sep = "")

					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.7),"+",ejey[[1]][1],sep=""),im, outimage1, sep = " "), stdout = TRUE)
				}

				else if (ejex[[2]][1]/ejey[[2]][1] > 5) { #verificar tooo era 4 ok
								
					outimage <- paste(out,".",k,"1.new.jpg",sep = "")
					outimage1 <- paste(out,".",k,"2.new.jpg",sep = "")

					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage1, sep = " "), stdout = TRUE)
				
				}
				else if (ejex[[2]][1]/ejey[[2]][1] > 2) { #verificar tooo era 4 ok
			
					outimage <- paste(out,".",k,"1.new.jpg",sep = "")
					outimage1 <- paste(out,".",k,"2.new.jpg",sep = "")		
					
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1],"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",(ejex[[1]][1]+(ejex[[2]][1]*0.45)),"+",ejey[[1]][1],sep=""),im, outimage1, sep = " "), stdout = TRUE)
				
				}
				else {
					outimage <- paste(out,".",k,".new.jpg",sep = "")
					outimage1 <- paste(out,".",k,"1.new.jpg",sep = "")
				
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1],"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]/2,"x",ejey[[2]][1]/2,"+",ejex[[1]][1],"+",(ejey[[1]][1]+ejey[[2]][1]/2),sep=""),im, outimage1, sep = " "), stdout = TRUE)
					#system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.45,"x",ejey[[2]][1]/3*2,"+",ejex[[1]][1],"+",(ejey[[1]][1]+ejey[[2]][1]*0.34),sep=""),im, outimage1, sep = " "), stdout = TRUE)
				}
			}
			} #}
			k=k+1
		}
	}
	}
	if(file.exists(file.path ('temp','output.jpg'))) file.remove(file.path ('temp','output.jpg'))
	
	}, error = function(err) {return(NULL)})
}

add.legend <- function (year,autor,im){
	
	h=as.numeric(image.identify(im,'%h')) 

	w=as.numeric(image.identify(im,'%w')) 
	numberautor <- do.call(rbind, strsplit(autor,"\\,"))

	if ((length(numberautor))>1){
		str_autor = "Authors:"
	}	
	else str_autor = "Author:"
	if(w>h){
	
		point_size = round(as.numeric(w*0.01))
	}
	else{
	
		point_size = round(as.numeric(h*0.01))
	}
	
	point = h*0.96
	tfn = file.path ("bin","ImageMagick","convert")
	txt = paste('" ©Copyright ',year,' International Potato Center.\n ',str_autor,autor,' "',sep='')
	tryCatch(system2(tfn,args=paste("-pointsize ", point_size," -annotate +1+",point," ",txt," ",im," ",im, sep = ""), stdout = TRUE), error = function(cond)NA ) 

}

