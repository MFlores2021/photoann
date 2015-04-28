
add.metadata <- function (file,img,mdata,nplot,keys,catalog) {
# change copyright
		
	ep = file.path("bin","exiv2","exiv2")
	sp1 = ' -M "set '
	sp = '" -M "set '

	if(is.na(mdata[2]) || str_trim(mdata[2]) == '') mdata[2]=0.0001
	if(is.na(mdata[3]) || str_trim(mdata[3]) == '') mdata[3]=0.0001

	gps_lat = to_deg(mdata[2],c("S", "N")) 
	gps_lng = to_deg(mdata[3],c("W", "E")) 

	#mdata[7] = str_replace(mdata[6],"Please-check-","")
	#mdata[8] = str_replace(mdata[7],"Please-check-","")
	
	autorxp		= paste("Exif.Image.XPAuthor Ascii",mdata[1])
	titl		= paste('Exif.Image.XPTitle Ascii',mdata[7])
	key 		= paste("Exif.Image.XPKeywords Ascii",keys)
	comm		= paste("Exif.Image.XPComment Ascii ", 'image')
	latitude	= paste("Exif.GPSInfo.GPSLatitude ",gps_lat[1],'/1 ',gps_lat[2],'/1 ',gps_lat[3],sep='')
	latitude_ref= paste("Exif.GPSInfo.GPSLatitudeRef ",gps_lat[4])
	longitude	= paste('Exif.GPSInfo.GPSLongitude ',gps_lng[1],'/1 ',gps_lng[2],'/1 ',gps_lng[3],sep='')
	longitude_ref= paste('Exif.GPSInfo.GPSLongitudeRef',gps_lng[4])
	copyright	= paste('Exif.Image.Copyright Ascii',mdata[5])
	country		= paste('Xmp.dc.Country',mdata[4])
	plots		= paste("Xmp.dc.plot",nplot)
	exp			= paste('Xmp.dc.Experiment',mdata[6])

	autors		= paste('Xmp.dc.Autor',mdata[1])
	titles		= paste('Xmp.dc.title lang=en-US',mdata[7])
	gps_lat		= paste('Xmp.dc.gpsLat ',mdata[2])
	gps_lng		= paste('Xmp.dc.gpsLng ',mdata[3])
	catalogue	= paste('Xmp.dc.catalogue ',catalog)
	
	cmd = paste(ep,sp1,autorxp,sp,titl,sp,key,sp,comm,sp,latitude,sp,latitude_ref,sp,longitude,sp,longitude_ref,sp,copyright,sp,titles,sp,country,sp,plots,sp,autors,sp,gps_lat,sp,gps_lng,sp,catalogue,sp,exp,'" ',file,sep='')
	print(cmd)
	try(system(cmd,ignore.stdout = FALSE), silent = TRUE)
	
}

read.xls <- function (xls,t_sheet,cipnumber){
	
	wb  	<- loadWorkbook(xls)
	sheets	<- getSheets(wb)
	
	sheet 	<- sheets[[t_sheet]]
	if (t_sheet=='Minimal'){
		autor 	= get.row.value("Leader",sheet,1,2)	
		country = get.row.value("Country",sheet,1,2)
		copyright = get.row.value("License",sheet,1,2)	
		lat 	= get.row.value("Latitude",sheet,1,2)
		long 	= get.row.value("Longitude",sheet,1,2)
		exp 	= get.row.value("Short name or Title",sheet,1,2)
		res		= c(autor,lat,long,country,copyright,exp)
	}
	else if (t_sheet=='Fieldbook'){
		nplot 	= get.row.value(cipnumber,sheet,3,1)
		res 	= nplot
	}
	else res = NA
	return(res)
}

get.row.value <- function(aname,sheet,col,valuecol){
	n = sheet$getLastRowNum()+1
	m = 10
	
	v = readColumns(sheet,1,m,1,n)
	df = as.data.frame(v[,col],stringsAsFa=F)
	res = which(str_detect(df[,1],aname))[1]
	res = toString(v[res,valuecol])
	return(res)
}

to_deg <- function(value,loc){
	value=as.numeric(value)
	if (value < 0)	loc_value = loc[1]
	else if (value > 0)	loc_value =loc[2]
	else	loc_value = ""
	
	abs_value = abs(value)
	deg =  trunc(abs_value)
	t1 = (abs_value-deg)*60
	min = trunc(t1)
	seg = (t1 - min)* 60
	sec =toString(as.fractions(seg))
	res = c(deg, min, sec, loc_value)
	return (res)
}   

# Select Image for Catalogue adding as metadata xmp.dc.Catalogue = T
# select.image.catalogue(w)

select.image.catalogue <- function (w){
	out   <- "cancel"
	
	tryCatch({
				
		res = select.image.catalogue.Dlg(w)
		if(length(res)>1) change.metadata(res,'Xmp.dc.catalogue XmpText','Y')
		else if (res!=out) change.metadata(res,'Xmp.dc.catalogue XmpText','Y')
		
	}, error=function(cond) NA )
}
change.metadata <- function (img,atr,value) {
	
	ep = file.path("bin","exiv2","exiv2")
	sp1 = ' -M "set '
	
	img = stringr::str_replace_all(img,"\n"," ")
	img = paste(img, collapse = " ")
	metadata	= paste(atr,' ',value,sep='')
	
	cmd = paste(ep,sp1,metadata,'" ',img,sep='')
	
	try(system(cmd,ignore.stdout = TRUE), silent = TRUE)
	
}
select.image.catalogue.Dlg = function(w){
	
	dput("cancel",file="bin/temp.txt")
	
	win <- gbasicdialog(title='Select image for catalogue', parent=w)
	
	gl = glayout(cont=win)
	gl[1,1]=gcombobox(get.fb.list(), cont=gl)
	gl[2,1]=gcombobox(get.org.list(), cont=gl)
	
	gl[3,1]=gbutton("Select ...", container=gl,
			handler = function(h,...) {
				Filter = Filters[c("png","jpeg","bmp","All"),]
				
				files.image=choose.files(default = get.path.image(svalue(gl[1,1]),svalue(gl[2,1])), caption = "Select files for catalogue",multi = TRUE,filters = Filter)

				if(length(files.image) && !is.na(svalue(gl[1,1]))){
					nro = paste("Selected: ",length(files.image)," files")
					gl[4,1]=glabel(nro, cont=gl)
					gl[5,1]=glabel("                                    ", cont=gl)
					dput(files.image,file="bin/temp.txt")
				}
				else {
					gl[4,1]=glabel("                                    ", cont=gl)
					gl[5,1]=glabel("Choose images ", cont=gl)
					dput("cancel",file="bin/temp.txt")
				}
			})
	
	visible(win, set=TRUE) ## show dialog
	fn=dget("bin/temp.txt")
	dput("cancel",file="bin/temp.txt")
	return(fn)
}

# change metadata from image: metadata.by.image.Dlg(w)

metadata.by.image.Dlg = function(w){
	
	wDlg <- xgwindow("Change metadata from image",visible=F, parent=w, width=400, height=300)
	
	g  <- ggroup(horizontal = FALSE, cont = wDlg)
	wg <- ggroup(cont=g)
	gl <- glayout(cont=wg)
	bg <- ggroup(cont=g)
	addSpring(wg)
	addSpring(bg)
	tmp	  <- file.path('bin','temp.txt')
	
	gl[1,1] = gbutton("Select file ", cont = gl,handler = function(h,...) {
			
			book	=	get.fb.list()
			if (length(book)>1) book = ''
			Filter	=	Filters[c("png","jpeg","bmp","All"),]
			
			out		=	choose.files(default = get.path.image(book,''), caption = "Change metadata from ... ",multi = FALSE,filters = Filter)
			dput(out,file=tmp)
			visible(wDlg)<-FALSE
			dispose(wDlg)
			metadata.by.image.Dlg(w)
			
			})
	
	file  <- dget(tmp)
	img	  <- image.name(file)
	out   <- "cancel"
	
	if(file!=out)	gl[2,1]= paste("Selected: ",img)
	
	layout = layout.for.metadata(file)

	if(!is.null(layout)) wdg = gformlayout(layout, container=wg)
	
	gbutton("cancel", handler =  function(h,...) {
				
				dput(out,file=tmp)
				dispose(wDlg)
			},
			, container=bg)
	gbutton("ok", handler =  function(h,...) {
				meta <- svalue(wdg)
				
				autor	= meta$autor
				lat		= meta$latitude
				long	= meta$longitude
				country	= meta$country
				copyright = meta$copyright
				keys	= meta$key
				nplot	= meta$nplot
				titl	= meta$titl
				comm 	= meta$comm
				catalogue=meta$catalogue
				exp		=meta$exp
				mdata	= c(autor,lat,long,country,copyright,exp,titl,comm)
				
				add.metadata(file,img,mdata,nplot,keys,catalogue)
					
			titl=paste(str_replace(file,basename(file),""),titl,'.',image.typefile(file),sep='')
			file.rename(file,titl)
			
				dput(out,file=tmp)
				dispose(wDlg)
			},
			, container=bg)
	
	visible(wDlg)=T
}


layout.for.metadata = function(img){
	
	if (file.exists(img)){
	img.metadata <- read.metadata(img)
	
	layout <- list(type = "ggroup",
			horizontal = FALSE,
			children = list(
					list(type="fieldset",
							columns = 2,
							label = "Image",
							children = list(
									list(name = "autor",
											label = "Autor",
											type = "gedit",
											text = img.metadata[1]),
									list(name = "titl",
											label = "Title",
											type = "gedit",
											text = img.metadata[10]),
									list(name = "key",
											label = "Key",
											type = "gedit",
											text = img.metadata[6]),
									list(name = "copyright",
											label = "Copyright",
											type = "gedit",
											text = img.metadata[5]),
									list(name = "comm",
											label = "Comments",
											type = "gedit",
											text = img.metadata[7]),
									list(name = "exp",
											label = "Experiment",
											type = "gedit",
											text = img.metadata[11])
							)),
					list(type="fieldset",
							columns = 2,
							label = "Origin",
							children = list(
									list(name = "country",
											label = "Country",
											type = "gedit",
											text = img.metadata[4]),
									list(name = "nplot",
											label = "Plots",
											type = "gedit",
											text = img.metadata[8]),
									list(name = "latitude",
											label = "Latitude",
											type = "gedit",
											text = img.metadata[2]),
									list(name = "longitude",
											label = "Longitude",
											type = "gedit",
											text = img.metadata[3])									
							)),
					list(type="fieldset",
							columns = 2,
							label = "Catalogue",
							children = list(
									list(name = "catalogue",
											label = "Catalogue\n   (Y/N)",
											type = "gedit",
											text = img.metadata[9])
							)
					)
			)
	)
	layout
	} 
}

read.metadata <- function (img) {

	 autor = blank.str(read.one.metadata("Exif.Image.XPAuthor",img))
	 titl		= blank.str(read.one.metadata('Exif.Image.XPTitle',img))
	 key 		= blank.str(read.one.metadata("Exif.Image.XPKeywords",img))
	 comm		= blank.str(read.one.metadata("Exif.Image.XPComment",img))
	 #latitude	= read.one.metadata("Exif.GPSInfo.GPSLatitude",img)
	 #latitude_ref= read.one.metadata("Exif.GPSInfo.GPSLatitudeRef",img)
	 #longitude	= read.one.metadata("Exif.GPSInfo.GPSLongitude",img)
	 #longitude_ref= read.one.metadata("Exif.GPSInfo.GPSLongitudeRef",img)
	 copyright	= blank.str(read.one.metadata("Exif.Image.Copyright",img))
	 country	= blank.str(read.one.metadata("Xmp.dc.Country",img))
	 plots		= blank.str(read.one.metadata("Xmp.dc.plot",img))
	 gps_lat	= blank.str(read.one.metadata('Xmp.dc.gpsLat',img))
	 gps_lng	= blank.str(read.one.metadata('Xmp.dc.gpsLng',img))
	 catalogue	= blank.str(read.one.metadata('Xmp.dc.catalogue',img))
	 exp		= blank.str(read.one.metadata('Xmp.dc.Experiment',img))
	 
	 result = c(autor,gps_lat,gps_lng,country,copyright,key,comm,plots,catalogue,titl,exp)
	
	 return(result)
}

blank.str = function(str){
	if(!length(str)) str = '<NA>'
	str
}

get.path.image = function(book,ppl){
	
	if (length(book) && book!='' ){
		year_m = "20[0-9]{4}" 
		dir_date <- str_extract(book,year_m)
		if(ppl!=''){
			  folder = file.path(getwd(),'data',getCurrentCrop(),dir_date,book,part.plant(ppl),'.')
		}else folder = file.path(getwd(),'data',getCurrentCrop(),dir_date,book,'.')
	
	}else folder = file.path(getwd(),'data',getCurrentCrop(),'.')	
	
	if(!file.exists(folder)) folder = file.path(getwd(),'data',getCurrentCrop(),'.')
	
	if(!file.exists(folder)) folder = file.path(getwd(),'.')
	
	return(folder)
}

read.one.metadata <- function (metadata,img){
	
	ep = file.path("bin","exiv2","exiv2")
		
	cmd = paste(' -g ',metadata,' -Pv ',img,sep='')
	
	try(mdata <- system2(ep, args=cmd, stdout = TRUE, stderr = FALSE, invisible = TRUE), silent = TRUE)
	
	return(mdata)
}
### Change all title metadata by image name

change.metadata.title <- function (w){
	out   <- "cancel"
	tryCatch({
				res = change.metadata.title.Dlg(w)
				if(length(res)>1) {
					for(i in 1:length(res)) {
					change.metadata(res[i],'Xmp.dc.title XmpText',str_replace(basename(res[i]),".png",""))
					change.metadata(res[i],'Exif.Image.XPTitle Ascii',str_replace(basename(res[i]),".png",""))
				}
			}
				else if (res!=out) {
					change.metadata(res,'Xmp.dc.title XmpText',str_replace(basename(res),".png",""))
					change.metadata(res,'Exif.Image.XPTitle Ascii',str_replace(basename(res),".png",""))
			}
			}, error=function(cond) NA )
}

change.metadata.title.Dlg = function(w){
	
	dput("cancel",file="bin/temp.txt")
	
	win <- gbasicdialog(title='Change image metadata title', parent=w)
	
	gl = glayout(cont=win)
	gl[1,1]=gcombobox(get.fb.list(), cont=gl)
	gl[2,1]=gcombobox(get.org.list(), cont=gl)
	
	gl[3,1]=gbutton("Select ...", container=gl,
			handler = function(h,...) {
				Filter = Filters[c("png","All"),]
				
				files.image=choose.files(default = get.path.image(svalue(gl[1,1]),svalue(gl[2,1])), caption = "Select files",multi = TRUE,filters = Filter)
				
				if(length(files.image) && !is.na(svalue(gl[1,1]))){
					nro = paste("Selected: ",length(files.image)," files")
					gl[4,1]=glabel(nro, cont=gl)
					gl[5,1]=glabel("                                    ", cont=gl)
					dput(files.image,file="bin/temp.txt")
				}
				else {
					gl[4,1]=glabel("                                    ", cont=gl)
					gl[5,1]=glabel("Choose images ", cont=gl)
					dput("cancel",file="bin/temp.txt")
				}
			})
	
	visible(win, set=TRUE) ## show dialog
	fn=dget("bin/temp.txt")
	dput("cancel",file="bin/temp.txt")
	return(fn)
}
