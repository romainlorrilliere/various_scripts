# programme netoyage fichier tableau Pigeon (30/05/2006)

# source("slimTab.R")

# slimTab("2006-04-27-12-05-25")
# idPigeon("2006-04-27-12-05-25")

slimTab = function (fileName){
	distance = 10
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))
	dat = dat[order(dat[,3]),]
	vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
	datLight = dat[1,]
	i = 1
	while (i < dim(dat)[1]){
		n = 0
		finding = FALSE
		while ((finding == FALSE) & (i+n < dim(dat)[1])) {
			if (dat[i+n,3] == dat[i+n+1,3]) {finding = vectTemp[i+n] + distance < vectTemp[i+n+1]}
			else { finding = TRUE }
			n = n + 1
		}
		i = i + n
		datLight = rbind(datLight, dat[i,])
	}
	if ((dat[i,3] == dat[i-1,3]) & (vectTemp[i-1] + distance >= vectTemp[i])) {datLight = datLight[1:dim(datLight)[1]-1,]}
	datLight = datLight[order(datLight[,1]),]
	write.table(as.data.frame(datLight),paste(fileName,"Light-",distance,".csv",sep=''),sep=';')	
}


	
etudEcart = function(fileName){
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))
	dat = dat[order(dat[,3]),]
	vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
	dif = as.numeric(vectTemp[2]) - as.numeric(vectTemp[1])
	ecarts = c((dif[1] = dif[1] + (dif[2] = dif[2] + (dif[3] = dif[3] +  dif[4]*24) * 60) *60))
	i = 2
	while (i < dim(dat)[1]) {
		if (dat[i,3] == dat[i+1,3]) {
			dif = as.numeric(vectTemp[i+1]) - as.numeric(vectTemp[i])
			ecarts = c(ecarts, (dif[1] = dif[1] + (dif[2] = dif[2] + (dif[3] = dif[3] +  dif[4]*24) * 60) *60))}
		else { finding = TRUE }
		i = i + 1
	}
	print(summary(ecarts))
	X11()
	m = matrix(1:2,2,1)
	layout(m)
	layout.show(2)
	#boxplot(ecarts,horizontal=TRUE)
	#boxplot(ecarts[ecarts<120], horizontal = TRUE)
	#hist(ecarts,nclass=max(ecarts)*600)
	hist(ecarts[ecarts<600],nclass=600)
	hist(ecarts[ecarts<240],nclass=240)
	#hist(ecarts[ecarts<20],nclass=20)
	
}

ecart = function(dat){
	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))
	dat = dat[order(dat[,3]),]
	vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
	dif = 0
	ecarts = 0
	i = 1
	while (i < dim(dat)[1]) {
		if (dat[i,3] == dat[i+1,3]) {
			dif = as.numeric(vectTemp[i+1]) - as.numeric(vectTemp[i])
			ecarts = c(ecarts, (dif[1] = dif[1] + (dif[2] = dif[2] + (dif[3] = dif[3] +  dif[4]*24) * 60) *60))}
		else { finding = TRUE }
		i = i + 1
	}
	print(summary(ecarts))
	X11()
	m = matrix(1:2,2,1)
	layout(m)
	layout.show(2)
	#boxplot(ecarts,horizontal=TRUE)
	#boxplot(ecarts[ecarts<120], horizontal = TRUE)
	#hist(ecarts,nclass=max(ecarts)*600)
	hist(ecarts[ecarts<600],nclass=600)
	hist(ecarts[ecarts<240],nclass=240)
	#hist(ecarts[ecarts<20],nclass=20)
	
}
moving = function(dat){
	dataTab = data.frame(time= strptime(as.character(dat[,1]), "%d/%m/%Y %T"),gate= dat[,2],pigeonId= as.factor(dat[,3]))
	dataTab = dataTab[order(dataTab[,3]),]
	dataTab = cbind(dataTab,character(dim(dataTab)[1]))
	colnames(dataTab,c("time","gate","pigeonId","detect"))
	
}
	


idPigeon = function(fileName){
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))
	print(length(levels(as.factor(dat[,3]))))
	levels(as.factor(dat[,3]))
}



detectBird = function (char){
	if (char == "entry") {return("exit")}
	else {return("entry")}
}




dimTab = function (fileName) {
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')	
	print(dim(dat))
}


ecartByGate = function (fileName){
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
	dat1 = as.data.frame(dat[,1][dat[,2] == 1] )
	dat1 = cbind (as.data.frame(dat[,1][dat[,2] == 1] ),dat[,2][dat[,2] == 1],dat[,3][dat[,2] == 1])
	dat2 = cbind (as.data.frame(dat[,1][dat[,2] == 2] ),dat[,2][dat[,2] == 2],dat[,3][dat[,2] == 2])
	dat3 = cbind (as.data.frame(dat[,1][dat[,2] == 3] ),dat[,2][dat[,2] == 3],dat[,3][dat[,2] == 3])
	dat4 = cbind (as.data.frame(dat[,1][dat[,2] == 4] ),dat[,2][dat[,2] == 4],dat[,3][dat[,2] == 4])
	print(dim(dat1))
	ecart(dat1)
	print(dim(dat2))
	ecart(dat2)
	print(dim(dat3))
	ecart(dat3)
	print(dim(dat4))
	ecart(dat4)
}





slim = function (dat){
	distance = 10
	dat = dat[order(dat[,3]),]
	vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
	datLight = dat[1,]
	i = 1
	while (i < dim(dat)[1]){
		n = 0
		finding = FALSE
		while ((finding == FALSE) & (i+n < dim(dat)[1])) {
			if (dat[i+n,3] == dat[i+n+1,3]) {finding = vectTemp[i+n] + distance < vectTemp[i+n+1]}
			else { finding = TRUE }
			n = n + 1
		}
		i = i + n
		datLight = rbind(datLight, dat[i,])
	}
	if ((dat[i,3] == dat[i-1,3]) & (vectTemp[i-1] + distance >= vectTemp[i])) {datLight = datLight[1:dim(datLight)[1]-1,]}
	datLight = datLight[order(datLight[,1]),]
	write.table(as.data.frame(datLight),paste("WEEK","Light-",distance,".csv",sep=''),sep=';')
	return(dat)
}



week = function () {

	fileName = paste("2006-06-09",sep = '')
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
	fileName = paste("2006-06-10",sep = '')
	datDay = read.table(paste(fileName,".csv",sep=''),sep=';')
	dat = rbind(dat,datDay)
#	for (i in (10 : 14)) {
#		fileName = paste("2006-06-",i,sep = '')
#		datDay = read.table(paste(fileName,".csv",sep=''),sep=';')
#		dat = rbind(dat,datDay)
#	}
#	print(dim(dat))
	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))
	
	print("ecart")
	ecart(dat)
	
	dat = slim(dat)
	
	print(length(levels(as.factor(dat[,3]))))
	levels(as.factor(dat[,3]))	
	
	dim(dat)
	ecart(dat)
	return(dat)
}
	

#dat = read.table(paste("WEEKLight-10.csv",sep=''),sep=';')

night = function (dat) {
	ids = levels(as.factor(dat[,3]))
	id = ids[1]
	datid = cbind (as.data.frame(dat[,1][dat[,3] == id] ),dat[,2][dat[,3] == id],dat[,3][dat[,3] == id])
	vectTempId = strptime(as.character(datid[,1]), "%d/%m/%Y %T")
	i = 1
	while ((as.numeric(vectTempId[i])[4] == as.numeric(vectTempId[i+1])[4])) {i = i + 1}
	night = vectTempId[i+1] - vectTempId[i]
	lifid = as.data.frame(id)
	lifid = cbind(lifid,dim(datid)[1],as.numeric(night),vectTempId[i],vectTempId[i+1])
	lif = as.data.frame(lifid)
	for (j in (2:length(ids))){
		id = ids[j]
		datid = cbind (as.data.frame(dat[,1][dat[,3] == id] ),dat[,2][dat[,3] == id],dat[,3][dat[,3] == id])
		vectTempId = strptime(as.character(datid[,1]), "%d/%m/%Y %T")
		i = 1
		while (((as.numeric(vectTempId[i])[4] == as.numeric(vectTempId[i+1])[4])) & ((i + 1) < length(as.character(vectTempId)))) {i = i + 1}
		night = vectTempId[i+1] - vectTempId[i]
		lifid = as.data.frame(id)
		lifid = cbind(lifid,dim(datid)[1],as.numeric(night),vectTempId[i],vectTempId[i+1])
		if (dim(datid)[1]>0){lif = rbind(lif,lifid)}
	}	
	
	print(lif)
	write.table(as.data.frame(lif),"night2.csv",sep=';', dec = ',')
}

slim = function (tab){
	distance = 10
	dat = dat[order(dat[,3]),]
	vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
	datLight = dat[1,]
	i = 1
	while (i < dim(dat)[1]){
		n = 0
		finding = FALSE
		while ((finding == FALSE) & (i+n < dim(dat)[1])) {
			if (dat[i+n,3] == dat[i+n+1,3]) {finding = vectTemp[i+n] + distance < vectTemp[i+n+1]}
			else { finding = TRUE }
			n = n + 1
		}
		i = i + n
		datLight = rbind(datLight, dat[i,])
		print(i)
	}
	if ((dat[i,3] == dat[i-1,3]) & (vectTemp[i-1] + distance >= vectTemp[i])) {datLight = datLight[1:dim(datLight)[1]-1,]}
	datLight = datLight[order(datLight[,1]),]
	write.table(as.data.frame(datLight),paste("night","2",".csv",sep=''),sep=';')	
	return(datLight)
}

twoDays = function () {

	fileName = paste("2006-06-09",sep = '')
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
	fileName = paste("2006-06-10",sep = '')
	datDay = read.table(paste(fileName,".csv",sep=''),sep=';')
	dat = rbind(dat,datDay)
	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))


	dat = slim(dat)
	
	print(length(levels(as.factor(dat[,3]))))
	levels(as.factor(dat[,3]))	
	
	dim(dat)
	ecart(dat)
	return(dat)
}


nights = function (){
	for (f in (10 : 13)) {
		fileName = paste("2006-06-",f,".csv",sep = '')
		print(fileName)
		print(f)
		dat = read.table(fileName,sep=';')
		fileName = paste("2006-06-",f+1,sep = '')
		datDay = read.table(paste(fileName,".csv",sep=''),sep=';')
		dat = rbind(dat,datDay)
		dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))

		dim(dat)
		
		distance = 10
		dat = dat[order(dat[,3]),]
		vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
		datLight = dat[1,]
		i = 1
		while (i < dim(dat)[1]){
			n = 0
			finding = FALSE
			while ((finding == FALSE) & (i+n < dim(dat)[1])) {
				if (dat[i+n,3] == dat[i+n+1,3]) {finding = vectTemp[i+n] + distance < vectTemp[i+n+1]}
				else { finding = TRUE }
				n = n + 1
			}
			i = i + n
			datLight = rbind(datLight, dat[i,])
		}
		if ((dat[i,3] == dat[i-1,3]) & (vectTemp[i-1] + distance >= vectTemp[i])) {datLight = datLight[1:dim(datLight)[1]-1,]}
		datLight = datLight[order(datLight[,1]),]
		fileName = paste("twoDays",f,".csv",sep='')
		print(fileName)
		write.table(datLight,fileName,sep=';')

		dat = datLight
		
		ids = levels(as.factor(dat[,3]))
		id = ids[1]
		datid = cbind (as.data.frame(dat[,1][dat[,3] == id] ),dat[,2][dat[,3] == id],dat[,3][dat[,3] == id])
		vectTempId = strptime(as.character(datid[,1]), "%d/%m/%Y %T")
		i = 1
		while ((as.numeric(vectTempId[i])[4] == as.numeric(vectTempId[i+1])[4])) {i = i + 1}
		night = vectTempId[i+1] - vectTempId[i]
		lifid = as.data.frame(id)
		lifid = cbind(lifid,dim(datid)[1],as.numeric(night),vectTempId[i],vectTempId[i+1])
		lif = as.data.frame(lifid)
		for (j in (2:length(ids))){
			id = ids[j]
			datid = cbind (as.data.frame(dat[,1][dat[,3] == id] ),dat[,2][dat[,3] == id],dat[,3][dat[,3] == id])
			vectTempId = strptime(as.character(datid[,1]), "%d/%m/%Y %T")
			i = 1
			while (((as.numeric(vectTempId[i])[4] == as.numeric(vectTempId[i+1])[4])) & ((i + 1) < length(as.character(vectTempId)))) {i = i + 1}
			night = vectTempId[i+1] - vectTempId[i]
			lifid = as.data.frame(id)
			lifid = cbind(lifid,dim(datid)[1],as.numeric(night),vectTempId[i],vectTempId[i+1])
			if (dim(datid)[1]>0){lif = rbind(lif,lifid)}
		}	
		
		print(lif)
		fileName = paste("night",f,".csv",sep='')
		print(fileName)
		write.table(lif,fileName,sep=';', dec = ',')
			
	}

}






# & (dat[i+n,2] == dat[i+n+1,2]))



#dim(data)

#
#dim(data2dif)
#dim(data10dif)
#dim(data60dif)
#dim(data2)
#dim(data10)
#dim(data60)
#
#vectTemp
#vectTemp[1]
#vectTemp[1] + 2
#vectTemp[1] + 60
#vectTemp[1] + 3600
#vectTemp[1] + 86400
#
#
#slimTab("2006-04-27-12-05-25")
#

#
#fileName = "2006-04-27-12-05-25Light-5"
#	dat = read.table(paste(fileName,".csv",sep=''),sep=';')
#	dat[,3] = as.numeric(paste("0x",as.character(dat[,3]),sep=''))
#	dat = dat[order(dat[,3]),]
#	vectTemp = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
#	dif = as.numeric(vectTemp[2]) - as.numeric(vectTemp[1])
#	ecarts = c((dif[1] = dif[1] + (dif[2] = dif[2] + (dif[3] = dif[3] +  dif[4]*24) * 60) *60))
#	i = 2
#	while (i < dim(dat)[1]) {
#		if (dat[i,3] == dat[i+1,3]) {
#			dif = as.numeric(vectTemp[i+1]) - as.numeric(vectTemp[i])
#			ecarts = c(ecarts, (dif[1] = dif[1] + (dif[2] = dif[2] + (dif[3] = dif[3] +  dif[4]*24) * 60) *60))
#		}
#		else { finding = TRUE }
#		i = i + 1
#	}
#	print(summary(ecarts))
#	X11()
##	m = matrix(c(1,2),2,1)
#	layout(m)
#	layout.show(2)
#	boxplot(ecarts,horizontal=TRUE)
#	hist(ecarts,nclass=max(ecart)*12)






