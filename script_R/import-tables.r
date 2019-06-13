# programme de prépartation à l'importation dans la base de donnée

#				source("import-tables.r")

#				importFile(2006,"06",nbJour=14,jourDebut=8)
#				month(2007,"06",nbJour=14,jourDebut=6)



# fonction de collage des tabeaux d'un même mois
month = function (year,mois,nbJour,jourDebut=1) {    # year est l'année en 4 chiffre et le moiis en 2
	cat("\n ---- Fonction month ----\n")
	if (jourDebut < 10) { jour = paste("0", jourDebut, sep='')}
		else{ jour = jourDebut}

	fileName = paste("data/",year,"-",mois,"-",jour,sep = '')
	dat = read.table(paste(fileName,".csv",sep=''),sep=';')

#	browser()

	for (i in ((jourDebut+1):nbJour)) {
		if (i < 10) { jour = paste("0", i, sep='')}
		else{ jour = i}
		cat(i, " | ")
		flush.console()
		fileName = paste("data/",year,"-",mois,"-",jour,sep = '')
		datDay = read.table(paste(fileName,".csv",sep=''),sep=';')
		dat = rbind(dat,datDay)
	}
	

	dat = cbind(dat,as.numeric(paste("0x",as.character(dat[,3]),sep='')))
	
	vectDateHeure = strptime(as.character(dat[,1]), "%d/%m/%Y %T")
	vectDate = format(vectDateHeure,"%d/%m/%Y")
	vectHeure = format(vectDateHeure,"%H:%M:%S")

	dat[,1]= vectHeure
	dat = cbind(as.data.frame(vectDate),dat)

	colnames(dat) = c("date","heure","porte","ind_16","ind_10")

	cat("\n")

	return(dat)
}




# netoyage des redondances de capture

slim = function (dat){

	cat("\n----  Fonction SLIM  ---- \n")

	distance = 3600
	dat = dat[order(dat[,3]),]
	vectTemp = strptime(as.character(dat[,2]), "%T")
	datLight = dat[1,]
	i = 1
	while (i < dim(dat)[1]){
		
		n = 0
		finding = FALSE
		while ((finding == FALSE) & (i+n < dim(dat)[1])) {
			if ((n+i) %% 100 == 0) { cat(((n+i)/100)," | ")}
			if ((n+i) %% 1000 == 0) { cat("\n")}			# AFFICHAGE DE LA PROGRESSION DU PROGRAMME PAR DEPARTEMENT
			flush.console()
			if (dat[i+n,3] == dat[i+n+1,3]) {finding = vectTemp[i+n] + distance < vectTemp[i+n+1]}
			else { finding = TRUE }
			n = n + 1
		}
		i = i + n
		datLight = rbind(datLight, dat[i,])
	}
	if ((dat[i,3] == dat[i-1,3]) & (vectTemp[i-1] + distance >= vectTemp[i])) {datLight = datLight[1:dim(datLight)[1]-1,]}
	datLight = datLight[order(datLight[,1]),]
	return(dat)
}




#la nouvelle fonction utilise plus les capaciter de traitement matricielle 
# de R et va d'après un premier test au moins 10 fois plus vite.

newSlim = function(tab)
{
	cat("\n ---- Nouvelle fonction SLIM ----\n")
	#browser()
	# 1. je cherche les valeurs unique du doublon individus_heure
	
	vectTempHeure = strptime(as.character(tab[,2]), "%T")
	vectTempDate = strptime(as.character(tab[,1]), "%d/%m/%Y")
	vectKey = paste(tab[,5],as.character(vectTempDate),format(vectTemp,"%H"),sep='_')
	vectUnique = unique(vectKey)
	cat(":: Nombre de lignes du tableau traité : ",length(vectUnique),"\n\n")

	i=1
	uniqueKey = c(min(which(vectKey==vectUnique[i])))

	# 2. pour chacune de ces valeur je cherche la première occurance dans le vecteur des doublons
	for (i in 2:length(vectUnique))
	{
	# AFFICHAGE DE LA PROGRESSION DU PROGRAMME PAR DEPARTEMENT
		if (i %% 100 == 0) {cat("\n")}
		if (i %% 10 == 0) {cat(i," | ") }
				
		flush.console()
		uniqueKey = c(uniqueKey,min(which(vectKey==vectUnique[i])))
	}
	cat("\n")
	# 3. je retourne notre tableau avec seulement les indices qui nous intéresse
	return(tab[uniqueKey,])

}




ecriture_base = function (tab,nameFile)
{
	write.table(tab,paste(nameFile,".csv",sep=''),row.names = FALSE ,sep=";")
	cat("\n     <-- ",nameFile,".csv\n")
}





#--------------------
# fonction global de calul des fichiers par mois nettoyer et non 

importFile = function(year,mois,nbJour,jourDebut=1)
{
	baseMonth = month(year,mois,nbJour,jourDebut)
	fileNameMonth = paste("data/dataMonth/",year,"-",mois,sep='')
	ecriture_base(baseMonth,fileNameMonth)
#browser()
	fileMonthSlim=newSlim(baseMonth)
	fileNameMonthSlim = paste("data/dataSlim/",year,"-",mois,"-SLIM",sep='')
	ecriture_base(fileMonthSlim,fileNameMonthSlim)	
}	


