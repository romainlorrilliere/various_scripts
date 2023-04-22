
	#------------------------

	# Boucle la fonction "makeVectorHS(,)" pour le calcul sur l'ensemble des simulations d'un step la concaténation de tout les vecReturn

makeVectHsByStep = function(nbStep,nbSimul,nbBatch,vecHS)
{
	vecReturn = c() # initialisation en vecteur vide
	for(b in 1:nbBatch)
	{
  	cat(" " ,b)
		if (i%%25==0) cat("\n")
		flush.console()

    for(i in 1:nbSimul)
  	{	
  		cat(" " ,i)
  		if (i%%25==0) cat("\n")
  		flush.console()
  		vecNbCol = readNbCol(nbStep,i,b)
  		vecReturn = c(vecReturn,makeVectorHS(vecNbCol,vecHS))
  	}
	}
	return(vecReturn)
}

	#------------------------

	# Calcul de la moyenne et de l'interval de confience CI 95%


calculMeanCI = function(vec)
{
	moyenne = mean(vec)
	CI95min = quantile(vec,0.025)
	CI95max = quantile(vec,0.975)
	mediane = median(vec)
	if (length(vec) > 1)
	{
		ecart_type = sd(vec)
		ecart_type_min = moyenne - (ecart_type / 2)
		ecart_type_max = moyenne + (ecart_type / 2)
	}
	else
	{
		cat("!!!!!! ATTENTION CALCUL DE ECART TYPE AVEC 1 VALEUR !!!!!! \n")
		ecart_type = moyenne
		ecart_type_min = moyenne
		ecart_type_max = moyenne
	}
	dataReturn = data.frame(moyenne,CI95min,CI95max,mediane,ecart_type,ecart_type_min,ecart_type_max,row.names=NULL)
	cat("\n")
	print(dataReturn)
	return(dataReturn)
}


	#------------------------

	# Affichage graphique de la moyenne et du CI95 en pointillé en variable le tableau de moyenne et les CI

plotMeanCI = function(tab,ecriture=TRUE)
{	
	x = row.names(tab)


	
	if (ecriture == TRUE)
	{
#		fileNameWrite = "F:/Héron/Publi/figures/HS_meanCI81.png" # avec l'IC95
#		cat("\n \n  <-- ",fileNameWrite,"\n")
#		png(file=fileNameWrite)
#		matplot(x, tab[1:3], type='l',
#			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#			xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#			main="HS weighted mean",font.main = 2, cex.main= 2)
#		dev.off()
#
#		fileNameWrite = "F:/Héron/Publi/figures/HS_meanSD81.png" # avec l'IC95
#		cat("  <-- ",fileNameWrite,"\n")
#		png(file=fileNameWrite)
#		matplot(x, tab[,c(1,6,7)], type='l',
#			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#			xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#			main="HS weighted mean",font.main = 2, cex.main= 2)
#		dev.off()
#
		fileNameWrite2 = "D:/Recherche/Héron/testSensibilit/HS_mean81.png" 
		cat("  <-- ",fileNameWrite2,"\n\n")
		png(file=fileNameWrite2)
		plot(x, tab$moyenne, type='l',lwd = 3,
			col="black", xlab= "Year", ylab= "Habitat suitability",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 2)
		dev.off()

	}
	
#	x11() # avec l'IC95
#	matplot(x, tab[,1:3], type='l',
#		col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#		xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#		main="HS weighted mean",font.main = 2, cex.main= 2)
#	
#	x11() # avec le SD
#	matplot(x, tab[,c(1,6,7)], type='l',
#		col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#		xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#		main="HS weighted mean",font.main = 2, cex.main= 2)
#	
	x11()
	plot(x, tab$moyenne, type='l',lwd = 3,
			col="black", xlab= "Year", ylab= "Habitat suitability",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 2)

	
}

	#------------------------

	# fonction qui calcul affiche et enregistre 

HSmeanCI = function(nbStep=20,nbSimul=1,nbBatch=1,ecriture=TRUE)
{
	vecHS = readHS()
	cat("\n step number =  ",0,"\n")
	vecCalcul =  makeVectorHS(readNbCol(0,0),vecHS)
	tabMeanCI = calculMeanCI(vecCalcul)
	for (i in 1:nbStep)
	{
		cat("\n step number =  ",i,"\n")
		vecCalcul = makeVectHsByStep(i,nbSimul,nbBatch,vecHS)
		tabMeanCI = rbind(tabMeanCI,calculMeanCI(vecCalcul))
		flush.console()

	}

	tabMeanCI = as.data.frame(tabMeanCI)

	row.names(tabMeanCI)= seq(1981,length.out=dim(tabMeanCI)[1],by=4)
	
	cat("\n \n \n======================= tabMeanCI ========================== \n")
	print(tabMeanCI)	
	cat("==================================================================== \n")

	plotMeanCI(tabMeanCI,ecriture=ecriture)

	if (ecriture == TRUE)
	{
		cat("  <== tabHSmeanCI.txt \n\n")
		write.table(tabMeanCI,"D:/Recherche/Héron/testSensibilit/sensibilyAnalis/tabHSmeanCI.txt")
	}

}

# farbique le graphe de la moyenne du nombre de colonie en France 


	# fabrique le tableau de la somme de chaque step pour chaque simulation

makedataSomme = function(nbStep,batch,nbSim,nbBatch)
{
  nombreSimulation = sum(nbSim)
  dataSomme = matrix(NA,nombreSimulation,nbStep)
	for (j in (1 : nbStep))
	{
	cat(j," ")
	flush.console()
	for (B in (1:length(batch)))
	{
  	theBatch = batch[B]
    for(b in (1:nbBatch[B]))
    	{
    		#cat(" " ,b)
        #if (b%%25==0) cat("\n")
    		#	flush.console()
 			
    	for (i in (1 : 1))#nbSim[b]))
    		{
  	     if(i==69)	 browser()	
          fileName = paste("D:/Recherche/Héron/testSensibilit/sensibilyAnalis/",theBatch,"_",b,"_",i,"_temp.",j,".txt", sep = "")
    			if (file.exists(fileName)) tab = as.data.frame(read.table(fileName, header= TRUE))
    			else cat(" !!! FICHIER ABSENT : ", fileName,"\n")
    			dataSomme[b,j]=sum(tab)
    			#cat(" " ,i)
    			#if (i%%25==0) cat("\n")
    			#flush.console()
    		}
    	}
 	  }

	}
	row.names(dataSomme) = 1:nombreSimulation
	colnames(dataSomme) = seq(1985,length.out=dim(dataSomme)[2],by=4)
	cat("\n\n <-- D:/Recherche/Héron/testSensibilit/tabColSommesensib.txt \n\n")
	write.table(dataSomme,"D:/Recherche/Héron/testSensibilit/tabColSommesensib.txt")
	return(dataSomme)
}

#makedataSomme(20,rep(1,100),100)
			


	#------------------------

	# Affichage graphique de la moyenne et du CI95 en pointillé en variable le tableau de moyenne et les CI

plotMeanCI_ablineVrai = function(tab,tab2,ecriture=TRUE)
{	
	x = row.names(tab)

	if (ecriture == TRUE)
	{
		fileNameWrite = "D:/Recherche/Héron/testSensibilit/COL_meanCI81sensib.png" # avec l'IC95
		cat("\n \n  <-- ",fileNameWrite,"\n")
		png(file=fileNameWrite)
		matplot(x, tab[1:3], type='l',
			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
			xlab= "Year", ylab= "Colony number",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 1.5)
		points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
		dev.off()

#		fileNameWrite = "F:/Héron/Publi/figures/COL_meanSD81.png" # avec l'IC95
#		cat("  <-- ",fileNameWrite,"\n")
#		png(file=fileNameWrite)
#		matplot(x, tab[,c(1,6,7)], type='l',
#			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#			xlab= "Years", ylab= "Colonies numbers",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#			main="Mean number of breeding colonies",font.main = 2, cex.main= 1.5)
#		points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
#		dev.off()
#
#		fileNameWrite2 = "F:/Héron/Publi/figures/COL_mean81.png" 
#		cat("  <-- ",fileNameWrite2,"\n\n")
#		png(file=fileNameWrite2)
#		plot(x, tab$moyenne, type='l',lwd = 3,
#			col="black", xlab= "Years", ylab= "Colonies numbers",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#			main="Mean number of breeding colonies",font.main = 2, cex.main= 1.5)
#		points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
#		dev.off()
#
	}
	
	x11() # avec l'IC95
	matplot(x, tab[1:3], type='l',
			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
			xlab= "Year", ylab= "Colony number",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 1.5)
	points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
	
#	x11() # avec le SD
#	matplot(x, tab[,c(1,6,7)], type='l',
#		col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#		xlab= "Years", ylab= "Colonies numbers",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#		main="Mean number of breeding colonies",font.main = 2, cex.main= 1.5)
#	points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
#	
#	x11()
#	plot(x, tab$moyenne, type='l',
#		col="black",lty = 1, lwd = 3,
#		xlab= "Years", ylab= "Colonies numbersn",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#		main="Mean number of breeding colonies",font.main = 2, cex.main= 1.5)
#	points(row.names(tab2),(tab2[,1]),type='p',pch=3, cex = 2)
#
	
}

	#------------------------

	# fonction qui calcul affiche et enregistre 

COLmeanCI = function(nbStep,batch,nbSimul,nbBatch,ecriture=TRUE)
{
	tabVrai = read.table("D:/Recherche/Héron/RardeaOrsay/tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
	cat("----------- Calcul Tableau de somme du nombre de colonie /step / simul ----------- \n")
	dataSomme = makedataSomme(nbStep,batch,nbSimul,nbBatch)
	
	cat("\n----------- Calcul Tableau de resumé /step ----------- \n")

	cat("\n step number =  ",0,"\n")
	tabMeanCI = calculMeanCI(tabVrai[1,1]) # initiation du tableau avec les valeur de 81

	for (i in 1:nbStep)
	{
		cat(i," ")
		#cat(" ",i)
    tabMeanCI = rbind(tabMeanCI,calculMeanCI(dataSomme[,i]))
		#flush.console()
	}

	tabMeanCI = as.data.frame(tabMeanCI)

	row.names(tabMeanCI)= seq(1981,length.out=dim(tabMeanCI)[1],by=4)
	
	cat("\n \n \n================================= tabMeanCI ===================================== \n")
	print(tabMeanCI)	
	cat("================================================================================= \n")


	if (ecriture == TRUE)
	{
		cat("  <== tabCOLmeanCIsensib.txt \n\n")
		write.table(tabMeanCI,"D:/Recherche/Héron/testSensibilit/tabCOLmeanCIsensib.txt")
	}


  	cat("----------- Calcul Tableau de HS mean /step / simul ----------- \n")
  # browser()
  vecHS = readHS()

  tabHSmeanWeight = matrix(0,nbBatch[1],(nbStep+1))
  frame.ColHS = data.frame(vecHS,readNbCol_origine(0,0))
  tabHSmeanWeight[,1] =  meanWeight(frame.ColHS)
	for (st in 1:nbStep)
	{
    cat(st," ")
    for (nb in 1:nbBatch[1])
    {
  	  frame.ColHS = data.frame(vecHS,readNbCol(batch,nb,1,st))
      tabHSmeanWeight[nb,st+1] =  meanWeight(frame.ColHS)
   	}
   	flush.console()
 	}
 	 	  cat("\n")
      write.table(tabHSmeanWeight,"D:/Recherche/Héron/testSensibilit/tabHSmeanWeightsensib.txt")
   #frame.ColHS = data.frame(vecHS,readNbCol(0,0))
	vecCalcul =  tabHSmeanWeight[,1]
	tabHSmean = calculMeanCI(vecCalcul)
	for (i in 1:nbStep)
	{
		 cat(i," ")
    vecCalcul = tabHSmeanWeight[,i+1]
		tabHSmean = rbind(tabHSmean,calculMeanCI(vecCalcul))
		flush.console()
 	}
 	plotMeanCI_ablineVrai(tabMeanCI,tabVrai,ecriture=ecriture)

	tabHSmean = as.data.frame(tabHSmean)
	row.names(tabHSmean)= seq(1981,length.out=dim(tabHSmean)[1],by=4)
	cat("\n \n \n======================= tabMeanCI ========================== \n")
	print(tabHSmean)
	cat("==================================================================== \n")
  write.table(tabHSmean,"D:/Recherche/Héron/testSensibilit/tabHSmeansensib.txt")
}







#	tabVrai = read.table("tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
#	tableau = read.table("tabCOLMeanCI.txt")
#	plotMeanCI_ablineVrai(tableau,tabVrai)



##########################################################

# Calcul de la moyenne pondéré de HS et des intervalles de confiences à 95%


	#--------------------------

	# Importe un vector de nbCol et on retourne un vecteur des HS avec une valeur de HS correspondant à la cellule dans laquelle est une colonie et par colonie

makeVectorHS = function(colHS)
{
		#browser()
    #vecHS = vecHS[which(vecNbCol>0),]
		#vecNbCol = vecNbCol[vecNbCol>0]
		tabcolHS = colHS[which(colHS[,2]>0),]
    vecReturn = c() # initialisation en vecteur vide
		for (i in 1:nrow(tabcolHS))
		{
			for(j in 1:vecNbCol[i])	vecReturn = c(vecReturn,vecHS[i])
		}
		return(vecReturn)
}




meanWeight = function(colHS)
return(sum(colHS[,1]*colHS[,2])/sum(colHS[,2]))



	#--------------------------

	# Ouverture du ficher nbCol en fonction de l'année et de la simulation

readNbCol = function(theBatch,param,nbSimul,nbStep)
{
#cat("\t nbSimul : ",nbSimul," / nbStep : ",nbStep)
      fileName = paste("D:/Recherche/Héron/testSensibilit/sensibilyAnalis/",
      theBatch,"_",param,"_",nbSimul,"_temp.",nbStep,".txt", sep = "")
      #cat("--> ",fileName,"\n")
	tab = read.table(fileName, header= TRUE)
	tab = as.vector(t(tab))
	return(tab)
}

# Ouverture du ficher nbCol en fonction de l'année et de la simulation

readNbCol_origine = function(nbStep,nbSimul)
{
#cat("\t nbSimul : ",nbSimul," / nbStep : ",nbStep)
	fileName = paste(
    "D:/Programmes/VisualWorks_7.6/cormas/Models/Heron/data/Heron",
    nbSimul,"_temp.",nbStep,".txt", sep = "")
#cat("--> ",fileName,"\n")
	tab = read.table(fileName, header= TRUE)
	tab = as.vector(t(tab))
	return(tab)
}


	#-------------------------

	# Ouverture du fichier HS

readHS = function()
{
		#browser()
  hs = read.table("D:/Programmes/VisualWorks_7.6/cormas/Models/Heron/maps/archive/hs.txt")

#	hs = as.vector(t(hs))
	return(hs)
}



	#------------------------

	# Boucle la fonction "makeVectorHS(,)" pour le calcul sur l'ensemble des simulations d'un step la concaténation de tout les vecReturn

makeVectHsByStep = function(nbStep,nbSimul,vecHS)
{
	vecReturn = c() # initialisation en vecteur vide
	for(i in 1:nbSimul)
	{	
		cat(" " ,i)
		if (i%%25==0) cat("\n")
		flush.console()
		vecNbCol = readNbCol(nbStep,i)
		vecReturn = c(vecReturn,makeVectorHS(vecNbCol,vecHS))
	}
	return(vecReturn)
}

	#------------------------

	# Calcul de la moyenne et de l'interval de confience CI 95%


calculMeanCI = function(vec)
{
	moyenne = mean(vec)
	CI95min = quantile(vec,0.025)
	CI95max = quantile(vec,0.975)
	mediane = median(vec)
	if (length(vec) > 1)
	{
		ecart_type = sd(vec)
		ecart_type_min = moyenne - (ecart_type / 2)
		ecart_type_max = moyenne + (ecart_type / 2)
	}
	else
	{
		cat("!!!!!! ATTENTION CALCUL DE ECART TYPE AVEC 1 VALEUR !!!!!! \n")
		ecart_type = moyenne
		ecart_type_min = moyenne
		ecart_type_max = moyenne
	}
	dataReturn = data.frame(moyenne,CI95min,CI95max,mediane,ecart_type,ecart_type_min,ecart_type_max,row.names=NULL)
	#cat("\n")
#	print(dataReturn)
	return(dataReturn)
}


	#------------------------

	# Affichage graphique de la moyenne et du CI95 en pointillé en variable le tableau de moyenne et les CI

plotMeanCI = function(tab,ecriture=TRUE)
{	
	x = row.names(tab)


	
	if (ecriture == TRUE)
	{
#		fileNameWrite = "F:/Héron/Publi/figures/HS_meanCI81.png" # avec l'IC95
#		cat("\n \n  <-- ",fileNameWrite,"\n")
#		png(file=fileNameWrite)
#		matplot(x, tab[1:3], type='l',
#			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#			xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#			main="HS weighted mean",font.main = 2, cex.main= 2)
#		dev.off()
#
#		fileNameWrite = "F:/Héron/Publi/figures/HS_meanSD81.png" # avec l'IC95
#		cat("  <-- ",fileNameWrite,"\n")
#		png(file=fileNameWrite)
#		matplot(x, tab[,c(1,6,7)], type='l',
#			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#			xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#			main="HS weighted mean",font.main = 2, cex.main= 2)
#		dev.off()
#
		fileNameWrite2 = "F:/Héron/Publi/figures/HS_mean81.png" 
		cat("  <-- ",fileNameWrite2,"\n\n")
		png(file=fileNameWrite2)
		plot(x, tab$moyenne, type='l',lwd = 3,
			col="black", xlab= "Year", ylab= "Habitat suitability",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 2)
		dev.off()

	}
	
#	x11() # avec l'IC95
#	matplot(x, tab[,1:3], type='l',
#		col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#		xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#		main="HS weighted mean",font.main = 2, cex.main= 2)
#	
#	x11() # avec le SD
#	matplot(x, tab[,c(1,6,7)], type='l',
#		col="black", lty = c(1,3,3), lwd = c(3,2,2), 
#		xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
#		main="HS weighted mean",font.main = 2, cex.main= 2)
#	
	x11()
	plot(x, tab$moyenne, type='l',lwd = 3,
			col="black", xlab= "Year", ylab= "Habitat suitability",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 2)

	
}

	#------------------------

	# fonction qui calcul affiche et enregistre 

HSmeanCI = function(nbStep,nbSimul,ecriture=TRUE)
{
	vecHS = readHS()
	cat("\n step number =  ",0,"\n")
	vecCalcul =  makeVectorHS(readNbCol(0,0),vecHS)
	tabMeanCI = calculMeanCI(vecCalcul)
	for (i in 1:nbStep)
	{
		cat("\n step number =  ",i,"\n")
		vecCalcul = makeVectHsByStep(i,nbSimul,vecHS)
		tabMeanCI = rbind(tabMeanCI,calculMeanCI(vecCalcul))
		flush.console()

	}

	tabMeanCI = as.data.frame(tabMeanCI)

	row.names(tabMeanCI)= seq(1981,length.out=dim(tabMeanCI)[1],by=4)
	
	cat("\n \n \n======================= tabMeanCI ========================== \n")
	print(tabMeanCI)	
	cat("==================================================================== \n")

	plotMeanCI(tabMeanCI,ecriture=ecriture)

	if (ecriture == TRUE)
	{
		cat("  <== tabHSmeanCI.txt \n\n")
		write.table(tabMeanCI,"tabHSmeanCI.txt")
	}

}



#      source("HSmeanCI.r")
#      HSmeanCI(,1)


#tableau = read.table("tabMeanCI.txt")
#plotMeanCI(tableau)








grapheColHS = function(nbStep = 20,nbSimul = 300)
{ 	

  tabVrai = read.table("tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
	cat("----------- Calcul Tableau de somme du nombre de colonie /step / simul ----------- \n")
	dataSomme = makedataSomme(nbStep,nbSimul)
	
	cat("\n----------- Calcul Tableau de resumé /step ----------- \n")

	cat("\n step number =  ",0,"\n")
	tabMeanCI = calculMeanCI(tabVrai[1,1]) # initiation du tableau avec les valeur de 81

	for (i in 1:nbStep)
	{
		cat("\n step number =  ",i,"\n")
		tabMeanCI = rbind(tabMeanCI,calculMeanCI(dataSomme[,i]))
		flush.console()
	}

	tabMeanCI = as.data.frame(tabMeanCI)

	row.names(tabMeanCI)= seq(1981,length.out=dim(tabMeanCI)[1],by=4)
	
	cat("\n \n \n================================= tabMeanCI ===================================== \n")
	print(tabMeanCI)	
	cat("================================================================================= \n")
  
  plot.heron(tabMeanCI,dataSomme,tabHSmean)
  
 	if (ecriture == TRUE)
	{
		cat("  <== D:/Recherche/Héron/testSensibilit/tabCOLmeanCIsensib.txt \n\n")
		write.table(tabMeanCI,"D:/Recherche/Héron/testSensibilit/tabCOLmeanCIsensib.txt")
	}
	

}



plot.heron = function(tabColSimul,tabColVrai,tabHS)
{

browser()
  plot.new()
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = margen, pty = "s", las = 1, new = FALSE)
 	matplot(x, tabColSimul[1:3], type='l',
			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
			xlab= "Year", ylab= "Colony number",font.lab = 3, 
      cex.lab = 1.5, cex.axis= 1.2,font.main = 2, cex.main= 1.5)
	points(row.names(tabColVrai),(tabColVrai[,1]),type='p',pch=3,cex = 2)
  lines(row.names(tabColVrai), (tabHS[,1]), col = "gray", lwd = 2)
  axis(2)
  axis(4)
  axis(1)
  mtext("Number of colony", 2, col = 1, las = 1, line = 3, adj = 0, at = 55)
  mtext("HS weighted mean", 4, col = 2, las = 1, line = 3, adj = 1, at = 55)
  mtext("Years", 1, col = 2, las = 1, line = 3, adj = 1, at = 55)
  abline(0, 0)
}


# grapheColHS(3,5)

  


 



#      source("HSmeanCI.r")
#     HSmeanCI(20,1,70)


#tableau = read.table("tabMeanCI.txt")
#plotMeanCI(tableau)


#COLmeanCI(20,2:4,c(rep(1,194),rep(1,150),rep(1,56)),c(194,150,56))
COLmeanCI(20,c(8),rep(1,300),c(300))



