# farbique le graphe de la moyenne du nombre de colonie en France 


	# fabrique le tableau de la somme de chaque step pour chaque simulation

makedataSomme = function(nbStep,nbSim)
{
	dataSomme = matrix(0,nbSim,nbStep)
	for (j in (1 : nbStep))
	{
	cat(j," ")
	flush.console()
	for (i in (1 : nbSim))
		{
			
      fileName = paste("D:/Programmes/VisualWorks_7.6/cormas/Models/Heron/data/Heron",i,"_temp.",j,".txt", sep = "")
			tab = as.data.frame(read.table(fileName, header= TRUE))
			dataSomme[i,j]=sum(tab)
			#cat(" " ,i)
			#if (i%%25==0) cat("\n")
			#flush.console()
		}
		
	
	}
	row.names(dataSomme) = 1:nbSim
	colnames(dataSomme) = seq(1985,length.out=dim(dataSomme)[2],by=4)
	cat("\n\n <-- tabColSomme.txt \n\n")
	write.table(dataSomme,"tabColSomme.txt")
	return(dataSomme)
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
		cat("!!!!!! ATTENTION CALCUL DE L ECART TYPE AVEC 1 VALEUR !!!!!! \n")
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

plotMeanCI_ablineVrai = function(tab,tab2,ecriture=TRUE)
{	
	x = row.names(tab)
	if (ecriture == TRUE)
	{
		fileNameWrite = "F:/Héron/Publi/figures/COL_meanCI81.png" # avec l'IC95
		cat("\n \n  <-- ",fileNameWrite,"\n")
		png(file=fileNameWrite)
		matplot(x, tab[1:3], type='l',
			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
			xlab= "Year", ylab= "Colony number",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 1.5)
		points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
		dev.off()
 	}
	x11() # avec l'IC95
	matplot(x, tab[1:3], type='l',
			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
			xlab= "Year", ylab= "Colony number",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 1.5)
	points(row.names(tab2),(tab2[,1]),type='p',pch=3,cex = 2)
}

	#------------------------

# fonction qui calcul affiche et enregistre 

COLmeanCI = function(nbStep,nbSimul,ecriture=TRUE)
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

	plotMeanCI_ablineVrai(tabMeanCI,tabVrai,ecriture=ecriture)

	if (ecriture == TRUE)
	{
		cat("  <== D:/Recherche/Héron/PubliHéron/figures/tabCOLmeanCI.txt \n\n")
		write.table(tabMeanCI,
      "D:/Recherche/Héron/PubliHéron/figures/tabCOLmeanCI.txt")
	}

}

#	tabVrai = read.table("tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
#	tableau = read.table("tabCOLMeanCI.txt")
#	plotMeanCI_ablineVrai(tableau,tabVrai)



##########################################################

# Calcul de la moyenne pondéré de HS et des intervalles de confiences à 95%


	#--------------------------

	# Importe un vector de nbCol et on retourne un vecteur des HS avec une valeur 
# de HS correspondant à la cellule dans laquelle est une colonie et par colonie

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

readNbCol = function(nbStep,nbSimul)
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
	hs = read.table("D:/Programmes/VisualWorks_7.6/cormas/Models/Heron/maps/archive/hs.txt")
#	browser()
#	hs = as.vector(t(hs))
	return(hs)
}
	


	#------------------------

	# Boucle la fonction "makeVectorHS(,)" 
# pour le calcul sur l'ensemble des simulations d'un step la concaténation 
# de tout les vecReturn

makeVectHsByStep = function(nbStep,nbSimul,vecHS)
{
	vecReturn = c() # initialisation en vecteur vide
	for(i in 1:nbSimul)
	{	
	#	cat(" " ,i)
	#	if (i%%25==0) cat("\n")
	#	flush.console()
		vecNbCol = readNbCol(nbStep,i)
	browser()
	frame.ColHS = data.frame(vecHS,vecNbCol)
		vecReturn = c(vecReturn,makeVectorHS(frame.ColHS))
	}
	return(vecReturn)
}

	#------------------------

	# Affichage graphique de la moyenne et du CI95 en pointillé 
#  en variable le tableau de moyenne et les CI

plotMeanCI = function(tab,ecriture=TRUE)
{	
	x = row.names(tab)
	if (ecriture == TRUE)
	{
		fileNameWrite2 = "D:/Recherche/Héron/PubliHéron/figures/HS_mean81.png" 
		cat("  <-- ",fileNameWrite2,"\n\n")
		png(file=fileNameWrite2)
		plot(x, tab$moyenne, type='l',lwd = 3,
			col="black", xlab= "Year", ylab= "Habitat suitability",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			,font.main = 2, cex.main= 2)
		dev.off()
	}
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
		cat("  <== D:/Recherche/Héron/PubliHéron/figures/tabHSmeanCI.txt \n\n")
		write.table(tabMeanCI,"D:/Recherche/Héron/PubliHéron/figures/tabHSmeanCI.txt")
	}
}



#      source("HSmeanCI.r")
#      HSmeanCI(,1)


#tableau = read.table("tabMeanCI.txt")
#plotMeanCI(tableau)





##########################################################



grapheColHS = function(nbStep = 20,nbSimul = 300,ecriture=TRUE)
{ 	

  tabVrai = read.table("D:/Recherche/Héron/RardeaOrsay/tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
	cat("----------- Calcul Tableau de somme du nombre de colonie /step / simul ----------- \n")
	dataSomme = makedataSomme(nbStep,nbSimul)
	
	cat("\n----------- Calcul Tableau de resumé /step ----------- \n")

#	cat("\n step number =  ",0,"\n")
	tabMeanCI = calculMeanCI(tabVrai[1,1]) # initiation du tableau avec les valeur de 81

	for (i in 1:nbStep)
	{
		cat(i," ")
		tabMeanCI = rbind(tabMeanCI,calculMeanCI(dataSomme[,i]))
		flush.console()
	}             

	tabMeanCI = as.data.frame(tabMeanCI)

	row.names(tabMeanCI)= seq(1981,length.out=dim(tabMeanCI)[1],by=4)
	
	cat("\n \n \n================================= tabMeanCI ===================================== \n")
	print(tabMeanCI)	
	cat("================================================================================= \n")
  
  
	cat("----------- Calcul Tableau de HS mean /step / simul ----------- \n")

  vecHS = readHS()
 # browser()
  tabHSmeanWeight = matrix(0,nbSimul,(nbStep+1))
  frame.ColHS = data.frame(vecHS,readNbCol(0,0))
  tabHSmeanWeight[,1] =  meanWeight(frame.ColHS)
	for (st in 1:nbStep)
	{
    cat(st," ")
    for (sim in 1:nbSimul)
    {
  	  frame.ColHS = data.frame(vecHS,readNbCol(st,sim))
      tabHSmeanWeight[sim,st+1] =  meanWeight(frame.ColHS)
   	}
   	flush.console()
 	}
 	 	  cat("\n")
      write.table(tabHSmeanWeight,"D:/Recherche/Héron/PubliHéron/figures/tabHSmeanWeight.txt")
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
 	
	tabHSmean = as.data.frame(tabHSmean)
	row.names(tabHSmean)= seq(1981,length.out=dim(tabHSmean)[1],by=4)
	cat("\n \n \n======================= tabMeanCI ========================== \n")
	print(tabHSmean)	
	cat("==================================================================== \n")
  
  tabSensib = read.table("D:/Recherche/Héron/testSensibilit/tabCOLmeanCIsensib.txt",header=TRUE)
  tabHSsensib = read.table("D:/Recherche/Héron/testSensibilit/tabHSmeansensib.txt",header=TRUE)
  
  plot.heron2(tabMeanCI,tabVrai,tabHSmean, tabSensib, tabHSsensib)
  
 	if (ecriture == TRUE)
	{
		cat("  <== D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt \n\n")
		write.table(tabMeanCI,"D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt")
		cat("  <== D:/Recherche/Héron/PubliHéron/figures/tabHSmean.txt \n\n")
		write.table(tabHSmean,"D:/Recherche/Héron/PubliHéron/figures/tabHSmean.txt")
	}

}



plot.heron = function(tabColSimul,tabColVrai,tabHS)
{
  x = row.names(tabColSimul)
  plot.new()
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = margen, pty = "s", las = 1, new = FALSE)
  
  colCol = "black"
  colHS = "blue"
  
 	matplot(x, tabColSimul[1:3], type='l',
			col= colCol, lty = c(1,3,3), lwd = c(3,2,2), font.lab = 3, 
      xlab= "", ylab= "",
      cex.lab = 1.5, cex.axis= 1.2,font.main = 2, cex.main= 1.5,axes = FALSE)

  points(row.names(tabColVrai),(tabColVrai[,1]),type='p',pch=3,cex = 2)

  yGmax = max(tabColSimul)
  yGmin = 0
  yDmax = trunc(max(tabHS[,1]))+1
  yDmin = trunc(min(tabHS[,1]))-1
  tabHS.trans = ((tabHS-yDmin)/(yDmax-yDmin)) * yGmax
  tabHS.trans = tabHS.trans[-1,]
 
  lines(row.names(tabHS.trans), (tabHS.trans[,1]), col = colHS, lwd = 2)
  coordLabelAxe = c(0,(yGmin:(yGmax/250)+1) * 250)
  labCol = paste(
    as.integer(seq(from = yGmin , to = yGmax , 
      by = (yGmax - yGmin) / (length(coordLabelAxe)-1))))
  labHS = paste(
    round(seq(from = yDmin , to = yDmax , 
      by = (yDmax - yDmin) / (length(coordLabelAxe)-1),digits=2),digits=2))
  axis(2, coordLabelAxe, labels = labCol, col.axis = colCol)
  axis(4, coordLabelAxe, labels = labHS, col.axis = colHS)
  axis(1)
  box()
  mtext("Number of colonies", 
    side = 2, col =colCol, las = 3, line = 4, adj = 0.5)
  mtext("HS weighted mean", 
    side = 4, col = colHS, las = 3, line = 4, adj = 0.5)
  mtext("Years", 
    side = 1, col = "black", las = 1, line = 2, adj = 0.5)
}


 

plot.heron2 = function(tabColSimul,tabColVrai,tabHS,tabSensib, tabHSsensib)
{
  #fichierFigure = "D:/Recherche/Héron/PubliHéron/figures/colHSibis.eps" 
  #postscript(fichierFigure)
  
  fichierFigure = "D:/Recherche/Héron/PubliHéron/figures/colHSibis" 
 # png(fichierFigure)
  plot.new()
	m = matrix(c(2,1),2,1)
	layout(m)
	layout.show(2)
  x = row.names(tabColSimul)
  par(mar=c(5,4,0,2))
  
  matplot(x, cbind(tabColSimul[,1],tabSensib[,2:3]),ann = FALSE,axes = TRUE,col=c("white","black","black"),type = 'l',lty=c(1,2,2),lwd=c(1,2,2))
#browser()
  polygonCI = rbind(
    cbind(as.numeric(row.names(tabColSimul)),tabColSimul[,2]),
    cbind(rev(as.numeric(row.names(tabColSimul))),rev(tabColSimul[,3])))
  polygon(polygonCI[,1], polygonCI[,2], col = gray(0.8), border = gray(0.8))
  lines(x, tabColSimul[,1], type='l',lty = 1, lwd = 2)  
  points(row.names(tabColVrai),(tabColVrai[,1]),type='p',pch=3,cex = 2)
  mtext("Number of colonies", 
    side = 2, las = 3, line = 3, adj = 0.5)
  mtext("Years", 
    side = 1, las = 1, line = 2, adj = 0.5)  
  #tabHS[1,] = NA
  par(mar = c(0,4,4,2))
  #plot(row.names(tabHS),tabHS[,1],
 
   matplot(x,cbind(tabHS[,1],tabHSsensib[,2:3]),
    type = 'l', lty = c(1,2,2), lwd = c(2,2,2),col="black", ann = FALSE,axes = FALSE)
  #  browser()
   polygonCI = rbind(
    cbind(as.numeric(row.names(tabHS)),tabHS[,2]),
    cbind(rev(as.numeric(row.names(tabHS))),rev(tabHS[,3])))
  polygon(polygonCI[,1], polygonCI[,2],col = gray(0.8), border = gray(0.8))
#  effaceHS = tabHS[1:2,]
  lines(x,tabHS[,1],type='l',lty = 1, lwd = 2)
  lines(x,tabHSsensib[,2],type='l',lty=2,lwd=2)
  axis(2)
  box()
  mtext("HS weighted mean", 
    side = 2, las = 3, line = 3, adj = 0.5)
    
  #dev.off()
  savePlot(fichierFigure,type="png")
  savePlot(fichierFigure,type="eps")
  cat("<== ",fichierFigure,"\n")

	
 

}


grapheColHS_seul = function()
{
  tabCol =  read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt",row.names = 1,sep="\t",dec=",")
  tabVrai =  read.table("D:/Recherche/Héron/PubliHéron/figures/tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
  tabHs = read.table("D:/Recherche/Héron/PubliHéron/figures/tabHSmean.txt",row.names = 1,sep="\t",dec=",")
  plot.heron2(tabCol,tabVrai,tabHs)

}

#grapheColHS()
#grapheColHS_seul()



	tabMeanCI = read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt",header=TRUE)

		tabHSmean = read.table("D:/Recherche/Héron/PubliHéron/figures/tabHSmean.txt",header=TRUE)

  tabVrai = read.table("D:/Recherche/Héron/RardeaOrsay/tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")

    tabSensib = read.table("D:/Recherche/Héron/testSensibilit/tabCOLmeanCIsensib.txt",header=TRUE)
  tabHSsensib = read.table("D:/Recherche/Héron/testSensibilit/tabHSmeansensib.txt",header=TRUE)
  
  plot.heron2(tabMeanCI,tabVrai,tabHSmean, tabSensib, tabHSsensib)
