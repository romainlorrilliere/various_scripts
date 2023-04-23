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

#	tabMeanCI = read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt",header=TRUE)
#
#		tabHSmean = read.table("D:/Recherche/Héron/PubliHéron/figures/tabHSmean.txt",header=TRUE)
#
#  tabVrai = read.table("D:/Recherche/Héron/RardeaOrsay/tabColSomVrai.txt",row.names = 1,sep="\t",dec=",")
#
#    tabSensib = read.table("D:/Recherche/Héron/testSensibilit/tabCOLmeanCIsensib.txt",header=TRUE)
#  tabHSsensib = read.table("D:/Recherche/Héron/testSensibilit/tabHSmeansensib.txt",header=TRUE)
#  
#  plot.heron2(tabMeanCI,tabVrai,tabHSmean, tabSensib, tabHSsensib)
#

makedataMean = function(nbStep,nbSim)
{
	dataMean = matrix(0,10000,nbStep)
	for (j in (1 : nbStep))
	{
	cat(j," ")
	flush.console()
	totalTab = matrix(0,10000,nbSim)
	for (i in (1 : nbSim))
	{		
    fileName = paste("D:/Programmes/VisualWorks_7.6/cormas/Models/Heron/data/Heron",i,"_temp.",j,".txt", sep = "")
		tab = as.data.frame(read.table(fileName, header= TRUE))
		totalTab[,i] = tab[,1]
#		cat(" " ,i)
#		if (i%%25==0) cat("\n")
#		flush.console()
	}
  dataMean[,j]=rowMeans(totalTab)      	
	}
	row.names(dataMean) = 1:nrow(dataMean)
	colnames(dataMean) = seq(1985,length.out=dim(dataMean)[2],by=4)
	cat("\n\n <-- tabColMean.csv \n\n")
	write.csv(dataMean,"D:/Recherche/Héron/tabColMean.csv")
	return(dataMean)
}


tabMapFranceMeanObsPredXY = function(nbStep = 20,nbSimul = 300,ecriture=TRUE)
{ 	

 	cat("----------- Calcul Tableau de moyenne du nombre de colonie /step / simul ----------- \n")
	dataMean = makedataMean(nbStep,nbSimul)
  hs1981 = read.csv("D:/Recherche/Héron/hs1981.csv",header=TRUE)
  colnames(hs1981)= c("id1981","hs1981","obs1981")
  hs2000 = read.csv("D:/Recherche/Héron/ColonieGrid2000.csv")
  colnames(hs2000)= c("id2000","hs2000","obs2000")  
  colonie = hs2000$obs2000
  colonietab = matrix(0,100,100)
  for(i in 1:100)
  colonietab[,i]=colonie[((i-1)*100+1) :(i*100)]
  #filled.contour(1:100,1:100,colonietab)
#  browser()
  colonietab2 = matrix(0,100,100)
  for(i in 1:100)
  colonietab2[,i] = colonietab[,100-(i-1)]
 #  filled.contour(1:100,1:100,colonietab2)  
  
  colonie2  = vector()
  for(i in 1:100)
  colonie2 = c(colonie2,colonietab2[,i])
  
  obs2000 = colonie2
  
  dataMean = cbind(dataMean,hs1981,obs2000)
  tab = cbind(dataMean[,21],dataMean[,22],dataMean[,1:20],dataMean[,23:24] )
  colnames(tab) = c("id","hs",paste("predit",colnames(dataMean)[1:20],sep=""),colnames(dataMean)[23:24])
  
  for (an in c(85,89,94)) 
    tab = ajoutObs(tab,an)
  
  #attach(tab)
#edit(id)
  id = as.numeric(paste(tab$id))
  X = id %% 100
  Y = 100-(id %/% 100)
  tab = cbind(X,Y,tab)
  tab2 = cbind(tab[,3:4],tab[1:2],tab[,5:25],tab[,29],tab[,32],tab[,35],tab[,26])
  colnames(tab2) = colnames(tab)[c(3:4,1:2,5:25,29,32,35,26)]
#edit(tab)


 
  cat("\n\n <-- tabColMean.csv \n\n")
	write.csv(tab,"D:/Recherche/Héron/tabColMean.csv",row.names=FALSE)
	
	cat("\n\n <-- tabColMeanPredObsXY.csv \n\n")
	write.csv(tab2,"D:/Recherche/Héron/tabColMeanPredObsXY.csv",row.names=FALSE)
	return(tab2)
 
  
  
}

ajoutObs=function(tab,an,repertoire = "D:/Recherche/Héron/")
{

  fichierAn = paste(repertoire,"GLMFrance",an,".txt",sep="")
  tabAn = read.table(fichierAn,header=TRUE)
  vecI = sort(seq(1,(dim(tab)[1]),by = 100),decreasing=TRUE)
  z=0
  tabAnNew = matrix(0,10000,3)
   # browser()
  for(i in vecI)
  {
    for( j in i:(i+99))
    {
      if(tab$hs[j]>0)
      {
          #browser()
        z = z+ 1
        tabAnNew[j,1] = tabAn$id[which(tabAn$id==z)]
        tabAnNew[j,2] = tabAn$hs[which(tabAn$id==z)]
        tabAnNew[j,3] = (tabAn$col[which(tabAn$id==z)]+tabAn$newCol[which(tabAn$id==z)])

      }
    }
  }
  colnames(tabAnNew) = paste(c("id","hs","obs19"),an,sep="")
 # browser()
  return(cbind(tab,tabAnNew))
  



}

grapheMapFrance = function(tab,anneePredit = 2001,anneeObs = 2000)
{
   repertoire = "D:/Recherche/Héron/"

  nombreNiveau = 20
  maximumCouleur = 0.5

  variable = paste("obs",anneeObs,sep="")
  colonne = which(colnames(tab)== variable)
  selection = which(tab[,colonne] > 0)
  plot(tab$X[selection],tab$Y[selection],pch =20 ,cex = 0.5,col = "red")

  variable = paste("predit",anneePredit,sep="")
  colonne = which(colnames(tab)== variable)
  tabKrig = matrix(NA,100,100)
  
  
  fichierPlot = paste(repertoire,"plotObs",anneeObs,sep="")
  savePlot(fichierPlot,"tif")

  for(i in 1:10000)
    if (tab$hs[i]>0)
    {
      if (tab[i,colonne] > maximumCouleur) tabKrig[tab$X[i],tab$Y[i]]= maximumCouleur
      else tabKrig[tab$X[i],tab$Y[i]]=tab[i,colonne]
    }
      #
  #
  tabKrig = (tabKrig*10 + 1)
  tabKrigLog = log(tabKrig)
  
  tabKrig = -1 * (tabKrigLog/max(tabKrigLog,na.rm=TRUE)) + 1
 #summary(tabKrig)
  
 # krigLim = range(tabKrig, finite=TRUE)
 
  couleurs =  gray(seq(0,1,length.out=nombreNiveau))
  image(1:100,1:100,tabKrig,col = couleurs)
  fichierPlot = paste(repertoire,"plotPredit",anneePredit,sep="")
  savePlot(fichierPlot,"tif")

  #browser()
  variable = paste("obs",anneeObs,sep="")
  colonne = which(colnames(tab)== variable)
  selection = which(tab[,colonne] > 0)
  points(tab$X[selection],tab$Y[selection],pch =20 ,cex = 1.5,col = "red")
  
  fichierPlot = paste(repertoire,"plotProjeter",anneePredit,"_",anneeObs,sep="")
  savePlot(fichierPlot,"tif")
  
   # browser()
#    tabLegend = as.matrix(niveau[-length(niveau)]+0.001,nrow = 1, ncol = nombreNiveau,byrow =TRUE)
#  image(tabLegend, col = couleurs)
#       fichierPlot = paste(repertoire,"legende",sep="")
#  savePlot(fichierPlot,"tif")
# 
# a = (0:(maximumCouleur*1000)) / 1000
# tabKrigLog = log(a*10 + 1)
# tabKrig = -1 * (tabKrigLog/max(tabKrigLog,na.rm=TRUE)) + 1
#  echelle = as.vector(quantile(tabKrig,(round(seq(0,1,length.out=nombreNiveau),2)),na.rm=TRUE))
#  echelleIndex = which(tabKrig==echelle[1])
# # browser()
#  for(i in 2:nombreNiveau)
#  echelleIndex = c(echelleIndex,which(tabKrig==echelle[i]))
#  print(a[echelleIndex])

 
  
}


#tab = tabMapFranceMeanObsPredXY()
#an = 85
#head(tab)
#tab = ajoutObs(tab,an)
tab =read.csv("D:/Recherche/Héron/tabColMeanPredObsXY.csv")
grapheMapFrance(tab,anneePredit = 2001,anneeObs = 2000)
grapheMapFrance(tab,anneePredit = 1985,anneeObs = 1985)
grapheMapFrance(tab,anneePredit = 1993,anneeObs = 1994)
