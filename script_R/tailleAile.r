# Analyse donnees Donges

# Recherche de patern dans la distribution de la taille des ailles

# lecture tableau
# selection des baguages
# pour chaque espece centre reduire la LP
# pour phénologie calcul de la LP mean par jour et par an


# tableau = lectureTableau()
lectureTableau = function()
{
  tableau = read.csv(
    "D:/Nature/Baguage/Donges/Analyse Donnée/Donne2003-2007.csv")
  tableau = convert.tableau(tableau)  
  return(tableau)
}



convert.tableau = function(tableau)
{
  new.table = tableau
#browser()
#  new.table[,4] = as.character(tableau[,4])
#  new.tableDateHeure = ifelse(tableau[,9]=="",strptime(as.character(tableau[,4]),"%d/%m/%Y"),strptime(paste(as.character(tableau[,4]),as.character(tableau[,9])),"%d/%m/%Y %HH%M"))
 #   colonneNum = c(15:17)
  new.table[,15] = as.numeric(as.character(tableau[,15]))
  new.table[,16] = as.numeric(as.character(tableau[,16]))
  new.table[,17] = as.numeric(as.character(tableau[,17]))
#  new.table = cbind(new.table[,1:4],new.tableDateHeure,new.table[5:21])
  return(new.table)
}



# table.B = actBaguage(tableau)
actBaguage = function(tableau)
{
  new.tableau = tableau[which(tableau[,3] == "B"),]
  new.tableau[,12] =  as.factor(new.tableau[,12])
  return(new.tableau)
}

# tableClear.B = clearTableauLP(table.B)
clearTableauLP = function(tableau)
{
  clear.tableau = tableau[which(tableau$LP>10),]
  return(clear.tableau)
}


distibEsp = function(tableau,sp)
{
#  tableau.sp = tableau[which(tableau$ESPECE==sp),] 
#  browser()
 # LPnorme = scale(tableau$LP, center=TRUE, scale=TRUE)
 # frame.sp = data.frame(tableau.sp,LPnorme)
  hist.aile(tableau$LP,sp)
 # return(frame.sp)
}




hist.aile = function(unVecteur,nomSp,fichier=TRUE,affichage=FALSE)
{
# histo de la distribution de la LP
#	browser()
  if (affichage)
	{
		hist(unVecteur,xlab= "LP", ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
	}
	if (fichier)
	{
		fileName = paste("distribAile/",
      nomSp,'.png',sep='')
		cat("  <-- ",fileName,"\n")
		png(file=fileName)
		hist(unVecteur,xlab= "LP", ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
		dev.off()
	}
	flush.console()
}




distribEsp.tout = function(tableau)
{
  lesEspeces = unique(tableau$ESPECE)
  i = 1
  sp = lesEspeces[i]
  tableauSp = tableau[which(tableau$ESPECE == sp),]
  table.summary = distibEsp(tableauSp,sp)
  for ( i in 2:length(lesEspeces))
  {
    sp = lesEspeces[i]
    tableauSp = tableau[which(tableau$ESPECE == sp),]
    table.summary = rbind(table.summary,distibEsp(tableauSp,sp))
  }
  
    write.table(table.summary , "distribAile/tableSummaryAile.txt")

}


normalisationToutsp = function(tableau)
{
 lesEspeces = c("ACRARU","ACROLA","ACRSCH","ACRSCI","ALCATT",
  "CARCAN","CARCHL","CISJUN","EMBSCH","ERIRUB","HIPPOL","HIRRUS",
  "JYNTOR","LOCLUS","LOCNAE","LUSSVE","LUSMEG","MOTFLA",
  "PANBIA","PARCAE","PARMAJ","PHYCOL","PHYLUS","PRUMOD",
  "RIPRIP","SAXRUB","SAXTOR","STUVUL","SYLBOR","SYLCOM"
  ,"TURMER","TURPHI")
# browser()
  i = 1
  sp = lesEspeces[i]
  table.normal = normalisation.sp(tableau,sp)
  for (i in 2:length(lesEspeces))
  {
    sp = lesEspeces[i]
    table.normal = rbind(table.normal,normalisation.sp(tableau,sp))
  }
  
  tableauJourAnnee.LPnorme(table.normal)
}



normalisation.sp = function(tableau,sp)
{   
#browser()   
   tableauSp = tableau[which(tableau$ESPECE == sp),]
   LPnorme = scale(tableauSp$LP, center=TRUE, scale=TRUE)
   frame.sp = data.frame(tableauSp,LPnorme)
   tableauJourAnnee.LPnorme(frame.sp)
   return(frame.sp)
   
}


tableauJourAnnee.LPnorme = function(tableau)
{

  minLigne = min(tableau$VAL_JOUR)
  minColonne = min(tableau$ANNE)
  nomColonne = 2003:2008
#browser()
  nomLigne = seq(min(tableau$VAL_JOUR),max(tableau$VAL_JOUR))
  sommeLP = matrix(
              0,
              max(tableau$VAL_JOUR)-min(tableau$VAL_JOUR)+1,
              6)
  sommeIter = matrix(
              0,
              max(tableau$VAL_JOUR)-min(tableau$VAL_JOUR)+1,
              6)
              
  for ( b in 1:nrow(tableau))
  {    
      i = tableau$VAL_JOUR[b] - minLigne + 1
      j = tableau$ANNEE[b] - minColonne + 1
      sommeLP[i,j] = sommeLP[i,j] + tableau$LPnorme[b]
      sommeIter[i,j] = sommeIter[i,j] + 1
  }

  sommeIter = ifelse(sommeIter>0,sommeIter, NA)
  moyenneLP = sommeLP / sommeIter
#browser()
  colnames(moyenneLP) = nomColonne
  row.names(moyenneLP) = nomLigne
  if (length(unique(tableau$ESPECE))==1)
		  sp = tableau$ESPECE[1]
  else
      sp = "Toutes especes"
  matplotLP(moyenneLP,sp)
}


matplotLP = function(unSousTableau,sp,affichage=FALSE,fichier = TRUE,
                                tailleTitre = 1.5, tailleAxe = 1.2,
                                couleurFont = 'Black', couleurTexte = 'white',
                                typeLigne = 1, largeurLigne = 3)
{

  if (affichage)
	{
		X11()
    nomVariable = "moyenne LP norme"
		texteLegende = colnames(unSousTableau)[1:5]
		x = row.names(unSousTableau)
		require(graphics)
		palette()               # obtain the current palette
		palette(rainbow(length(texteLegende),start=.5,end=.1))#,start=0.4,end=0.05))   
    par(bg = couleurFont, col = couleurTexte) 
    matplot(x,unSousTableau[,1:5], type='l',pch = 20,lty = typeLigne, lwd = largeurLigne, 
      col=c(1:length(texteLegende)),
		  xlab= "Jours calendaires", ylab= nomVariable,
      font.lab = 3, cex.lab = tailleAxe, cex.axis= 1.2,
		  main = sp, xlim = c(200, 240),
      font.main = 1, cex.main= tailleTitre, 
      col.axis = couleurTexte, col.lab = couleurTexte,
      col.main = couleurTexte
    )
    legend("topright", texteLegende, pch=20,col=c(1:length(texteLegende)))
		palette("default")      # reset back to the default
	}


 if (fichier)
	{
    nomVariable = "moyenne LP norme"
    fileName = paste('phenoAileL/',sp,'.png',sep='')
		cat("  <-- ",fileName,"\n")
		png(file=fileName)
		texteLegende = colnames(unSousTableau)[1:5]
		x = row.names(unSousTableau)
		require(graphics)
		palette()               # obtain the current palette
		palette(heat.colors(length(texteLegende)))#,start=.6,end=.1))#,start=0.4,end=0.05))   
    par(bg = couleurFont, col = couleurTexte) 
    matplot(x,unSousTableau, type='l',pch = 20,lty = typeLigne, lwd = largeurLigne, 
      col=rev(c(1:length(texteLegende))),
		  xlab= "Jours calendaires", ylab= nomVariable,
      font.lab = 3, cex.lab = tailleAxe, cex.axis= 1.2,
		  main = sp,  xlim = c(200, 240),
      font.main = 1, cex.main= tailleTitre, col.axis = couleurTexte, col.lab = couleurTexte,
      col.main = couleurTexte
    )
		legend("topright", texteLegende, pch=20,col=rev(c(1:length(texteLegende))))
		palette("default")      # reset back to the default
		dev.off()
	}
	flush.console()
}



# resum = distibEsp("ACRSCI",table.B)
# resum



# tableauTout = lectureTableau()
# table.B = actBaguage(tableauTout)
# tableClear.B = clearTableauLP(table.B)
# distribEsp.tout(tableClear.B) 
normalisationToutsp(tableClear.B)


#table.B$LP[1]
#essai = as.numeric(table.B$LP[1])
#table.B$LP = as.numeric(table.B$LP)