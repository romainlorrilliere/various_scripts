###########################################
#                                         #
#  Moulinette d'analyse des données STOC  #
#                                         #
###########################################

# 1. lecture de la table
#       fonction : lectureFichier()
# 2. tableau croisé dynamique global
#   sortir pour chaque espèce capturée un bilan globale
#       fonction : croiseGlobal
# 3. tableau croisé dynamique par session
#   sortir pour chaque session un bilan
#       fonction : croiseSession
# 4. bilan par filet
#   sortir tableau pour chaque filet
#   carte kriggeage de distribution des populations les plus abondantes
#   et indice de shannon
# 5. les controles
#   les histoires individuelles
#   les déplacements

lectureFichier = function(an)
{
  nomFichier = paste("D:/Nature/STOC cojoux/AnalyseDonnées/STOC",an,".csv",sep="")
  cat('==>',nomFichier,"\n")
  return(read.csv(nomFichier))
}

# connection a la base de donnees
ouvrirConnection = function(nom_base)
  return(odbcConnect(nom_base))  

femerConnection = function(connexion)
  close(connexion)


bilan.global = function(tab,annee)
{
  tabB = tab[which(tab$ACT=="B"),] 
  DATE = annee
  NB_BAGUAGE = nrow(tabB)
  NB_ESPECE = length(unique(tabB$ESPECE))
  male = length(which(tabB$SEXE=="M"))
  femelle = length(which(tabB$SEXE=="F"))
  male_p = length(which(tabB$SEXE=="M?"))
  femelle_p = length(which(tabB$SEXE=="F?"))
  SEX_RATIO = femelle /(male+femelle)
  SEX_RATIO_P = (femelle_p + femelle)  / (male + male_p + femelle + femelle_p)
  SEX_INCERT = (male_p + femelle_p) / (male + male_p + femelle + femelle_p)
  juv = length(which(tabB$AGE=="1A" | tabB$AGE=="PUL"))
  ad = length(which(tabB$AGE=="+1A" | tabB$AGE=="2A" | tabB$AGE=="+2A" | tabB$AGE=="2A?" | tabB$AGE=="+2?"))
  juv_p = length(which(tabB$AGE=="1A?"))
  ad_p =  length(which(tabB$AGE=="+1?"))
  AGE_RATIO = juv / (juv+ad)
  AGE_RATIO_P =  (juv + juv_p) / (ad + juv + ad_p + juv_p)
  AGE_INCERT = (ad_p + juv_p) / (ad + juv + ad_p + juv_p)
  SHANNON = 0
  espece = as.factor(unique(tabB$ESPECE))
  for(sp in 1:length(espece))
  {
    f = nrow(tabB[which(tabB$ESPECE==espece[sp]),])/nrow(tabB)
    SHANNON = SHANNON - f*log(f)
  }
  ligne = data.frame(DATE,NB_BAGUAGE,NB_ESPECE,SEX_RATIO,SEX_RATIO_P,
    SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,
    SHANNON)
  return(ligne)
}

# les colonnes du tableau de sortie :
# ESPECE, NB_BAGUAGE, SEX_RATIO,SEX_RATIO_P, SEX_INCERT, AGE_RATIO, AGE_RATIO_P, AGE_INCERT, NB_BAGUAGE_RELATIF, MASSE
croiseGlobal = function(tab)
{
  tabB = tab[which(tab$ACT=="B"),]
  espece = as.factor(levels(tab$ESPECE))
  ESPECE = "BILAN"
  NB_BAGUAGE = 0
  SEX_RATIO = 0
  AGE_RATIO = 0
  SEX_RATIO_P = 0
  AGE_RATIO_P = 0
  SEX_INCERT = 0
  AGE_INCERT = 0
  NB_BAGUAGE_RELATIF = 0
  MASSE = 0
  ligne = data.frame(ESPECE,NB_BAGUAGE,SEX_RATIO,SEX_RATIO_P,
    SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,NB_BAGUAGE_RELATIF,MASSE)
  tab = ligne
  for (sp in 1:length(espece))
  {
#browser()
    tabBsp = tabB[which(tabB$ESPECE==espece[sp]),]
    ESPECE = paste(espece[sp])
    NB_BAGUAGE = nrow(tabBsp)
    male = length(which(tabBsp$SEXE=="M"))
    femelle = length(which(tabBsp$SEXE=="F"))
    male_p = length(which(tabBsp$SEXE=="M?"))
    femelle_p = length(which(tabBsp$SEXE=="F?"))
    SEX_RATIO = femelle /(male+femelle)
    SEX_RATIO_P = (femelle_p + femelle)  / (male + male_p + femelle + femelle_p)
    SEX_INCERT = (male_p + femelle_p) / (male + male_p + femelle + femelle_p)
    juv = length(which(tabBsp$AGE=="1A" | tabBsp$AGE=="PUL"))
    ad = length(which(tabBsp$AGE=="+1A" | tabBsp$AGE=="2A" |
      tabBsp$AGE=="+2A" | tabBsp$AGE=="2A?" | tabBsp$AGE=="+2?"))
    juv_p = length(which(tabBsp$AGE=="1A?"))
    ad_p =  length(which(tabBsp$AGE=="+1?"))
    AGE_RATIO = juv / (juv+ad)
    AGE_RATIO_P =  (juv + juv_p) / (ad + juv + ad_p + juv_p)
    AGE_INCERT = (ad_p + juv_p) / (ad + juv + ad_p + juv_p)
    NB_BAGUAGE_RELATIF = nrow(tabBsp) / nrow(tabB)
    MASSE = mean(tabBsp$MA,na.rm=TRUE)
    ligne = data.frame(ESPECE,NB_BAGUAGE,SEX_RATIO,SEX_RATIO_P,
      SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,NB_BAGUAGE_RELATIF,MASSE)
    tab = rbind(tab,ligne)
  }
  tab[1,2] = sum(tab$NB_BAGUAGE,na.rm=TRUE)
  tab[1,3] = mean(tab$SEX_RATIO,na.rm=TRUE)
  tab[1,4] = mean(tab$SEX_RATIO_P,na.rm=TRUE)
  tab[1,5] = mean(tab$SEX_INCERT,na.rm=TRUE)
  tab[1,6] = mean(tab$AGE_RATIO,na.rm=TRUE)
  tab[1,7] = mean(tab$AGE_RATIO_P,na.rm=TRUE)
  tab[1,8] = mean(tab$AGE_INCERT,na.rm=TRUE)
  tab[1,9] = sum(tab$NB_BAGUAGE_RELATIF,na.rm=TRUE)
  tab[1,10] = mean(tab$MASSE,na.rm=TRUE)
  tab = tab[order(tab$NB_BAGUAGE,decreasing=TRUE),]
  return(tab)
}


hist.masse = function(unVecteur,nomSp,fichier=TRUE,affichage=TRUE)
{
# histo de la distribution de la MASSE
#	browser()
  if (affichage)
	{
  	hist(unVecteur,xlab= "MASSE", ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
	}
	if (fichier)
	{
		fileName = paste("D:/Nature/STOC cojoux/AnalyseDonnées/distribMASSE/",
      nomSp,'.png',sep='')
		cat("  <-- ",fileName,"\n")
		png(file=fileName)
		hist(unVecteur,xlab= "MASSE", ylab="effectif",
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
  table.summary =  hist.masse(tableauSp$MA,sp)
  for ( i in 2:length(lesEspeces))
  {
    sp = lesEspeces[i]
    tableauSp = tableau[which(tableau$ESPECE == sp),]
    table.summary = rbind(table.summary, hist.masse(tableauSp$MA,sp))
  }

    write.table(table.summary , "D:/Nature/STOC cojoux/AnalyseDonnées/distribMASSE/tableSummaryAile.txt")

}

distibEsp = function(tableau,sp)
{
  hist.masse(tableau$MA,sp)
}



normalisationToutsp = function(tableau)
{
 lesEspeces = unique(tableau$ESPECE)
  i = 1
  sp = lesEspeces[i]
  table.normal = normalisation.sp(tableau,sp)
  for (i in 2:length(lesEspeces))
  {
    sp = lesEspeces[i]
    table.normal = rbind(table.normal,normalisation.sp(tableau,sp))
  }
  return(table.normal)
}



normalisation.sp = function(tableau,sp)
{
   tableauSp = tableau[which(tableau$ESPECE == sp),]
   MAnorme = scale(tableauSp$MA, center=TRUE, scale=TRUE)
   frame.sp = data.frame(tableauSp,MAnorme)
   return(frame.sp)
}


# les colonnes du tableau de sortie :
# ID_SESSION,SESSION,DATE,NB_BAGUAGE,NB_ESPECE,SEX_RATIO,SEX_RATIO_P,SEX_INCERT, AGE_RATIO,AGE_RATIO_P,AGE_INCERT,MASSE,SHANNON
croiseSession = function(tab,tab.sp)
{
  tabB = tab[which(tab$ACT=="B"),]
  session = unique(tablo$REF_SESSION) 
  ID_SESSION = 0
  SESSION = 0 
  DATE = "DATE"
  NB_BAGUAGE = 0
  NB_ESPECE = 0
  SEX_RATIO = 0
  AGE_RATIO = 0
  SEX_RATIO_P = 0
  AGE_RATIO_P = 0
  SEX_INCERT = 0
  AGE_INCERT = 0
  NB_BAGUAGE_RELATIF = 0
  MASSE = 0
  SHANNON = 0
  ligne = data.frame(SESSION,ID_SESSION,DATE,NB_BAGUAGE,NB_ESPECE,SEX_RATIO,SEX_RATIO_P,
    SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,
    NB_BAGUAGE_RELATIF,MASSE,SHANNON)
  tab = ligne
  for (ses in 1:length(session))
  {
    tabBs = tabB[which(tabB$REF_SESSION==session[ses]),]
    SESSION = ses
    ID_SESSION = tabBs$REF_SESSION[1]
    DATE = tabBs$DATE[1]
    NB_BAGUAGE = nrow(tabBs)
    NB_ESPECE = length(unique(tabBs$ESPECE))
    male = length(which(tabBs$SEXE=="M"))
    femelle = length(which(tabBs$SEXE=="F"))
    male_p = length(which(tabBs$SEXE=="M?"))
    femelle_p = length(which(tabBs$SEXE=="F?"))
    SEX_RATIO = femelle /(male+femelle)
    SEX_RATIO_P = (femelle_p + femelle)  / (male + male_p + femelle + femelle_p)
    SEX_INCERT = (male_p + femelle_p) / (male + male_p + femelle + femelle_p)
    juv = length(which(tabBs$AGE=="1A" | tabBs$AGE=="PUL"))
    ad = length(which(tabBs$AGE=="+1A" | tabBs$AGE=="2A" | tabBs$AGE=="+2A" | tabBs$AGE=="2A?" | tabBs$AGE=="+2?"))
    juv_p = length(which(tabBs$AGE=="1A?"))
    ad_p =  length(which(tabBs$AGE=="+1?"))
    AGE_RATIO = juv / (juv+ad)
    AGE_RATIO_P =  (juv + juv_p) / (ad + juv + ad_p + juv_p)
    AGE_INCERT = (ad_p + juv_p) / (ad + juv + ad_p + juv_p)
    NB_BAGUAGE_RELATIF = nrow(tabBs) / nrow(tabB)
    MASSE = mean(tabBs$MAnorme,na.rm=TRUE)

    SHANNON = 0
    espece = as.factor(unique(tabBs$ESPECE))
    for(sp in 1:length(espece))
    {
      f = nrow(tabBs[which(tabBs$ESPECE==espece[sp]),])/nrow(tabBs)
      SHANNON = SHANNON - f*log(f)
    }

    ligne = data.frame(SESSION,ID_SESSION,DATE,NB_BAGUAGE,NB_ESPECE,
      SEX_RATIO,SEX_RATIO_P, SEX_INCERT,
      AGE_RATIO,AGE_RATIO_P,AGE_INCERT,
      NB_BAGUAGE_RELATIF,MASSE,SHANNON)
    tab = rbind(tab,ligne)
  }
  tab[1,4] = mean(tab$NB_BAGUAGE,na.rm=TRUE)
  tab[1,5] = length(unique(tabB$ESPECE))
  tab[1,6] = mean(tab$SEX_RATIO,na.rm=TRUE)
  tab[1,7] = mean(tab$SEX_RATIO_P,na.rm=TRUE)
  tab[1,8] = mean(tab$SEX_INCERT,na.rm=TRUE)
  tab[1,9] = mean(tab$AGE_RATIO,na.rm=TRUE)
  tab[1,10] = mean(tab$AGE_RATIO_P,na.rm=TRUE)
  tab[1,11] = mean(tab$AGE_INCERT,na.rm=TRUE)
  tab[1,12] = mean(tab$NB_BAGUAGE_RELATIF)
  tab[1,13] = mean(tab$MASSE,na.rm=TRUE)

  SHANNON = 0
  espece = as.factor(unique(tabB$ESPECE))
  for(sp in 1:length(espece))
    {
      f = nrow(tabB[which(tabB$ESPECE==espece[sp]),])/nrow(tabB)
      SHANNON = SHANNON - f*log(f)
    }
  tab[1,14] = SHANNON

  tab = tab[order(tab$SESSION,decreasing=FALSE),]
  return(tab)
}

# ID_FILET,NB_BAGUAGE,NB_ESPECE,SEX_RATIO,SEX_RATIO_P,SEX_INCERT, AGE_RATIO,AGE_RATIO_P,AGE_INCERT,NB_BAGUAGE_RELATIF,MASSE,SHANNON,
croiseFilet= function(tablo)
{
  filetLong = c(12,12,12,12,12,7,7,12,7,12,7,12)
  tabB = tablo[which(tablo$ACT=="B"),]
  filet = unique(tablo$NF) 
  nf = 0
  ID_FILET = 0
  LONGUEUR = 0
  NB_BAGUAGE = 0
  NB_ESPECE = 0
  SEX_RATIO = 0
  AGE_RATIO = 0
  SEX_RATIO_P = 0
  AGE_RATIO_P = 0
  SEX_INCERT = 0
  AGE_INCERT = 0
  NB_BAGUAGE_RELATIF = 0
  NB_BAGUAGE_METRE = 0
  MASSE = 0
  SHANNON = 0
  SHANNON_CORRIGE = 0
  PCAPTURE = 0
  ligne = data.frame(
    ID_FILET,LONGUEUR,NB_BAGUAGE,NB_ESPECE,
    SEX_RATIO,SEX_RATIO_P,SEX_INCERT,
    AGE_RATIO,AGE_RATIO_P,AGE_INCERT,
    NB_BAGUAGE_RELATIF,NB_BAGUAGE_METRE, MASSE,SHANNON,SHANNON_CORRIGE)
  tab = ligne
  for (nf in 1:length(filet))
  {
    tabBs = tabB[which(tabB$NF==nf),]
    ID_FILET = nf
    NB_BAGUAGE = nrow(tabBs)
    NB_ESPECE = length(unique(tabBs$ESPECE))
    male = length(which(tabBs$SEXE=="M"))
    femelle = length(which(tabBs$SEXE=="F"))
    male_p = length(which(tabBs$SEXE=="M?"))
    femelle_p = length(which(tabBs$SEXE=="F?"))
    SEX_RATIO = femelle /(male+femelle)
    SEX_RATIO_P = (femelle_p + femelle)  / (male + male_p + femelle + femelle_p)
    SEX_INCERT = (male_p + femelle_p) / (male + male_p + femelle + femelle_p)
    juv = length(which(tabBs$AGE=="1A" | tabBs$AGE=="PUL"))
    ad = length(which(tabBs$AGE=="+1A" | tabBs$AGE=="2A" | tabBs$AGE=="+2A" | tabBs$AGE=="2A?" | tabBs$AGE=="+2?"))
    juv_p = length(which(tabBs$AGE=="1A?"))
    ad_p =  length(which(tabBs$AGE=="+1?"))
    AGE_RATIO = juv / (juv+ad)
    AGE_RATIO_P =  (juv + juv_p) / (ad + juv + ad_p + juv_p)
    AGE_INCERT = (ad_p + juv_p) / (ad + juv + ad_p + juv_p)
    NB_BAGUAGE_RELATIF = nrow(tabBs) / nrow(tabB)
    MASSE = mean(tabBs$MAnorme,na.rm=TRUE)

    SHANNON = 0
    SHANNON_CORRIGE = 0
    espece = as.factor(unique(tabBs$ESPECE))
    for(sp in 1:length(espece))
    {
      f = nrow(tabBs[which(tabBs$ESPECE==espece[sp]),])/nrow(tabBs)
      SHANNON = SHANNON - f*log(f)
      facteurCor = 12/filetLong[nf]
      fcor = (nrow(tabBs[which(tabBs$ESPECE==espece[sp]),])*facteurCor)/(nrow(tabBs)*facteurCor)
      SHANNON_CORRIGE = SHANNON_CORRIGE - fcor*log(fcor)
    }
    ligne = data.frame(
      ID_FILET,LONGUEUR,NB_BAGUAGE,NB_ESPECE,
      SEX_RATIO,SEX_RATIO_P,SEX_INCERT,
      AGE_RATIO,AGE_RATIO_P,AGE_INCERT,
      NB_BAGUAGE_RELATIF,NB_BAGUAGE_METRE,MASSE,SHANNON,SHANNON_CORRIGE)
    tab = rbind(tab,ligne)
  }
  tab[1,3] = length(filet)
  tab[1,2] = sum(filetLong)
  tab[1,3] = sum(tab$NB_BAGUAGE,na.rm=TRUE)
  tab[1,4] = length(unique(tabB$ESPECE))
  tab[1,5] = mean(tab$SEX_RATIO,na.rm=TRUE)
  tab[1,6] = mean(tab$SEX_RATIO_P,na.rm=TRUE)
  tab[1,7] = mean(tab$SEX_INCERT,na.rm=TRUE)
  tab[1,8] = mean(tab$AGE_RATIO,na.rm=TRUE)
  tab[1,9] = mean(tab$AGE_RATIO_P,na.rm=TRUE)
  tab[1,10] = mean(tab$AGE_INCERT,na.rm=TRUE)
  tab[1,11] = sum(tab$NB_BAGUAGE_RELATIF)
  tab[1,12] = sum(tab$NB_BAGUAGE_METRE)
  tab[1,13] = mean(tab$MASSE,na.rm=TRUE)

  SHANNON = 0
  espece = as.factor(unique(tabB$ESPECE))
  for(sp in 1:length(espece))
    {
      f = nrow(tabB[which(tabB$ESPECE==espece[sp]),])/nrow(tabB)
      SHANNON = SHANNON - f*log(f)
    }
  tab[1,14] = SHANNON
  tab = tab[order(tab$ID_FILET,decreasing=FALSE),]
  return(tab)
  
}


ecirtureTable = function(tabl,variableCroi,annee)
{
		dossier = paste("D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/",annee,sep='/')
  prefixe = paste(dossier,annee,sep='')
  fichier = paste(prefixe,"_",variarbleCroi,".txt",sep='')
  write.table(tabl,fichier)
  cat("  <-- TXT : ",fichier,"\n")
  flush.console()
}

#filet.table2 = filet.table[-1,]
#variableCroi = "Filets"
#i = 2
#titre = paste(colnames(filet.table2)[i],"par",variableCroi) 
#tab.plot = t(as.matrix(filet.table2[,i]))
#colnames(tab.plot) = 1:ncol(tab.plot) 
# 
#barplot((tab.plot,main=titre,xlab=variableCroi,col="lightgreen")
#
#



bilanFiletSession = function(db,an,mybarcol = "lightgreen", myPointCol="darkgreen" ,titre="")
{
  
  # analyse en terme de catpure par filets 
  
  filetLong = c(12,12,12,12,12,7,7,12,7,12,7,12)
  SQL = paste("SELECT capture.NF, Count(capture.ACT) as 'capture', capture.ID_SESSION FROM capture Inner Join session ON capture.ID_SESSION = session.REF_SESSION WHERE year(session.DATE_SESSION) =  '",an,"' AND session.ID_PROGRAMME =  '3' GROUP BY capture.NF, capture.ID_SESSION ",sep="")
 
  cat("sql: ",SQL,"\n")
  flush.console()
   # browser()
  tab = sqlQuery(db, SQL)
  for(f in unique(tab$NF))
  {
#    browser()
    nbObs = nrow(tab[which(tab$NF == f),])
    if(nbObs<4)
      {
        sessionTout = unique(tab$ID_SESSION)
        sessionNF = unique(tab$ID_SESSION[which(tab$NF==f)])
        sessionAdd = sessionTout[-which(sessionTout %in% sessionNF)]
        tabAdd = cbind(rep(f,4-nbObs),rep(0,4-nbObs),sessionAdd)
        colnames(tabAdd) = colnames(tab)
        tab = rbind(tab,tabAdd)
      } 
  }
  longueurFilet = vector()
  for(i in 1:nrow(tab))
  {
    longueurFilet = c(longueurFilet,filetLong[tab$NF[i]])
  }

  tab = cbind(tab,longueurFilet)
  
  captPm = tab$capture / longueurFilet
  #browser()
  tab = cbind(tab,captPm)
  vecMoyenne = as.vector(by(tab$capture,tab$NF,mean))
  #print(by(tab$capture,tab$NF,mean))
  vecSD = as.vector(by(tab$capture,tab$NF,sd))
  #print(by(tab$capture,tab$NF,sd))
  testDiff  = rep(NA,length(unique(tab$NF)))

  transfo = asin(sqrt(tab$captPm))#normalisation des données proportions

  model = glm(capture~factor(NF)+factor(ID_SESSION)+longueurFilet,data = tab,family=distribution)
  cat("-------------------------\n")
  print(summary(model))
  cat("-------------------------\n")
  print(anova(model,test="Chisq"))
  cat("=========================\n")

  vecA = as.vector(summary(model)$coefficients[,4])
  vecA[1] = 1
  vecA = vecA[1:length(filetLong)]
  testDiff = vecA<0.05
  print(testDiff)
  
  vec95sup =  vecMoyenne + (vecSD/2)
  vec95inf =  vecMoyenne - (vecSD/2)
  
  if(any(testDiff)) limiteY = c(0,max(c(vec95sup*testDiff+(0.8 * testDiff),vec95sup)))
  else  limiteY = c(0,max(vec95sup))

  names(vecMoyenne) = 1:length(vecMoyenne)
 #
  titre = paste("Nombre de capture\n par filet\nannée : ",an,sep="")
  mp <- barplot(vecMoyenne, beside = TRUE, col = mybarcol, main = titre, 
        border = mybarcol,
        cex.names = 0.8,
        space = 0.5,
        ylim=limiteY)
  arrows(mp, vec95sup, mp, vec95inf, col = myPointCol, lwd = 1.5, code=3,angle=90,length=0.05)
 
  if (any(testDiff)) 
  {
    test  = which(testDiff==TRUE)
    points(mp[test],(vec95sup[test]+0.5),pch=8,cex=2,col=myPointCol)
  }

  fichierPlot = paste("D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/barplotFillet",an,sep="")
  savePlot(fichierPlot,"tiff")
  
  
  
  vecMoyenne = as.vector(by(tab$captPm,tab$NF,mean))
  #print(by(tab$capture,tab$NF,mean))
  vecSD = as.vector(by(tab$captPm,tab$NF,sd))
  #print(by(tab$capture,tab$NF,sd))
  testDiff  = rep(NA,length(unique(tab$NF)))

  transfo = asin(sqrt(tab$captPm))#normalisation des données proportions

  model = glm(transfo~factor(NF)+factor(ID_SESSION),data = tab)
  cat("-------------------------\n")
  print(anova(model))
  cat("-------------------------\n")
  print(summary(model))
  cat("=========================\n")

  vecA = as.vector(summary(model)$coefficients[,4])
  vecA[1] = 1
  vecA = vecA[1:length(filetLong)]
  testDiff = vecA<0.05
  print(testDiff) 
 
  
  vec95sup =  vecMoyenne + (vecSD/2)
  vec95inf =  vecMoyenne - (vecSD/2)
  
  if(any(testDiff)) limiteY = c(0,max(c(vec95sup*testDiff+(0.2 * testDiff),vec95sup)))
  else  limiteY = c(0,max(vec95sup))

  names(vecMoyenne) = 1:length(vecMoyenne)
 #
  titre = paste("Nombre de capture\n par mètre de filet\nannée : ",an,sep="")  
  mp <- barplot(vecMoyenne, beside = TRUE, col = mybarcol, main = titre, 
        border = mybarcol,
        cex.names = 0.8,
        space = 0.5,
        ylim=limiteY)
  arrows(mp, vec95sup, mp, vec95inf, col = myPointCol, lwd = 1.5, code=3,angle=90,length=0.05)
 
  if (any(testDiff)) 
  {
    test  = which(testDiff==TRUE)
    points(mp[test],(vec95sup[test]+0.1),pch=8,cex=2,col=myPointCol)
  }

  fichierPlot = paste("D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/barplotFilletParMetre",an,sep="")
  savePlot(fichierPlot,"tiff")
  
  
   # analyse en terme de nombre d'espèce catpurées par filets 
  

#  SQL = paste("SELECT capture.NF, Count(capture.ACT) as 'capture', capture.ESPECE, capture.ID_SESSION FROM capture Inner Join session ON capture.ID_SESSION = session.REF_SESSION WHERE year(session.DATE_SESSION) =  '",an,"' AND session.ID_PROGRAMME =  '3' GROUP BY capture.NF, capture.ID_SESSION, capture.ESPECE  ",sep="")
# 
#  cat("sql: ",SQL,"\n")
#  flush.console()
#  #  browser()
#  tabSQL = sqlQuery(db, SQL)
#  
#
#  
#  
#  for(f in unique(tab$NF))
#  {
##    browser()
#    nbObs = nrow(tab[which(tab$NF == f),])
#    if(nbObs<4)
#      {
#        sessionTout = unique(tab$ID_SESSION)
#        sessionNF = unique(tab$ID_SESSION[which(tab$NF==f)])
#        sessionAdd = sessionTout[-which(sessionTout %in% sessionNF)]
#        tabAdd = cbind(rep(f,4-nbObs),rep(0,4-nbObs),sessionAdd)
#        colnames(tabAdd) = colnames(tab)
#        tab = rbind(tab,tabAdd)
#      } 
#  }
#  
  
  
 
  
  
  

}



  

barplot2.croise = function(annee,var1,var2,variableCoi,tabl)
{
		dossier = paste("D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/",annee,sep='/')
    prefixe = paste(dossier,annee,sep='')
 
    
    
		cat("  <-- PNG : ",fileName,"\n")
		png(file=fileName)
		par(mfrow=c(1,2))
    barplot(x1, horiz=TRUE,ylab = var1)
    barplot(x2, horiz=TRUE,ylab = var2)
		hist(unVecteur,xlab= "MASSE", ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
		dev.off()
	#
	flush.console()
}

barplot.croise = function(annee,variable,variableCoi,tabl)
{
		dossier = paste("D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/",annee,sep='/')
    prefixe = paste(dossier,annee,sep='')
    #  nomSp,'.png',sep='')
    
    
		cat("  <-- PNG : ",fileName,"\n")
		png(file=fileName)
		hist(unVecteur,xlab= "MASSE", ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
		dev.off()
	
	flush.console()
}

lesFigures = function(tabl,variableCroi,annee)
{
  if (variableCroi == "sp")
  {
    variableFigure = c
    
  }
  if (variableCroi == "session")
  {
  
  }
  if (variableCroi == " filet")
  {
  
  }
}

analyseSTOC = function(annee)
{
  tablo = lectureFichier(annee)
  tabloN = normalisationToutsp(tablo)
  tablo.sp = croiseGlobal(tablo)
  #distribEsp.tout(tablo)
  
  bilan.table = bilanGlobal(tablo,annee)
  sp.table = croiseGlobal(tablo)
  session.table = croiseSession(tabloN)
  filet.table = croiseFilet(tabloN)
  
  ecritureTable(bilan.table,"bilan",annee)
  ecritureTable(sp.table,"sp",annee)
  ecritureTable(session.table,"session",annee)
  ecritureTable(filet.table,"filet",annee)
  
  dir.create(paste("D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/",annee,sep="/"),sowWarnings=FALSE)
  
  lesFigures(sp.table,"sp",annee)
  lesFigures(session.table,"session",annee)
  lesFigures(filet.table,"filet",annee)
}




####################################
# Graphe
###################################



barplot_t.test = function(repertoire,an,variable="NB_ESPECE",variableCroise ="session" , mybarcol = "lightgreen", myPointCol="darkgreen" ,titre="",testSignif = TRUE)
{
 
  fichier = paste(repertoire,an,"_",variableCroise,".txt",sep="")
  tab = read.table(fichier)
  tab = tab[-1,]
  #browser()
  #attach(tab)
  vec = tab[,which(colnames(tab)==variable)]
  testDiff  = rep(NA,length(vec))
  for(i in 1:length(vec))
  {
  #browser()
    sousVec = vec[-i]
    valeur = vec[i]
    testDiff[i] = t.test(sousVec,mu=valeur)$p.value<0.05
  } 
  if (variable == "NB_ESPECE" ) titre = paste("Nombre d'espèces\ncapturés par session\nannée : ",an,sep="")
  if (variable == "NB_BAGUAGE" )  titre = paste("Nombre d'individus\ncapturés par session\nannée : ",an,sep="")
  if (variable == "AGE_RATIO" )  titre = paste("Age ratio\n par session\nannée : ",an,sep="")
  
  names(vec) = 1:length(vec)
  b = barplot(vec,
  ,beside = TRUE, col = mybarcol,  main = titre, 
        border = mybarcol,
        cex.names = 0.8,
        space = 0.5,
        ylim=c(0,(max(vec)+0.5)))

  if (testSignif) 
  {
    test  = which(testDiff==TRUE)
    points(b[test],(vec[test]+0.5),pch=8,cex=2,col=myPointCol)
  }
  fichierPlot = paste(repertoire,"barplot",variableCroise,variable,an,sep="")
  savePlot(fichierPlot,"png")
 # print(vec)
 # print(testDiff)

}


pie.sp = function(repertoire = "D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/",an,variable="NB_BAGUAGE",variableCroise ="sp", titre="Proportion des captures")
{

  nomFichier = paste(repertoire,an,"_",variableCroise,".txt",sep="")
  tab = read.table(nomFichier,header=TRUE,stringsAsFactors=FALSE)
  tab = tab[-1,]
  require(graphics)
  colorSp = rainbow(nrow(tab))
   vec = tab[,which(colnames(tab)==variable)]
#browser()
#print(tabAn[1:nb,1])
  #print(colorSp)
  
  labelSp = tab$ESPECE
  pie(vec, col=colorSp,labels = labelSp,cex=0.8,radius=1,main=titre,border=TRUE)  
  fichierPlot = paste(repertoire,"pie",variableCroise,variable,an,sep="")
  savePlot(fichierPlot,"png")



}

#---------------------------------------
#     MAIN
#---------------------------------------

# librairie
library(RODBC)
library(stats)

# variable globale
repertoire = "D:/Nature/STOC cojoux/AnalyseDonnées/Rapport/"
an = 2008
nomBase = "baguage"

# ouverture de la base de donnée
db = ouvrirConnection(nomBase)


  
#graphSession(repertoire,an,variable="NB_BAGUAGE")
#graphSession(repertoire,an)
#graphSession(repertoire,an,variable="AGE_RATIO")
#pie.sp(an = 2008)
barplot_t.test(repertoire,an)
barplot_t.test(repertoire,an,variable = "NB_BAGUAGE")
#barplot_t.test(repertoire,an,variable = "AGE_RATIO")
bilanFiletSession(db,an)
# fermeture de la base
femerConnection(db)
session_modif = ifelse((tab$ID_SESSION==13 | tab$ID_SESSION==16),2,1)
tab = cbind(tab,session_modif)

cat("#####################################\n")

distribution = "poisson"

#model = glm(capture~factor(NF)+factor(session_modif)+longueurFilet,data = tab,family=distribution)
#print(summary(model))
#cat("-------------------------\n")
#print(anova(model,test="Chisq"))
#cat("=========================\n")
#
#model = glm(capture~longueurFilet,data = tab,family=distribution)
#print(summary(model))
#cat("-------------------------\n")
#print(anova(model,test="Chisq"))
#cat("=========================\n")
#
#model = glm(capture~factor(NF)+factor(session_modif),data = tab,family=distribution)
#print(summary(model))
#cat("-------------------------\n")
#print(anova(model,test="Chisq"))
#cat("=========================\n")
#
#model = glm(capture~factor(session_modif),data = tab,family=distribution)
#print(summary(model))
#cat("-------------------------\n")
#print(anova(model,test="Chisq"))
#cat("=========================\n")
#
#model = glm(capture~factor(NF),data = tab,family=distribution)
#print(summary(model))
#cat("-------------------------\n")
#print(anova(model,test="Chisq"))
#cat("=========================\n")
#
#
#
#
#
#model = glm(transfo~factor(NF)+factor(session_modif)+longueurFilet,data = tab)
#print(summary(model))
#cat("-------------------------\n")
#print(anova(model))
#cat("=========================\n")
#
#
#
#
#
#model = glm(transfo~factor(NF),data = tab)
#print(anova(model))
#cat("-------------------------\n")
#print(summary(model))
#cat("=========================\n")
#

