#-------------------------
# Analyse par connection avec ODBC (base MySQL)
#---------------------------


# http://www.mnhn.fr/semin-r/



# connection a la base de donnees
ouvrirConnection = function(nom_base)
  return(odbcConnect(nom_base))  

femerConnection = function(connexion)
  close(connexion)

# quelques fonction

#dataD <- sqlFetch (db,"basecomplete")


#sql = paste("SELECT ... ;",sep="")
#tab = sqlQuery(db, sql)


 
# verification de la base de donnees de Donges


analyseLP_MA = function(db)
{
  sqlSP = paste("SELECT hubert.ESPECE FROM hubert GROUP BY hubert.ESPECE;")
 	#cat("SQL: ",sqlSP,"\n")
  tabSP = sqlQuery(db,sqlSP)
  for (sp in 1:nrow(tabSP))
  {
    nomsp = tabSP[sp,1]
    sql1 = paste("SELECT basecomplete.ESPECE, basecomplete.LP, basecomplete.MA FROM basecomplete WHERE basecomplete.ESPECE = '",nomsp,"'",sep="")
    #cat("SQL: ",sql1,"\n")
    tab = sqlQuery(db, sql1)
    hist.verif(tab$LP,"LP",nomsp)
    hist.verif(tab$MA,"MA",nomsp)
  }
}  

# histo de distribution 
hist.verif = function(unVecteur,nomVariable,nomSp,fichier=TRUE,affichage=FALSE)
{
  if (affichage)
	{
		hist(unVecteur,xlab= nomVariable, ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
	}
	if (fichier)
	{
    repName = paste("D:/Recherche/Donges/verif",nomVariable,sep="")
    dir.create(repName)
		fileName = paste(repName,"/",
      nomVariable,"-",nomSp,'.png',sep='')
		cat("  <-- ",fileName,"\n")
		png(file=fileName)
		hist(unVecteur,xlab= nomVariable, ylab="effectif",
      font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		  main= nomSp,font.main = 2, cex.main= 2)
		dev.off()
	}
	flush.console()
}

#--------------------------
#       Bilan
#--------------------------


tableauRapport.GlobalMultiAn = function(tab)
{
  tab = tab[,c(1:5,7)]
  colnames(tab) = c("Année","Nombre de jours de captures","Nombre de baguages","Nombre de contrôles","Nombre d'individus","Nombre d'espèces")
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobalRAPPORT.csv",sep="")
  write.csv(tab,nomFichier,row.names=FALSE)
  cat("  <-- ",nomFichier,"\n")
  flush.console()

}  



bilanGlobalMultiAn = function(db)
{

  SQL = paste("SELECT annee.ANNEE FROM annee WHERE annee.ANNEE BETWEEN  2003 AND 2007")
  cat("sql: ",SQL,"\n")
  flush.console()
  an = sqlQuery(db,SQL)
  #  browser()
  tableau = data.frame(
                      "annees" = c(as.vector(an)), 
                      "effort_jour" = rep(0,nrow(an)), 
                      "baguages" = rep(0,nrow(an)),
                      "controles" = rep(0,nrow(an)))
  #print(dim(tableau))

  SQL = paste("SELECT Count(hubert.ACT) AS baguages, hubert.ANNEE FROM hubert WHERE hubert.ACT =  'B'  AND  hubert.ERREUR < 2 AND hubert.ANNEE BETWEEN  2003 AND 2007 GROUP BY hubert.ANNEE",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()
  tab = sqlQuery(db, SQL)
  tableau$baguages = tab$baguages
  #
  SQL = paste("SELECT Count(hubert.ACT) AS controles, hubert.ANNEE FROM hubert WHERE hubert.ACT =  'C'  AND  hubert.ERREUR < 2  AND hubert.ANNEE BETWEEN  2003 AND 2007 GROUP BY hubert.ANNEE",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()

  tab = sqlQuery(db, SQL)
  tableau$controles = tab$controles
  
  SQL = paste("SELECT annee.EFFORT_NBJOUR FROM annee WHERE annee.ANNEE BETWEEN  2003 AND 2007")
  cat("sql: ",SQL,"\n")
  flush.console()
  tab = sqlQuery(db, SQL)
  tableau$effort_jour = tab$EFFORT_NBJOUR
#browser()
   total = c("TOTAL",apply(tableau[,2:4],2,sum))
   tableau = rbind(tableau,total)
 # for(a in (1:nrow(tableau)))
 #{
 #    SQL = paste("SELECT hubert.ESPECE FROM hubert WHERE hubert.ANNEE = '",tableau$ANNEE[a],"' AND  hubert.ERREUR < 2  GROUP BY hubert.ESPECE ",sep="")
 #    cat("sql: ",SQL,"\n")
 #    flush.console()
 #    tab = sqlQuery(db, SQL)
 #    tableau$nombre_especes[a] = nrow(tab)
 # }
 
   for(a in (1:nrow(an)))
  {
      if (a == 1)tablBilan = bilanAn(db,tableau$ANNEE[a])
      else  tablBilan = rbind(tablBilan,bilanAn(db,tableau$ANNEE[a]))
  }
   #browser()
  tablBilan = rbind(tablBilan,bilanAn(db))
  tableau = cbind(tableau,tablBilan)
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobal.csv",sep="")
 # print(tableau)
  write.csv(tableau,nomFichier,row.names=FALSE)
  cat("  <-- ",nomFichier,"\n")
  flush.console()
  tableauRapport.GlobalMultiAn(tableau)
}




ajustAge = function(tab,an)
{
  # browser()
   ageAjust = tab$AGE
   tab = cbind(tab,ageAjust)
   tab$ageAjust[which(tab$ANNEE.1 < an)] = "+1A"
   tab$ageAjust[which(tab$ageAjust == "1a?" )] = "1A?"
   tab$ageAjust[which(tab$ageAjust == "1a" |tab$ageAjust == "pul" )] = "1A"
   return(tab)
}  
  
  
bilanAn = function(db,an=0)
{
  if(an == 0) SQL = paste("SELECT hubert.CENTRE, hubert.BAGUE, hubert.ACT, hubert.ANNEE, hubert.SITE, oiseaubague.ANNEE,oiseaubague.ESPECE, oiseaubague.SEXE, oiseaubague.AGE, oiseaubague.ACT FROM hubert Inner Join oiseaubague ON hubert.BAGUE = oiseaubague.BAGUE WHERE hubert.ANNEE  BETWEEN  2003 AND 2007 AND  hubert.ERREUR < 2 GROUP BY hubert.BAGUE ",sep="")
  else SQL = paste("SELECT hubert.CENTRE, hubert.BAGUE, hubert.ACT, hubert.ANNEE, hubert.SITE, oiseaubague.ANNEE, oiseaubague.ESPECE,oiseaubague.SEXE, oiseaubague.AGE, oiseaubague.ACT FROM hubert Inner Join oiseaubague ON hubert.BAGUE = oiseaubague.BAGUE  WHERE hubert.ANNEE =", an," AND hubert.ERREUR < 2 GROUP BY hubert.BAGUE ",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tab = sqlQuery(db, SQL)
  tab$AGE = toupper(tab$AGE)
  tab$SEXE = toupper(tab$SEXE)
  NB_INDIVIDUS = nrow(tab)
  NB_INDIVIDUS_PROTOCOLE = nrow(subset(tab,(is.na(tab$SITE)| tab$SITE == "" | tab$SITE == "B" )))
  NB_ESPECES = length(unique(tab$ESPECE))
  male = length(which(tab$SEXE=="M"))
  femelle = length(which(tab$SEXE=="F"))
  male_p = length(which(tab$SEXE=="M?"))
  femelle_p = length(which(tab$SEXE=="F?"))
  if ((femelle + male) != 0) SEX_RATIO = femelle /(femelle + male)
  else SEX_RATIO = NA
  if ((femelle + femelle_p + male + male_p) != 0) SEX_RATIO_P = (femelle + femelle_p)  / (femelle + femelle_p + male + male_p)
  else SEX_RATIO_P = NA 
  if ((male_p + femelle_p + male + femelle) != 0) SEX_INCERT = (male_p + femelle_p) / (male_p + femelle_p + male + femelle)
  else SEX_INCERT = NA
  tab = ajustAge(tab,an)
#browser()
  juv = length(which(tab$ageAjust=="1A" | tab$ageAjust=="PUL"))
  ad = length(which(tab$ageAjust=="+1A" | tab$ageAjust=="2A" | tab$ageAjust=="+2A" | tab$ageAjust=="2A?" | tab$ageAjust=="+2?"))
  juv_p = length(which(tab$ageAjust=="1A?"))
  ad_p =  length(which(tab$ageAjust=="+1?"))
  if ((ad + juv)!=0) AGE_RATIO = juv / (ad + juv) 
  else AGE_RATIO = NA
  if ((juv + juv_p + ad + ad_p) != 0) AGE_RATIO_P =  (juv + juv_p) / (juv + juv_p + ad + ad_p)
  else AGE_RATIO_P = NA
  if ((ad + juv + ad_p + juv_p) != 0) AGE_INCERT = (ad_p + juv_p) / (ad + juv + ad_p + juv_p)
  else AGE_INCERT = NA
  SHANNON = 0
  espece = as.factor(unique(tab$ESPECE))
  for(sp in 1:length(espece))
  {
    f = nrow(tab[which(tab$ESPECE==espece[sp]),])/nrow(tab)
    SHANNON = SHANNON - f*log(f)
  }
  ligne = data.frame(NB_INDIVIDUS, NB_INDIVIDUS_PROTOCOLE,NB_ESPECES,SEX_RATIO,SEX_RATIO_P,
    SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,
    SHANNON)
  
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobal",an,".txt",sep="")
  return(ligne)
}
    



bilanAnSp = function(db,an=0)
{
  if (an == 0)
  {
    SQL = paste("SELECT hubert.CENTRE, hubert.BAGUE, hubert.ACT, hubert.ANNEE, hubert.SITE,hubert.MA, oiseaubague.ANNEE, oiseaubague.ESPECE,oiseaubague.SEXE, oiseaubague.AGE, oiseaubague.ACT, oiseauBague.LOCALITE FROM hubert Inner Join oiseaubague ON hubert.BAGUE = oiseaubague.BAGUE  WHERE hubert.ERREUR < 2  AND hubert.ANNEE BETWEEN  2003 AND 2007 GROUP BY hubert.BAGUE ",sep="")
 #   browser()
  }
  else 
  {
    SQL = paste("SELECT hubert.CENTRE, hubert.BAGUE, hubert.ACT, hubert.ANNEE, hubert.SITE,hubert.MA, oiseaubague.ANNEE, oiseaubague.ESPECE,oiseaubague.SEXE, oiseaubague.AGE, oiseaubague.ACT, oiseauBague.LOCALITE FROM hubert Inner Join oiseaubague ON hubert.BAGUE = oiseaubague.BAGUE  WHERE hubert.ANNEE =", an," AND hubert.ERREUR < 2 GROUP BY hubert.BAGUE ",sep="")                                         
  }

  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tabGlobal = sqlQuery(db, SQL)
  tabGlobal$LOCALITE  == toupper(paste(tabGlobal$LOCALITE))

  tabGlobal$ACT.1  == toupper(paste(tabGlobal$ACT.1))
  tabGlobal$ACT  == toupper(paste(tabGlobal$ACT))
  if (an == 0)
    tabB = tabGlobal[which(tabGlobal$ACT.1=="B" & tabGlobal$LOCALITE  == "DONGES" &  tabGlobal$ANNEE.1 >= 2003 & tabGlobal$ANNEE.1 <= 2007),]  
  else  
    tabB = tabGlobal[which(tabGlobal$ACT.1=="B" & tabGlobal$LOCALITE  == "DONGES" &  tabGlobal$ANNEE.1 ==an),]
#browser()
  espece = as.factor(levels(tabGlobal$ESPECE))
  ESPECE = "BILAN"
  NB_BAGUAGE = 0
  NB_INDIVIDUS = 0
  NB_INDIVIDUS_PROTOCOLE =  0
  SEX_RATIO = 0
  AGE_RATIO = 0
  SEX_RATIO_P = 0
  AGE_RATIO_P = 0
  SEX_INCERT = 0
  AGE_INCERT = 0
  NB_BAGUAGE_RELATIF = 0
  MASSE = 0
  ligne = data.frame(ESPECE,NB_BAGUAGE,NB_INDIVIDUS,NB_INDIVIDUS_PROTOCOLE,SEX_RATIO,SEX_RATIO_P,
    SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,NB_BAGUAGE_RELATIF,MASSE)
  tab = ligne
  for (sp in 1:length(espece))
  {
#browser()

    tabBsp = tabB[which(tabB$ESPECE==espece[sp]),]
    tabSp = tabGlobal[which(tabGlobal$ESPECE==espece[sp]),]
    ESPECE = paste(espece[sp])
    NB_BAGUAGE = nrow(tabBsp)
    NB_INDIVIDUS = nrow(tabSp)
    NB_INDIVIDUS_PROTOCOLE =  nrow(subset(tabSp,(is.na(tabSp$SITE)| tabSp$SITE == "" | tabSp$SITE == "B" )))
    male = length(which(tabSp$SEXE=="M"))
    femelle = length(which(tabSp$SEXE=="F"))
    male_p = length(which(tabSp$SEXE=="M?"))
    femelle_p = length(which(tabSp$SEXE=="F?"))
    if ((femelle + male) != 0) SEX_RATIO = femelle /(femelle + male)
    else SEX_RATIO = NA
    if ((femelle + femelle_p + male + male_p) != 0) SEX_RATIO_P = (femelle + femelle_p)  / (femelle + femelle_p + male + male_p)
    else SEX_RATIO_P = NA 
    if ((male_p + femelle_p + male + femelle) != 0) SEX_INCERT = (male_p + femelle_p) / (male_p + femelle_p + male + femelle)
    else SEX_INCERT = NA
    tabSp = ajustAge(tabSp,an)
    juv = length(which(tabSp$ageAjust=="1A" | tabSp$ageAjust=="PUL"))
    ad = length(which(tabSp$ageAjust=="+1A" | tabSp$ageAjust=="2A" | tabSp$ageAjust=="+2A" | tabSp$ageAjust=="2A?" | tabSp$ageAjust=="+2?"))
    juv_p = length(which(tabSp$ageAjust=="1A?"))
    ad_p =  length(which(tabSp$ageAjust=="+1?"))
    if ((ad + juv)!=0) 
      AGE_RATIO = juv / (ad + juv) 
    else 
      AGE_RATIO = NA
    if ((juv + juv_p + ad + ad_p) != 0) 
      AGE_RATIO_P =  (juv + juv_p) / (juv + juv_p + ad + ad_p)
    else 
      AGE_RATIO_P = NA
    if ((ad + juv + ad_p + juv_p) != 0) 
      AGE_INCERT = (ad_p + juv_p) / (ad + juv + ad_p + juv_p)
    else 
      AGE_INCERT = NA
    NB_BAGUAGE_RELATIF = nrow(tabBsp) / nrow(tabB)
   
#browser()
    MASSE = mean(tabSp$MA,na.rm=TRUE)
    ligne = data.frame(ESPECE,NB_BAGUAGE,NB_INDIVIDUS,NB_INDIVIDUS_PROTOCOLE,SEX_RATIO,SEX_RATIO_P,
      SEX_INCERT,AGE_RATIO,AGE_RATIO_P,AGE_INCERT,NB_BAGUAGE_RELATIF,MASSE)
    tab = rbind(tab,ligne)
  }
 # browser()
  tab[1,2] = sum(tab$NB_BAGUAGE,na.rm=TRUE)
  tab[1,3] = sum(tab$NB_INDIVIDUS,na.rm=TRUE)  
  tab[1,4] = sum(tab$NB_INDIVIDUS_PROTOCOLE,na.rm=TRUE)
  tab[1,5] = mean(tab$SEX_RATIO,na.rm=TRUE)
  tab[1,6] = mean(tab$SEX_RATIO_P,na.rm=TRUE)
  tab[1,7] = mean(tab$SEX_INCERT,na.rm=TRUE)
  tab[1,8] = mean(tab$AGE_RATIO,na.rm=TRUE)
  tab[1,9] = mean(tab$AGE_RATIO_P,na.rm=TRUE)
  tab[1,10] = mean(tab$AGE_INCERT,na.rm=TRUE)
  tab[1,11] = sum(tab$NB_BAGUAGE_RELATIF,na.rm=TRUE)
  tab[1,12] = mean(tab$MASSE,na.rm=TRUE)
  tab = tab[order(tab$NB_BAGUAGE,decreasing=TRUE),]

  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobalSp",an,".csv",sep="")
  #print(tab)
  write.csv(tab,nomFichier,row.names=FALSE)
  cat("  <-- ",nomFichier,"\n")
  flush.console()

  tableauRapport.AnSp(tab,an)

}


tableauRapport.AnSp = function(tab,an)
{
  tab = tab[,c(1,2,3,5,8,11)]
  bilan = tab[1,]
  tab = tab[-1,]
  tab = rbind(tab,bilan)
  tab[,4]= round(tab[,4],3)
  tab[,5]= round(tab[,5],3)
  tab[,6]= round(tab[,6],3)
  colnames(tab) = c("Espèce",	"Nombre baguages",	"Nombre individus",	"Sex-ratio",	"Age-ratio",	"Nombre baguages relatifs")
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobalSp",an,"RAPPORT.csv",sep="")
  write.csv(tab,nomFichier,row.names=FALSE)
  cat("  <-- ",nomFichier,"\n")
  flush.console()

}  


batch.bilanAnSp = function(db,vecAn)
{
  for(a in vecAn)  bilanAnSp(db,a)
  bilanAnSp(db)
}  
  





#--------------------------
#       Graphe
#--------------------------



couleurSp = function(listSp)
{
  require(graphics)
  nbPalu = length(which(listSp[,2])) 
  tabColorPalu = data.frame(espece=listSp[which(listSp[,2]),1],couleur=rep(NA,nbPalu))
#browser()
  tabColorPasPalu = data.frame(espece=listSp[which(listSp[,2]==FALSE),1],couleur=rep(NA,length(which(listSp[,2]==FALSE))))
  tabColorPalu$couleur = rainbow(nbPalu,start = .1, end =.6)
  tabColorPalu$couleur[which(tabColorPalu$espece == "ACROLA")] = "red"
  tabColorPasPalu$couleur = rainbow(nrow(tabColorPasPalu),start = .6, end =.9)
  tabColor = rbind(tabColorPalu,tabColorPasPalu)
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"couleurEspece.csv",sep="")
  write.csv(tabColor,nomFichier)
  return(tabColor)

}



# camenbert des "nb" espèces les plus capturées


pie.annee = function(an,listSpCouleur,nb = 0)
{
  
  #browser()
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  if (an == 0)   nomFichier = paste(repertoire,"bilanGlobalSp0RAPPORT.csv",sep="")
  else nomFichier = paste(repertoire,"bilanGlobalSp",an,"RAPPORT.csv",sep="")
  tabAn = read.csv(nomFichier,stringsAsFactors=FALSE)
  require(graphics)
  colorSp = rep(NA,nb)

#browser()
#print(tabAn[1:nb,1])
  for(i in 1:(nb-1))
  {
    colorSp[i] = listSpCouleur$couleur[which(listSpCouleur$espece == tabAn[i,1])]
  }

  colorSp[nb] = "grey"
  #print(colorSp)
  if (an == 0)titre = paste("Proportion de captures année : 2003-2007",sep="")
  else titre = paste("Proportion de captures année : ",an,sep="")
  labelSp = c(tabAn[1:(nb-1),1],"Autres")
  vecPie = c(tabAn[1:(nb-1),3],tabAn[nrow(tabAn),3]-sum(tabAn[1:(nb-1),3]))
  pie(vecPie, col=colorSp,labels = labelSp,cex=1,radius=1,main=titre,border=FALSE)  
  



}



batch.pie.annee = function(nb=10)
{
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobalSp0RAPPORTsp.csv",sep="")
  bilanTout = read.csv(nomFichier,stringsAsFactors=FALSE)
  listSp = bilanTout[1:20,]
  couleur = couleurSp(listSp)
  annees = 2003:2008
  for (an in annees)
  {
#      print(an)
      pie.annee(an,couleur,nb)
      fichier.plot = paste(repertoire,"PieBilan",an)
      savePlot(fichier.plot)
  }
  pie.annee(an=0,couleur,nb)
  fichier.plot = paste(repertoire,"PieBilan")
  savePlot(fichier.plot)

  
}


# camenbert des "nb" espèces les plus capturées


pie.site = function(tab,listSpCouleur,site,nb)
{
  
  #browser()
  require(graphics)
  nb = min(nb,nrow(tab))
  colorSp = rep(NA,nb)


#print(tab[1:nb,1])
  for(i in 1:(nb))
  {
    colorSp[i] = listSpCouleur$couleur[which(listSpCouleur$espece == tab[i,1])]
  }
  autre = length(which(is.na(colorSp)))>0 
  if( autre )
  {
    colorSp[nb] = "grey"
    labelSp = c(tab[1:(nb),1],"Autres")
    vecPie = c(tab[1:(nb),2],(tab[nrow(tab),2]-sum(tab[1:(nb),2])))
  }
  else
  {
   labelSp = tab[1:nb,1]
   vecPie =  tab[1:nb,2]
   
  }
  #print(colorSp)
  titre = paste("Proportion de captures sur le site : ",site,sep="")
  # browser()
  #vecPie = c(tab[1:(nb-1),3],tab[nrow(tab),3]-sum(tab[1:(nb-1),3]))
  
  pie(vecPie, col=colorSp,labels = labelSp,cex=1,radius=1,main=titre,border=FALSE)  
  



}



batch.pie.MultiSite = function(nb=10)
{
  repertoire = "D:/Recherche/Donges/"
  nomFichier = paste(repertoire,"NbCparSite.csv",sep="")
  bilan = read.csv(nomFichier,stringsAsFactors=FALSE)
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichier = paste(repertoire,"bilanGlobalSp0RAPPORTsp.csv",sep="")
  bilanTout = read.csv(nomFichier,stringsAsFactors=FALSE)
  listSp = bilanTout[1:20,]
  couleur = couleurSp(listSp)
  sites = unique(bilan$SITE)
  for (sit in sites)
  {
#      print(an)
      tab = bilan[which(bilan$SITE == sit & bilan$Compte > 0),2:3]
      pie.site(tab,couleur,sit,nb)
      fichier.plot = paste(repertoire,"PieBilan",sit,sep="")
      savePlot(fichier.plot,"jpg")
  }
  
}



barplot.sp = function(db,sp)
{
  


  barplot(vec1)
  barplot(vec2,add=T)
  
}


batch.barplot.sp = function(db)
{
  annee = 2003:2007
  espece = "ACRSCH"
  for (sp in especes) barplot.sp(db,sp)
}


phenoQuantileExemple= function(db) 
{
  an = 2004
  sp = "ACRSCH"
  SQL = paste("SELECT Count(basecomplete.ACT), basecomplete.VAL_JOUR FROM basecomplete WHERE basecomplete.ACT= 'B' AND basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.SITE =  'B' OR basecomplete.ACT= 'B' AND basecomplete.SITE =  '' AND basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' GROUP BY basecomplete.VAL_JOUR ORDER BY basecomplete.VAL_JOUR ASC ",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tabGlobal = sqlQuery(db, SQL)
  SQL = paste("SELECT Count(basecomplete.ACT), basecomplete.VAL_JOUR FROM basecomplete WHERE basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.SITE =  'B' OR basecomplete.SITE =  '' AND basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' GROUP BY basecomplete.VAL_JOUR ORDER BY basecomplete.VAL_JOUR ASC ",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tabGlobal = tabGlobal[,c(2,1)]
  tabGlobal = cbind(tabGlobal,sqlQuery(db, SQL)[,1])
  
  titre =  "Nombre de captures par jour \n ACRSCH 2004"
  titreY = "Nombre de captures"
  titreX = "Jour de l'année"
  matplot(tabGlobal[,1],tabGlobal[,2:3],type='l',lwd=2,lty=1,col=c("yellow","orange"),main = titre,xlab = titreX,ylab=titreY)
 # op <- par(bg="antiquewhite1")
  xLegend = min(tabGlobal[,1])
  yLegend = max(tabGlobal[,3])

  legend(xLegend,yLegend,c("Baguage + Contrôle","Baguage"), col=c("orange","yellow"),cex = 0.8,lty = 1, lwd = 2,box.col="antiquewhite1")
 # par(op)

  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  fichier.plot = paste(repertoire,"distributionCapture",sep="")
  savePlot(fichier.plot)
  
  tabGlobal = cbind(tabGlobal,tabGlobal[,2:3])
  for(i in 2:nrow(tabGlobal))
    tabGlobal[i,4:5] = tabGlobal[i-1,4:5]+tabGlobal[i,2:3]
    
  titre =  "Nombre de captures cumulées\n ACRSCH 2004"
  titreY = "Nombre de captures"
  titreX = "Jour de l'année"

  matplot(tabGlobal[,1],tabGlobal[,4:5],type='l',lwd=2,lty=1,col=c("yellow","orange"),main = titre,xlab = titreX,ylab=titreY)
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  fichier.plot = paste(repertoire,"distributionCaptureCumul",sep="")
  
  SQL = paste("SELECT  basecomplete.ACT, basecomplete.VAL_JOUR FROM basecomplete WHERE basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.SITE =  'B' OR basecomplete.SITE =  '' AND basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' ORDER BY basecomplete.VAL_JOUR ASC",sep="") 
  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tabQuantile = sqlQuery(db, SQL)
  quanti = c(median(tabQuantile[,2]), quantile(tabQuantile[,2],0.025),quantile(tabQuantile[,2],0.975))
  abline(v = quanti[1],lty=1,col = "green")
  abline(v = quanti[2],lty=2,col="gray")
  abline(v = quanti[3],lty=3,col="gray")
#  op <- par(bg="antiquewhite1")
  xLegend = min(tabGlobal[,1])+1.5
  yLegend = max(tabGlobal[,5])
  legend(xLegend,yLegend,c("Baguage + Contrôle","Baguage","mediane","95% inferieur","95% superieur"), col=c("orange","yellow","green","gray","gray"),cex = 0.8,lty = c(1,1,1:3), lwd = c(2,2,1,1,1),box.col="antiquewhite1")
#  par(op)
  savePlot(fichier.plot,)
  
}



phenoSodaExemple= function(db) 
{
  an = 2004
  sp = "ACRSCH"
  SQL = paste("SELECT Count(basecomplete.ACT), basecomplete.VAL_JOUR FROM basecomplete WHERE basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.SITE =  'B' OR basecomplete.SITE =  '' AND basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' GROUP BY basecomplete.VAL_JOUR ORDER BY basecomplete.VAL_JOUR ASC ",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tabGlobal = sqlQuery(db, SQL)
  tabGlobal = tabGlobal[-1,]
  repertoire = "D:/Recherche/Donges/"
  fichierPhenoSoda = paste(repertoire,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
  tabPhenoSoda = read.csv(fichierPhenoSoda)
  phenoSoda = colSums(tabPhenoSoda)
  phenoSoda = phenoSoda[-1]
  tabGlobal = cbind(tabGlobal[,2],phenoSoda,tabGlobal[,1])
  titre =  paste("Phénologie classique Versus phénologie corrigée \n", sp," ", an,sep="")
  titreY = "Nombre d'oiseaux"
  titreX = "Jour de l'année"
  
  couleur=c("lightgreen","gray")
  matplot(tabGlobal[,1],tabGlobal[,2:3],type='l',lwd=2,lty=1,col=couleur,main = titre,xlab = titreX,ylab=titreY)
 # op <- par(bg="antiquewhite1")
  xLegend = min(tabGlobal[,1])
  yLegend = max(tabGlobal[,3])

  legend("topleft",c("Nombre d'oiseaux probable présents sur le site","Nombre de captures"), col=couleur,cex = 0.8,lty = 1, lwd = 2,bty="n")
 # par(op)

  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  fichier.plot = paste(repertoire,"exemplePhenologieSoda",sep="")
  savePlot(fichier.plot)
  
}



phenoQuantile.sp= function(db,sp) 
{
  annees = 2003:2008
  premierJour = c(216,215,204,207,203,208)
  dernierJour = c(236,236,239,244,238,244)  
  quantiToutAn = matrix(0,length(annees),3)
  row.names(quantiToutAn)=annees
  for (an in annees)
  {
    SQL = paste("SELECT  basecomplete.ACT, basecomplete.VAL_JOUR FROM basecomplete WHERE basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.SITE =  'B' OR basecomplete.SITE =  '' AND basecomplete.ANNEE =  ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' ORDER BY basecomplete.VAL_JOUR ASC",sep="") 
    cat("sql: ",SQL,"\n")
    flush.console()
  #browser()
    tabQuantile = sqlQuery(db, SQL)
    tabQuantile = tabQuantile[which(tabQuantile[,2]>=premierJour[which(annees==an)]& tabQuantile[,2]<=dernierJour[which(annees==an)]),]
    quanti = c(median(tabQuantile[,2]), quantile(tabQuantile[,2],0.025),quantile(tabQuantile[,2],0.975))
    quantiToutAn[which(annees==an),]= quanti
  }
  

  quantiToutAn = cbind(quantiToutAn,premierJour,dernierJour)
  quantiToutAn = as.data.frame(quantiToutAn)
  colnames(quantiToutAn)=c("mediane","95_inf","95_sup","Premier_jour","Dernier_jour")
  titre =  paste("Evolution de la distribution des captures pour les années ",min(annees)," à ",max(annees),"\n",sp,sep="")
  titreY = "Jour de l'année"
  titreX = "Année"
  
  matplot(annees,quantiToutAn,type='l',lwd=2,lty=c(1:5),col=c("lightgreen","darkseagreen","darkseagreen","gray","gray"),main = titre,xlab = titreX,ylab=titreY)

#  op <- par(bg="antiquewhite1")
  legend("topleft",c("mediane","95% inferieur","95% superieur","1er jour baguage","Dernier jour baguage","Tendance"), col=c("lightgreen","darkseagreen","darkseagreen","gray","gray","red"),cex = 0.7,lty = c(1:6), lwd = 1,bty="n")
#  par(op)
tendance = lm(quantiToutAn[,1]~annees)
  abline(tendance,col = "red",lty=6,lwd=2)
#browser()
  xLegend = min(annees)+0.6
  yLegend = min(premierJour)

  tendanceText = paste("Tendance :",round(tendance[[1]][2],2),"jour par an")
  text(xLegend,yLegend,tendanceText,cex = 0.8, font=2, col ="red" )
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  fichier.plot = paste(repertoire,"quantileCaputreMultiAn",sp,sep="")
  savePlot(fichier.plot,)
  
  
  fichierOut = paste(repertoire,"quantileCaputreMultiAn",sp,".csv",sep="" )
  write.csv(quantiToutAn,fichierOut)

}


 





batch.phenoQuantile = function(db)
{
  especes = c("ACRSCH","ACRSCI","ACROLA","LUSSVE")
  for (sp in especes) phenoQuantile.sp(db,sp)  
}







ageRatioJourSp = function(db,an,sp)
{
  varAge = "+1a"
  SQL = paste("SELECT basecomplete.VAL_JOUR, Count(basecomplete.ACT) AS nbAge FROM basecomplete WHERE basecomplete.ANNEE = ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.AGE =  '",varAge,"' GROUP BY basecomplete.VAL_JOUR ORDER BY basecomplete.VAL_JOUR ASC",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()
#browser()
  tabAd = sqlQuery(db, SQL)
  
  varAge = "1a"
  SQL = paste("SELECT basecomplete.VAL_JOUR, Count(basecomplete.ACT) AS nbAge FROM basecomplete WHERE basecomplete.ANNEE = ",an," AND basecomplete.LOCALITE =  'Donges' AND basecomplete.ESPECE =  '",sp,"' AND basecomplete.ERREUR <  '2' AND basecomplete.AGE =  '",varAge,"' GROUP BY basecomplete.VAL_JOUR ORDER BY basecomplete.VAL_JOUR ASC",sep="")
  cat("sql: ",SQL,"\n")
  flush.console()

  tabJuv = sqlQuery(db, SQL)
  
  jourDebut = min(tabAd$VAL_JOUR[1],tabJuv$VAL_JOUR[1])
  jourFin = max(tabAd$VAL_JOUR[nrow(tabAd)],tabJuv$VAL_JOUR[nrow(tabJuv)])
  jours = jourDebut:jourFin
  tabAgeRatio = data.frame(ageRatio = rep(NA,length(jours)))
  row.names(tabAgeRatio)= jours
  
  for(j in jours)
  {
   # if (j == 232) browser()
    #print(j)
    adJ = tabAd$nbAge[which(tabAd$VAL_JOUR==j)]
    if (length(adJ)== 0) adJ = 0
    juvJ = tabJuv$nbAge[which(tabJuv$VAL_JOUR==j)]
    if (length(juvJ)== 0) juvJ = 0
    tabAgeRatio$ageRatio[which(jours == j)] = juvJ / (juvJ + adJ)
  }


  repertoire = "D:/Recherche/Donges/analysesDescriptives/" 
  fichier = paste(repertoire,"ageRatioJour",sp,an,".csv",sep="")
  write.csv(tabAgeRatio,fichier)
  cat("  <-- ",fichier,"\n")
  flush.console()

  return(tabAgeRatio)
  
}
  

  

ageRatio.plot = function(tab,an,sp)
{
 jours = as.numeric(paste(row.names(tab)))
 age =  tab$ageRatio
 titre = paste("Evolution de l'age ratio au cours de la saison\n",sp," ",an,sep="")
 titreX = "Jour de l'année"
 titreY = "Age-ratio"
 plot(jours,age,pch=20,col="darkgreen",main=titre,xlab=titreX,ylab=titreY)
 #browser()
 tendance=lm(age~jours)
 #print(tendance)
 abline(tendance,col="red",lwd=2,lty=1)
 legend("bottomleft","tendance",col="red",lwd=2,lty=1,bty="n")
 repertoire="D:/Recherche/Donges/analysesDescriptives/"
 fichier = paste(repertoire,"plotAgeRatioJour",sp,an,sep="")
 savePlot(fichier,"png") 


}  
  
   
 

batch.ageRatio.plot = function(db)
{
  annees = 2003:2008
  especes = c("ACRSCH","ACRSCI","ACROLA","LUSSVE")
  for (sp in especes) 
    for(an in annees) ageRatio.plot(ageRatioJourSp(db,an,sp),an,sp)  


}


ageRatioJourSpToutAn = function(sp)
{

  annees = 2003:2007
  for (an in annees)
  {
    repertoire =  "D:/Recherche/Donges/analysesDescriptives/" 
    fichier =  paste(repertoire,"ageRatioJour",sp,an,".csv",sep="")
    newTab = read.csv(fichier)
    newTab = cbind(newTab,rep(an,nrow(newTab)))
    if (an == 2003)
      tab =  newTab
    else
      tab = rbind(tab,newTab) 
  }
   # browser()
    colnames(tab) = c("jour","ageRatio","annee")

  repertoire = "D:/Recherche/Donges/analysesDescriptives/" 
  fichier = paste(repertoire,"ageRatioJour",sp,"ToutAn.csv",sep="")
  write.csv(tab,fichier)
  cat("  <-- ",fichier,"\n")
  flush.console()
  return(tab)

 
}

ageRatioMultiAn.plot = function(tab,sp)
{
  jours = tab$jour
  age =  tab$ageRatio
  texteLegende = 2003:2007
  groupe = as.factor(tab$annee)
  titre = paste("Evolution de l'age ratio au cours de la saison \n",sp,sep="")
  titreX = "Jour de l'année"
  titreY = "Age-ratio"
  #browser()
  require(graphics)
  palette(rainbow(length(texteLegende),start=.0,end=.3))
#  palette(cm.colors(length(texteLegende)))  
  couleur = ((-1 * as.numeric(as.factor(tab$annee))) + length(texteLegende) + 1)
  plot(jours,age,pch=20,col=couleur,cex=0.8,main=titre,xlab=titreX,ylab=titreY)
  legend("bottomright",paste(texteLegende),pch=20,col=5:1,bty="n",cex=0.8)

  dateNA = which(is.na(age))
  dateNAmin = min(dateNA)
  dataExtract = which(jours < 215 & age > 0.9)
  dataExtract = c(dataExtract,dateNA)
    
  joursCour = jours[-dataExtract]
  ageCour = age[-dataExtract]
  #plot(joursCour,ageCour)
  
  tendance=smooth.spline(joursCour,ageCour,df=6)

  #print(tendance)
  
  lines(tendance,col="blue",lwd=2,lty=1)
  
  legend("bottomleft","tendance",col="blue",lwd=2,lty=1,bty="n")
  #  browser()
  repertoire="D:/Recherche/Donges/analysesDescriptives/"
  fichier = paste(repertoire,"plotAgeRatioJour",sp,"toutAn",sep="")
  savePlot(fichier,"png") 
  cat("  <-- ",fichier,"\n")
  flush.console()


}  
  

batch.ageRatioMultiAn.plot = function()
{
  especes = c("ACRSCH","ACRSCI","ACROLA","LUSSVE")
  for (sp in especes) 
    ageRatioMultiAn.plot(ageRatioJourSpToutAn(sp),sp)  


}



ageRatioAnMultiSp = function(especes,annees)
{
  tab = as.data.frame(matrix(0,length(annees),length(especes)))
  rownames(tab) = annees
  colnames(tab) = especes
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  for(an in annees)
  {
    #browser()
    fichier = paste(repertoire,"bilanGlobalSp",an,".csv",sep="")
    tabAn = read.csv(fichier)
    for(e in especes)
      tab[which(annees == an),which(especes == e)] = tabAn$AGE_RATIO[which(tabAn$ESPECE == e)]
  }
  fichierOut = paste(repertoire,"AgeRatioSpAn.csv",sep="")  
  write.csv(tab,fichierOut)
  return(tab)
}


ageRatioAnMultiSp.plot = function(tab)
{
  repertoire = "D:/Recherche/Donges/analysesDescriptives/"
  nomFichierCouleur = paste(repertoire,"couleurEspece.csv",sep="")
  listCouleur = read.csv(nomFichierCouleur,stringsAsFactors=FALSE)
  
  especes = colnames(tab)
  colorSp = listCouleur$couleur[which(listCouleur$espece==especes[1])]
  for(sp in 2:4) colorSp = c(colorSp,listCouleur$couleur[which(listCouleur$espece==especes[sp])])

  x = rownames(tab)
  titre = paste("Age-Ratio pour les années 2003 à 2007",sep=" ")

  matplot(x,tab,type='l',lty=1,lwd=2,col = colorSp,main = titre, ylab= "Age-ratio",xlab="Année")
  legend("bottomleft",especes,col = colorSp,cex = 0.8,lty = 1, lwd = 2,bty = "n") 
  
  fichierPlot = paste(repertoire,"AgeRatioSpAn",sep="")
  savePlot(fichierPlot,"png")  
}



#######################################
# MAIN PROGRAMME



library(RODBC)
library(stats)
#library(mra) 
#db = ouvrirConnection("donges_local")# ("donges_doxa")   #
##analyseLP_MA(db)
#analyseErreurControleDonges(db)
#bilanGlobalMultiAn(db)
vecteurAn = 2003:2007
#batch.bilanAnSp(db,vecteurAn)
#batch.pie.annee()
#batch.barplot.sp(db)
#phenoQuantileExemple(db)
#batch.phenoQuantile(db)
#batch.ageRatio.plot(db)
#especes = c("ACRSCH","ACRSCI","LUSSVE","ACROLA")
#ageRatioAnMultiSp.plot(ageRatioAnMultiSp(especes,vecteurAn))
#
#phenoSodaExemple(db)
#femerConnection(db)
#
batch.ageRatioMultiAn.plot()

#batch.pie.MultiSite()

