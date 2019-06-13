###############################################
# Phénologie part analyse du STOP overduration
###############################################

read.tableCMR = function(an,sp)
{
#  repertoire = paste("D:/Recherche/Donges/",sep="")
  repertoire =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
  fichier = paste(repertoire,sp,an,"-r.txt",sep="")
  tab=read.table(fichier,sep=" ")
  cat("       <-- ",fichier,"\n")
  histoireVide = which(rowSums(tab)==0)
  if(length(histoireVide)>0)
  {
    tab = tab[-histoireVide,]
    cat("       !!!!! ATTENTION !!!!! ",length(histoireVide)," histoire(s) sans capture !!!!! \n")
  }
  flush.console()
  return(tab)
}



read.tableCMR.group = function(an,sp)
{
  repertoire = paste("C:/DataAggeliki/etude/data/CMR/Age-Sexe/",sep="")
  fichier = paste(repertoire,sp,".txt",sep="")
  tab = read.delim(fichier,colClasses="character")
  tab = tab[which(tab$ANNEE == an),]
  historique = tab[,1]
  newTab = as.numeric(substr(historique,1,1))
  for (n in 2:nchar(historique[1]))
  newTab = cbind(newTab,as.numeric(substr(historique,n,n)))
  newTab = newTab[,-((ncol(newTab)-2):ncol(newTab))]
  histoireVide = which(rowSums(newTab)==0)
  if(length(histoireVide)>0)
  {
    tab = newTab[-histoireVide,]
    cat("       !!!!! ATTENTION !!!!! ",length(histoireVide)," histoire(s) sans capture !!!!! \n")
  }
# browser()
  if(sp == "LUSSVE")
  {
    newTab = cbind(newTab,tab$AGE)
    newTab = apply(newTab,2,as.numeric)
    newTab = as.data.frame(newTab)
    newTab = cbind(newTab,tab$SEXE)
    colnames(newTab) = c(paste("occ",1:(ncol(newTab)-2)),"AGE","SEXE")
  }
  else 
  {
    newTab = cbind(newTab,tab$AGE)
    newTab = apply(newTab,2,as.numeric)
    newTab = as.data.frame(newTab)
    colnames(newTab) = c(paste("occ",1:(ncol(newTab)-1)),"AGE")
  }
  return(newTab)
}



read.parametre = function(an,sp)
{
#  repertoire = paste("D:/Recherche/Donges/",sep="")
  repertoire =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")

  fichier = paste(repertoire,"results",sp,an,".csv",sep="")
  tab = read.csv(fichier)
  cat("       <-- ",fichier,"\n")
  flush.console()
  return(tab)
}

rempliPresence = function(tab)
{

  for(l in 1:nrow(tab))
  {
    present = min(which(tab[l,]==1)): max(which(tab[l,]==1))
    tab[l,present] = 1
  }
  return(tab)
}


rempliPresenceSTD = function(tab,before.moyenne,after.moyenne)
{
  for(l in 1:nrow(tab))
  {
    presence.min = min(which(tab[l,]==1))- before.moyenne
    presence.min = max(presence.min,1)
    presence.max = max(which(tab[l,]==1))+ after.moyenne
    presence.max = min(presence.max,ncol(tab))
    present = presence.min: presence.max
    tab[l,present] = 1
  }
  return(tab)
}


rempliPresenceSTDproba = function(tab,before.moyenne,before.sd,after.moyenne,after.sd)
{
# browser()
  for(l in 1:nrow(tab))
  {
#  cat(l," ")
#    if (l%%25==0) cat("\n")
#    flush.console()
    presence.min = min(which(tab[l,]==1))
    presence.max = max(which(tab[l,]==1))
   if (presence.min > 1)
    for(cel in 1:(presence.min-1))
      tab[l,cel] = (-1*(pnorm((presence.min-cel), mean = before.moyenne, sd = before.sd ))+1)
   
   if (presence.max < ncol(tab))
    for(cel in (presence.max+1):ncol(tab))
      tab[l,cel] = (-1*(pnorm((cel-presence.max), mean = after.moyenne, sd = after.sd ))+1)
  }
  return(tab)
}

rempliPresenceSTDproba_t = function(tab,before.moyenne,before.sd,after.moyenne,after.sd)
{
# browser()
  for(l in 1:nrow(tab))
  {
    presence.min = min(which(tab[l,]==1))
    presence.max = max(which(tab[l,]==1))
   if (presence.min > 1)
    for(cel in 1:(presence.min-1))
      tab[l,cel] = (-1*(pnorm((presence.min-cel), mean = as.numeric(before.moyenne[presence.min]), sd = as.numeric(before.sd[presence.min] )))+1)
   
   if (presence.max < ncol(tab))
    for(cel in (presence.max+1):ncol(tab))
      tab[l,cel] = (-1*(pnorm((cel-presence.max), mean = as.numeric(after.moyenne[presence.max]), sd = as.numeric(after.sd[presence.max]) ))+1)
  }
  return(tab)
}


pheno.spAn = function(sp,an)
{
#  tabImport = read.tableCMR(an,sp)
  if (sp == "LUSSVE") coVar = 2 
  else coVar = 1
 

 # browser()
  tabImportGroup = read.tableCMR.group(an,sp)
  colCoVar = (ncol(tabImportGroup)+1 - coVar):ncol(tabImportGroup)
  tabImport = tabImportGroup[,-(colCoVar)]
  tabParametre = read.parametre(an,sp)
  vecjour1 = c(216,216,204,207,203,208)
  jour1 = vecjour1[which(2003:2008 == an)]

  
  
  before.mean =   tabParametre[2,ncol(tabParametre)]
  before.sd =     tabParametre[11,ncol(tabParametre)]  #1 #
  after.mean =    tabParametre[3,ncol(tabParametre)]
  after.sd =      tabParametre[12,ncol(tabParametre)] #1 #
  T.before.mean = tabParametre[2,1:(ncol(tabParametre)-1)]
  T.before.sd =   tabParametre[11,1:(ncol(tabParametre)-1)] # rep(1,(ncol(tabParametre)-1)) #
  T.after.mean =  tabParametre[3,1:(ncol(tabParametre)-1)]
  T.after.sd =    tabParametre[12,1:(ncol(tabParametre)-1)] #rep(1,(ncol(tabParametre)-1)) #

  
  cat(
    "      before: ",round(before.mean,2), "(",round(before.sd),")\n",
    "      after: ",round(after.mean,2), "(",round(after.sd),")\n",sep="")
  
  cat("      (1) presence\n")
  flush.console()
  tabPresence = rempliPresence(tabImport)
  cat("      (2) presence STD\n")
  flush.console()
 tabPresenceSTD = rempliPresenceSTD(tabImport,round(before.mean),round(after.mean))
  cat("      (3) presence STD proba\n")
  flush.console()
  tabPresenceSTDproba = rempliPresenceSTDproba(tabPresence,before.mean,before.sd,after.mean,after.sd)
  cat("      (4) presence STD proba temps dépéndent\n")
  flush.console()
  tabPresenceSTDproba_t = rempliPresenceSTDproba_t(tabPresence,T.before.mean,T.before.sd,T.after.mean,T.after.sd)


  
  # browser()
  matrice = t(rbind(colSums(tabPresence),colSums(tabPresenceSTD),colSums(tabPresenceSTDproba),colSums(tabPresenceSTDproba_t)))
  
#  repertoire = paste("D:/Recherche/Donges/",sep="")
  repertoire =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")

  fichier = paste(repertoire,"tabPresence_",sp,an,".csv",sep="")
  tabPresence= cbind(tabPresence,tabImportGroup[,colCoVar])
  write.csv(tabPresence,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  fichier = paste(repertoire,"tabPresenceSTD_",sp,an,".csv",sep="")
  tabPresenceSTD= cbind(tabPresenceSTD,tabImportGroup[,colCoVar])
  write.csv(tabPresenceSTD,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  fichier = paste(repertoire,"tabPresenceSTDproba_",sp,an,".csv",sep="")
  tabPresenceSTDproba= cbind(tabPresenceSTDproba,tabImportGroup[,colCoVar])
  write.csv(tabPresenceSTDproba,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  fichier = paste(repertoire,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
  tabPresenceSTDproba_t= cbind(tabPresenceSTDproba_t,tabImportGroup[,colCoVar])
  write.csv(tabPresenceSTDproba_t,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  
#browser()

  x = jour1:(jour1+ncol(tabImport)-1)
  titre = paste("Phenologie",sp,an,sep=" ")
#  x11()
  matplot(x,matrice,type='l',lty=1,lwd=c(1,1,1,2),col = c("black","darkblue","darkred","red"),main = titre, ylab= "Effectif",xlab="Jour année")
                
#  repertoirePlot = paste("D:/Recherche/Donges/",sep="")
  repertoirePlot =  repertoire #paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/"sep="")
  fichierPlot = paste(repertoirePlot,"phenoSTD_",sp,an,sep="")
  savePlot(fichierPlot,"png")
  cat("       ==> ",fichierPlot,"\n")
  flush.console()
}



phenologieMultiSp.matplot = function(an)
{
  
 
  vecjour1 = c(216,216,204,207,203,208)
  jour1 = vecjour1[which(2003:2008 == an)]

  
  repertoire = "C:/DataAggeliki/etude/Tinn-R/"
  nomFichierCouleur = paste(repertoire,"couleurEspece.csv",sep="")
  listCouleur = read.csv(nomFichierCouleur,stringsAsFactors=FALSE)

  especes = c("ACRSCH","ACRSCI","LUSSVE","ACROLA")
  for(sp in especes)
  { 
    repertoire = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
    fichier = paste(repertoire,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
    tabSp = read.csv(fichier,stringsAsFactors=FALSE)
  
    tabSp = tabSp[,-1]
    if (which(especes == sp) == 1) tabMatplot =  colSums(tabSp)
    else tabMatplot = rbind(tabMatplot,colSums(tabSp))
   }
  
  tabMatplot = t(tabMatplot)
  colorSp = listCouleur$couleur[which(listCouleur$espece==especes[1])]
  for(sp in 2:4) colorSp = c(colorSp,listCouleur$couleur[which(listCouleur$espece==especes[sp])])
   
  x = jour1:(jour1+ncol(tabSp)-1)
  titre = paste("Phenologie",an,sep=" ")
# x11()
  matplot(x,tabMatplot,type='l',lty=1,lwd=2,col = colorSp,main = titre, ylab= "Effectif",xlab="Jours année")
  repertoirePlot =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sep="")
  op <- par(bg="lightgray")
  xLegend = max(x) - 5
  yLegend = max(tabMatplot) - 20
  legend("topright", especes, col=colorSp,cex = 0.8,lty = 1, lwd = 2,box.col="black")
  par(op)
  fichierPlot = paste(repertoirePlot,"phenoMultiSp_",an,sep="")
  savePlot(fichierPlot,"png")
  cat("       ==> ",fichierPlot,"\n")
  flush.console()


  #print(colorSp)
 


}

phenologieAgeSp.matplot = function(sp,an)
{
  if (sp == "LUSSVE") coVar = 2 
  else coVar = 1
   
 
  vecjour1 = c(216,216,204,207,203,208)
  jour1 = vecjour1[which(2003:2008 == an)]

  
#  repertoire = "C:/DataAggeliki/etude/Tinn-R/"
#  nomFichierCouleur = paste(repertoire,"couleurEspece.csv",sep="")
#  listCouleur = read.csv(nomFichierCouleur,stringsAsFactors=FALSE)

  repertoire = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
  fichier = paste(repertoire,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
  tabSp = read.csv(fichier,stringsAsFactors=FALSE)
  colCoVar = tabSp[,ncol(tabSp)+1 - coVar]
  tabSp = tabSp[,-1]
  tabMatplot = rbind(
    colSums(tabSp[,-ncol(tabSp)]),
    colSums(tabSp[which(colCoVar ==1),-ncol(tabSp)]),
    colSums(tabSp[which(colCoVar ==0),-ncol(tabSp)]))
  tabMatplot = t(tabMatplot)
  colorSp = c("red","darkgreen","green")
  #for(sp in 2:4) colorSp = c(colorSp,listCouleur$couleur[which(listCouleur$espece==especes[sp])])
  #browser() 
  x = jour1:(jour1+ncol(tabSp)-2)
  titre = paste("Phenologie",an,sp,sep=" ")
 #x11()
  matplot(x,tabMatplot,type='l',lty=1,lwd=2,col = colorSp,main = titre, ylab= "Effectif",xlab="Jours année")
  repertoirePlot =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
  op <- par(bg="lightgray")
  xLegend = max(x) - 5
  yLegend = max(tabMatplot) - 20
  legend("topleft", c("Tous les individus","Adultes","Juvéniles"), col=colorSp,cex = 0.7,lty = 1, lwd = 2,bty = "n")
  par(op)
  
  fichierPlot = paste(repertoirePlot,"phenoAGE_",sp,an,sep="")
  savePlot(fichierPlot,"png")
  cat("       ==> ",fichierPlot,"\n")
  flush.console()


  #print(colorSp)
 


}


phenologieSexeSp.matplot = function(sp,an)
{
  if (sp == "LUSSVE") coVar = 2 
  else coVar = 1
   
 
  vecjour1 = c(216,216,204,207,203,208)
  jour1 = vecjour1[which(2003:2008 == an)]

  
#  repertoire = "C:/DataAggeliki/etude/Tinn-R/"
#  nomFichierCouleur = paste(repertoire,"couleurEspece.csv",sep="")
#  listCouleur = read.csv(nomFichierCouleur,stringsAsFactors=FALSE)

  repertoire = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
  fichier = paste(repertoire,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
  tabSp = read.csv(fichier,stringsAsFactors=FALSE)
  colCoVar = tabSp[,ncol(tabSp)]
  tabSp = tabSp[,-1]
 # browser()
  tabMatplot = rbind(
    colSums(tabSp[,-ncol(tabSp)]),
    colSums(tabSp[which(colCoVar =="F"),-ncol(tabSp)]),
    colSums(tabSp[which(colCoVar =="M"),-ncol(tabSp)]))
  tabMatplot = t(tabMatplot)
  colorSp = c("darkgreen","darkred","darkblue")
  #for(sp in 2:4) colorSp = c(colorSp,listCouleur$couleur[which(listCouleur$espece==especes[sp])])
  #browser() 
  x = jour1:(jour1+ncol(tabSp)-2)
  titre = paste("Phenologie",an,sp,sep=" ")
 #x11()
  matplot(x,tabMatplot,type='l',lty=1,lwd=2,col = colorSp,main = titre, ylab= "Effectif",xlab="Jours année")
  repertoirePlot =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
  op <- par(bg="lightgray")
  xLegend = max(x) - 5
  yLegend = max(tabMatplot) - 20
  legend("topleft", c("Tous les individus","Femelles","Males"), col=colorSp,cex = 0.7,lty = 1, lwd = 2,bty = "n")
  par(op)
  
  fichierPlot = paste(repertoirePlot,"phenoSEXE_",sp,an,sep="")
  savePlot(fichierPlot,"png")
  cat("       ==> ",fichierPlot,"\n")
  flush.console()


  #print(colorSp)
 


}

phenoSodaEffortspAn = function(sp,an)
{
  repertoire = "D:/Recherche/Donges/"
  repertoireSODA = repertoire
  #repertoire =  paste("C:/DataAggeliki/etude/data/",sep="")
  #repertoireSODA  = paste(repertoire,"CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")
  fichierSODA =   paste(repertoireSODA,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
  fichierEffort = paste(repertoire,"EffortObs",sp,".csv",sep="")
# browser()
  phenoTab = read.csv(fichierSODA) 
  effort = read.csv(fichierEffort) 
  effort = effort[which(effort$ANNEE==an),]
  phenoTab = phenoTab[,-c(1,ncol(phenoTab))]
  pheno = colSums(phenoTab)
  
  tab =  cbind(effort$VAL_JOUR,effort$EffObs,pheno)
  colnames(tab) = c("val_jour","EffObs","pheno")
  repertoireOUT = repertoire
  #repertoireOUT = paste("C:/DataAggeliki/etude/data/CMR/t=1d/Global/",sp,"/",sep="")
  fichierOut = paste(repertoireOUT,"phenoEffort",sp,an,".csv",sep="")
  write.csv(tab,fichierOut)
  cat("<-- ",fichierOut,"\n")
  flush.console()
  return(tab)
}

             
phenoSodaEffortspAn.plot = function(sp)
{
 sp = "ACRSCH"
  #repertoire =  paste("C:/DataAggeliki/etude/data/",sep="")
  repertoire = "D:/Recherche/Donges/"
  fichierEffort = paste(repertoire,"EffortObs",sp,".csv",sep="")
  effort = read.csv(fichierEffort) 
  annees = unique(effort$ANNEE)
 print(annees)
  jourMin = min(effort$VAL_JOUR)
  jourMax = max(effort$VAL_JOUR)
  joursCaptures = jourMin:jourMax
  plot.new()
# browser()
  nombreCase = (length(annees)+1)*2
	m = matrix(c(0,rep(5:1,each=2),0),nombreCase,1)
	print(m)
	layout(m)
#  x = row.names(tabColSimul)
  layout.show(m)
  for (ia in 1:length(annees))
  {
 	  an = annees[ia]
    #browser()
    par(mar=c(0,4,0,4))
    tabAn = phenoSodaEffortspAn(sp,an)
    tabAn = as.data.frame(tabAn)
    if (min(tabAn$val_jour)>jourMin)
    {
      ajout = cbind(jourMin:(min(tabAn$val_jour)-1),rep(0,length(jourMin:min(tabAn$val_jour)-1)-1),rep(0,length(jourMin:min(tabAn$val_jour)-1)-1))

      colnames(ajout) = c("val_jour","EffObs","pheno") 
     # print(ajout)
    #  tabAn=rbind(ajout,tabAn)
    }
    if (max(tabAn$val_jour)<jourMax)
     {
      ajout = cbind(((max(tabAn$val_jour)+1):jourMax),rep(0,length(max(tabAn$val_jour)+1:jourMax)-1),rep(0,length(max(tabAn$val_jour)+1:jourMax)-1))
            colnames(ajout) = c("val_jour","EffObs","pheno")
      #       print(ajout) 
     # tabAn=rbind(tabAn,ajout)  
      
      }
   
    yGmax = max(tabAn$pheno)
    yGmin = 0
    yDmax = max(tabAn$EffObs)
    yDmin = 0
    EffObs.trans = ((tabAn$EffObs-yDmin)/(yDmax-yDmin)) * yGmax
    matplot(tabAn$val_jour,cbind(tabAn$pheno,EffObs.trans),xlim=c(jourMin,jourMax),type="l",col="lightblue",lwd=1,lty=1,axes=F,ann = FALSE)
    polygonCI = cbind(c(jourMin,tabAn$val_jour,jourMax),c(0,EffObs.trans,0))
    
    polygon(polygonCI[,1], polygonCI[,2],col = "lightblue", border = "blue")
    lines(tabAn$val_jour,tabAn$pheno,type='l',col="red",lty = 1, lwd = 2)
    
         text(1000,250,an,col="black",cex=2)
    coordLabelAxe = c(0,(yGmin:(yGmax/250)+1) * 250)
    labCol = paste(
    as.integer(seq(from = yGmin , to = yGmax , 
      by = (yGmax - yGmin) / (length(coordLabelAxe)-1))))
    labHS = paste(
    round(seq(from = yDmin , to = yDmax , 
      by = (yDmax - yDmin) / (length(coordLabelAxe)-1),digits=2),digits=2))
    #browser()
    if (ia %% 2 == 0)
    {
      axis(2, coordLabelAxe, labels = labCol, col.axis = "red",col = "red",padj=-0.5)
      axis(4, coordLabelAxe, labels = labHS, col.axis = "blue",col = "blue",padj=0.5)
       
    }
    else
    {
      axis(2, coordLabelAxe, labels = labCol, col.axis = "red",col = "red",padj =0.5)
      axis(4, coordLabelAxe, labels = labHS, col.axis = "blue",col = "blue",padj =-0.5)
    
    }
    if(ia == 1) axis(1)
    box()
    if(ia == 3 )
    {
      mtext("Nombre d'oiseaux probable dans la roselière", 
        side = 2, col ="red", las = 3, line = 4, adj = 0.5)
      mtext("Effort de capture", 
        side = 4, col = "blue", las = 3, line = 4, adj = 0.5)
    }
    if( ia == 1)
    mtext("Jour de l'année", 
      side = 1, col = "black", las = 1, line = 2, adj = 0.5)
    if(ia == 5 )
    {
     mtext(paste("Phénologie et effort de capture:",sp), 
      side = 3, col = "black", las = 1, line = 2, adj = 0.5)
    }
    anY = yGmax-100  
    anx = max(tabAn$val_jour)-1
    text(anx,anY,an,col="black",cex=1.5,)

    
    
  }

}


 

batch.phenoSodaEffortspAn =function(especes)
{
  especes="ACRSCH"
  for(sp in especes)
    phenoSodaEffortspAn.plot(sp)

}

#________________________________________________

#        Program Main

#________________________________________________

especes = c("ACRSCH","ACRSCI")#,"LUSSVE","ACROLA")
annees = 2004:2007


#especes ="ACROLA"
#annees = 2005


for(sp in especes)
{
  cat(sp,"\n")
   batch.phenoSodaEffortspAn(sp)
#  for(an in annees)
#  { 
##    cat("  ",an ,"\n")   
#    pheno.spAn(sp,an)
#   phenologieAgeSp.matplot(sp,an) 
#    if (sp=="LUSSVE") phenologieSexeSp.matplot(sp,an) 
#  }
}
#

#  require(graphics)
# for(an in annees)
#  { 
#    phenologieMultiSp.matplot(an)  
#  }
#
