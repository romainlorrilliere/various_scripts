# Analyse CMR Donges
#lecture fichier et preparation du tableau
read.sodaBRUT = function(sp,an)
{
#    repertoire = paste("D:/Recherche/Donges/",sep="")
  repertoire = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/",sep="")
  fichier = paste(repertoire,"results",sp,an,sep="")
  cat(" <-- ",fichier,"\n")
  tab = read.table(fichier)
  return(tab)
}
#
#
#
read.soda = function(sp,an)
{
     #browser()
     vecAnnees = 2003:2008
    occasion = c(21,21,35,38,36,37)
     tab = read.sodaBRUT(sp,an)
     lignePourri = which(rowMeans(tab)>100)
     if (length(lignePourri) >0 )
      tab = tab[-(lignePourri),]
    
    if(ncol(tab)>3)
    {
      occas = occasion[which(vecAnnees == an)]
      
      subTab = tab[,1:occas]
      moyenne = mean(as.matrix(subTab))
      mediane = median(as.vector(as.matrix(subTab)))     ###
      doxa95sup =  quantile(as.matrix(subTab),0.975)
      doxa95inf =  quantile(as.matrix(subTab),0.025)
      doxaSd =  sd(as.vector(as.matrix(subTab)))
      vecMean = colMeans(subTab)
      vecMedian = apply(subTab,2,median)  #????
      vec95sup = apply(subTab,2,quantile,0.975)
      vec95inf = apply(subTab,2,quantile,0.025)
      vecSd = apply(subTab,2,sd)
      
      subTab = tab[,(occas+1):(2*occas)]
      moyenne = c(moyenne, mean(as.matrix(subTab)))
      mediane = c(mediane, median(as.vector(as.matrix(subTab))))   ###
      doxa95sup =  c(doxa95sup, quantile(as.matrix(subTab),0.975))
      doxa95inf =  c(doxa95inf, quantile(as.matrix(subTab),0.025))
      doxaSd =  c(doxaSd,sd(as.vector(as.matrix(subTab))))
      vecMean = cbind(vecMean,colMeans(subTab))
      vecMedian = cbind(vecMedian,apply(subTab,2,median))   #????
      vec95sup = cbind(vec95sup,apply(subTab,2,quantile,0.975))
      vec95inf = cbind(vec95inf,apply(subTab,2,quantile,0.025))
      vecSd = cbind(vecSd,apply(subTab,2,sd))
      
      subTab = tab[,(2*occas+1):(3*occas)]
      moyenne = c(moyenne, mean(as.matrix(subTab)))
      mediane = c(mediane, median(as.vector(as.matrix(subTab))))      ###
      doxa95sup =  c(doxa95sup, quantile(as.matrix(subTab),0.975))
      doxa95inf =  c(doxa95inf, quantile(as.matrix(subTab),0.025))
      doxaSd =  c(doxaSd,sd(as.vector(as.matrix(subTab))))
      vecMean = cbind(vecMean,colMeans(subTab))
      vecMedian = cbind(vecMedian,apply(subTab,2,median))   #????
      vec95sup = cbind(vec95sup,apply(subTab,2,quantile,0.975))
      vec95inf = cbind(vec95inf,apply(subTab,2,quantile,0.025))
      vecSd = cbind(vecSd,apply(subTab,2,sd))
    #browser()
      tab = as.data.frame(cbind(rbind(t(vecMean),t(vecMedian),t(vec95sup),t(vec95inf),t(vecSd)),c(moyenne,mediane,doxa95sup, doxa95inf,doxaSd)))
      colonnes = c(paste("occasion_",1:occas),"total_moy_med_sup_inf_sd")
      lignes = c("moyenneTotal","moyenneBefore","moyenneAfter","medianeTotal","medianeBefore","medianeAfter","95supTotal","95supBefore","95supAfter","95infTotal","95infBefore","95infAfter","sdTotal","sdBefore","sdAfter")   ###
      colnames(tab)= colonnes
      rownames(tab) = lignes
    
      fichierSortie = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/results",sp,an,".csv",sep="")
      write.csv(tab,fichierSortie,sep=",",row.names=TRUE)
      cat(" ==> ",fichierSortie,"\n")
      flush.console() 
      return(tab)
    }
    else
       cat("BACHIBOUZOUK \n")
}


read.sodaResult = function(sp,an)
{
  fichier =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/results",sp,an,".csv",sep="")
  cat(" <-- ",fichier,"\n")
  tab = read.csv(fichier)
  return(tab)
}




histoSODA = function(sp,an)
{
   vecAnnees = 2003:2008
  occasion = c(21,21,35,38,36,37)
    tab = read.sodaBRUT(sp,an)  
    lignePourri = which(rowMeans(tab)>100)
    if (length(lignePourri) >0 )
      tab = tab[-(lignePourri),]

    if(ncol(tab)>3)
    {
      occas = occasion[which(vecAnnees == an)]
      subTab = tab[,1:occas]
    # browser()
      titre = paste("hist TOTAL",sp,an,sep=" ")
      hist(rowMeans(subTab),main=titre)#,breaks=15,xlim=c(0,60))
      hist(apply(subTab,1,median),main=titre)#,breaks=15,xlim=c(0,60))   #????
      fichierSortie = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/histTOTAL",sp,an,sep="")
      savePlot(fichierSortie,"png",restoreConsole = TRUE)
      cat(" ==> ",fichierSortie,"\n")
      flush.console() 
      subTab = tab[,(occas+1):(2*occas)]
      titre = paste("hist BEFORE",sp,an,sep=" ")
      hist(rowMeans(subTab),main=titre)#,breaks=15,xlim=c(0,40))
      hist(apply(subTab,1,median),main=titre)#,breaks=15,xlim=c(0,40)) #????
      fichierSortie = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/histBEFORE",sp,an,sep="")
      savePlot(fichierSortie,"png",restoreConsole = TRUE)
      cat(" ==> ",fichierSortie,"\n")
      flush.console() 
      subTab = tab[,(2*occas+1):(3*occas)]
      titre = paste("hist AFTER",sp,an,sep=" ")
      hist(rowMeans(subTab),main=titre)#,breaks=15,xlim=c(0,40))
      hist(apply(subTab,1,median),main=titre)#,breaks=15,xlim=c(0,40))  #????
      fichierSortie = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/histAFTER",sp,an,sep="")
      savePlot(fichierSortie,"png",restoreConsole = TRUE)
      cat(" ==> ",fichierSortie,"\n")
      flush.console() 
    }
    else
       cat("MARIN D'EAU DOUCE \n")
}


Stop_overDuration.matplot= function(sp,an)
{
  fichier = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/results",sp,an,".csv",sep="")
  tab = read.csv(fichier)  
  titre = paste("Plot Total",sp,an,sep=" ")
 # x11()
  matplot(t(tab[c(1,4,7),]),type='l',lty=c(1,2,2),ltw=c(2,1,1),col = "black",main = titre, ylab= "Nombre de jour",xlab="Jour année")
  fichierPlot = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/PlotTotal",sp,an,sep="")
  savePlot(fichierPlot,"png")
       cat(" ==> ",fichierPlot,"\n")
  
  titre = paste("Plot Before",sp,an,sep=" ")
 #x11()
  matplot(t(tab[c(2,5,8),]),type='l',lty=c(1,2,2),ltw=c(2,1,1),col = "black",main = titre, ylab= "Nombre de jour",xlab="Jour année")
  fichierPlot = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/PlotBefore",sp,an,sep="")
  savePlot(fichierPlot,"png") 
      cat(" ==> ",fichierPlot,"\n")
   
  titre = paste("Plot After",sp,an,sep=" ")
#  x11()
  matplot(t(tab[c(3,6,9),]),type='l',lty=c(1,2,2),ltw=c(2,1,1),col = "black",main = titre, ylab= "Nombre de jour",xlab="Jour année")
  fichierPlot = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total duration/PlotAfter",sp,an,sep="")
  savePlot(fichierPlot,"png")
        cat(" ==> ",fichierPlot,"\n")
 flush.console()
  }


Stop_overDurationMultiAn.boxplot= function(sp)
{

  for(an in 2003:2007)
  {
    tabAn = read.sodaResult(sp,an)
    if ( an == 2003)
    {
      tabMoyenne = tabAn[1,ncol(tabAn)]
      tabMediane = tabAn[4,ncol(tabAn)]
      tab95sup = tabAn[7,ncol(tabAn)]
      tab95inf = tabAn[10,ncol(tabAn)]
    }
    else
    {
      tabMoyenne = c(tabMoyenne,tabAn[1,ncol(tabAn)])
      tabMediane = c(tabMediane,tabAn[4,ncol(tabAn)])
      tab95sup = c(tab95sup,tabAn[7,ncol(tabAn)])
      tab95inf = c(tab95inf,tabAn[10,ncol(tabAn)])
    }
  }
  #tabMoyenne = as.data.frame(tabMoyenne)
  mybarcol <- "lightgreen"
  erreurCol = "darkgreen"
  titre = paste("Stop Over Duration",sp,sep=" : ")
  #tabMoyenne = as.matrix(tabMoyenne)
  names(tabMediane)= 2003:2008

  mp <- barplot(tabMediane, beside = TRUE, col = mybarcol, main = titre, 
        border = mybarcol,
        cex.names = 0.8,
        #space = 0.5,
        ylim=c(0,max(tab95sup)))
  arrows(mp, tab95sup, mp, tab95inf, col = erreurCol, lwd = 1.5, code=3,angle=90,length=0.05)
  
   fichierPlot = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"barplotSOD",sep="")
   savePlot(fichierPlot,"png")

   cat(" ==> ",fichierPlot,"\n")
   flush.console()
  
}


Stop_overDurationMultiSpAn.boxplot= function(especes,an)
{

  for(sp in especes)
  {
    tabAn = read.sodaResult(sp,an)
    if ( sp == especes[1])
    {
      tabMoyenne = tabAn[1,ncol(tabAn)]
      tabMediane = tabAn[4,ncol(tabAn)]
      tab95sup = tabAn[7,ncol(tabAn)]
      tab95inf = tabAn[10,ncol(tabAn)]
    }
    else
    {
      tabMoyenne = c(tabMoyenne,tabAn[1,ncol(tabAn)])
      tabMediane = c(tabMediane,tabAn[4,ncol(tabAn)])
      tab95sup = c(tab95sup,tabAn[7,ncol(tabAn)])
      tab95inf = c(tab95inf,tabAn[10,ncol(tabAn)])
    }
  }
  #tabMoyenne = as.data.frame(tabMoyenne)
  mybarcol <- "lightgreen"
  erreurCol = "darkgreen"
  titre = paste("Stop Over Duration",sp,sep=" : ")
  #tabMoyenne = as.matrix(tabMoyenne)
  names(tabMediane)= especes

  mp <- barplot(tabMediane, beside = TRUE, col = mybarcol, main = titre, 
        border = mybarcol,
        cex.names = 0.8,
        #space = 0.5,
        ylim=c(0,max(tab95sup)))
  arrows(mp, tab95sup, mp, tab95inf, col = erreurCol, lwd = 1.5, code=3,angle=90,length=0.05)
  
   fichierPlot = paste("C:/DataAggeliki/etude/data/CMR/t=1d/",sp,"barplotSOD",sep="")
   savePlot(fichierPlot,"png")

   cat(" ==> ",fichierPlot,"\n")
   flush.console()
  
}



#__________________________________________________________________
# MAIN PROGRAM


especes = c("ACRSCH","ACRSCI","LUSSVE")
#especes = "ACROLA"
annees = 2003:2007
#annees = 2004:2007

#sp =  "ACRSCH"
#an = 2004
#
#tableauSODA = read.soda(sp,an)
#histoSODA(sp,an)
#

for(sp in especes)
{ 
  for(an in annees)
  {
#    tableauSODA = read.soda(sp,an)
#    histoSODA(sp,an)
#    Stop_overDuration.matplot(sp,an)
#

  }
Stop_overDurationMultiAn.boxplot(sp)  
}

especes = "ACROLA"
annees = 2004:2007
for(sp in especes)  Stop_overDurationMultiAn.boxplot(sp)  

especes = c("ACRSCH","ACRSCI","LUSSVE")
Stop_overDurationMultiSpAn.boxplot(especes,2008)

