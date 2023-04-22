###############################################
# Phénologie part analyse du STOP overduration
###############################################

read.tableCMR = function(an,sp)
{
  repertoire = paste("D:/Recherche/Donges/",sep="")
 # repertoire =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/",sep="")
  fichier = paste(repertoire,sp,an,".inp",sep="")
      
    d=read.table(fichier,header=F,colClasses="character")
    tab=d[,1]
    newTab = as.numeric(substr(tab,1,1))
    for (n in 2:nchar(tab[1]))
    newTab = cbind(newTab,as.numeric(substr(tab,n,n)))
    
 
  tab=newTab
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


read.parametre = function(an,sp)
{
  repertoire = paste("D:/Recherche/Donges/",sep="")
#  repertoire =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")

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
 #browser()
  for(l in 1:nrow(tab))
  {
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
  tabImport = read.tableCMR(an,sp)
  tabParametre = read.parametre(an,sp)
  vecjour1 = rep(261,4)
  jour1 = vecjour1[2004:2007 == an]

  
  
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


  
  
  matrice = t(rbind(colSums(tabPresence),colSums(tabPresenceSTD),colSums(tabPresenceSTDproba),colSums(tabPresenceSTDproba_t)))
  
  repertoire = paste("D:/Recherche/Donges/",sep="")
#  repertoire =  paste("C:/DataAggeliki/etude/data/CMR/t=1d/",an,"/",sp,"/total\ duration/",sep="")

  fichier = paste(repertoire,"tabPresence_",sp,an,".csv",sep="")
  write.csv(tabPresence,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  fichier = paste(repertoire,"tabPresenceSTD_",sp,an,".csv",sep="")
  write.csv(tabPresenceSTD,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  fichier = paste(repertoire,"tabPresenceSTDproba_",sp,an,".csv",sep="")
  write.csv(tabPresenceSTDproba,fichier)
  cat("       --> ",fichier,"\n")
  flush.console()

  fichier = paste(repertoire,"tabPresenceSTDprobaT_",sp,an,".csv",sep="")
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


#________________________________________________

#        Program Main

#________________________________________________

especes = c("ACRSCH","ACRSCI","LUSSVE","ACROLA")
annees = 2004:2007
especes = c("ACRSCH")
annees = 2005


for(sp in especes)
{
  cat(sp,"\n")
  for(an in annees)
  { 
    cat("  ",an ,"\n")   
    pheno.spAn(sp,an)  
  }
}


