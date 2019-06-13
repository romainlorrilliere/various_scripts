#Mirror

Hist.pradel=function(especes)
{
  for (sp in especes)
  {
    fichier = paste("C:/DataAggeliki/etude/R-MARK/",sp,"/Global/",sp,"Global.txt",sep="")
    fichierOUT = paste("C:/DataAggeliki/etude/R-MARK/",sp,"/Global/",sp,"PradelGlobal.txt",sep="")


    d=read.table(fichier,header=T,colClasses="character")
    tab=d[,1]
    newTab = as.numeric(substr(tab,1,1))
    for (n in 2:nchar(tab[1]))
    newTab = cbind(newTab,as.numeric(substr(tab,n,n)))
    
    
    for (r in 1:nrow(newTab))
      newTab[r,min(which(newTab[r,]==1))] = 0

    tab=newTab[,1]
      for (cc in 2:ncol(newTab))
        tab = cbind(newTab[,cc],tab) 

    pr=apply(tab,1,paste,collapse="")
    
    pr=cbind(pr,d[,2])
    colnames(pr)= c("ch","year")
    
    write.table(pr,fichierOUT,sep=" ",quote=FALSE,row.names=FALSE)
    
    cat("-->",fichierOUT,"\n")
    flush.console()
  }
}     
      
#__________________________________________________________________
# MAIN PROGRAM


especes = c("ACRSCH","ACRSCI","LUSSVE","ACROLA")
           
Hist.pradel(especes)


    