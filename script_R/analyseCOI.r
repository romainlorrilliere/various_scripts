##' @param repGlobal
##' @param serie
##' @param nbRep
##' @param parametreNom
##' @param parametreValeur
##' @param reel
##' @param idSimul
##' @param parametreNomSortie

##' @return ...
 library(prabclus)

mainCOI <- function(repGlobal=  "/home/romain/1_Recherche/Model/Batch/",serie= 2020,
                            nbRep=100,reel = TRUE,idSimulations=1:10){
  for(s in idSimulations){
    cat(s,"\n")
    resumeSortieCOI(repGlobal="/home/romain/1_Recherche/Model/Batch/",serie= serie,
              nbRep=nbRep,reel = TRUE,idSimul=s)
 }
}

resumeSortieCOI <- function(repGlobal=  "/home/romain/1_Recherche/Model/Batch/",serie= 2020,
                            nbRep=10,reel = TRUE,idSimul=1){
                                        # repSortie {PATH} repertoire des sorties de la fonction
 
  repSortie <- paste(repGlobal,"BatchModel",serie,"/CommunauteResultatsResum/",sep="")
                                        # fichierDataSortie {PATH.FILE} fichier de sortie
  fichierDataSortie <- paste("SummarydataCOI",serie,".csv",sep="")
  repData <- paste(repGlobal,"BatchModel",serie,"/",sep="")
  rmin <-  1
  for(r in rmin:nbRep){
                                        # barreDeffilement() {myRfunction}
                                        #     affichage d'une barre de défilement
    barreDefilement(r,nbRep)
                                        # tab {DATA.FRAME} tab de ma dynamique des population
                                        # lectureFichierSortieFreePascal.pop() {local}
    tab <- lectureFichierSortieFreePascal.pop(repertoire=repData,
                                              serie=serie,rep=r,idSimul)
                                        # tabPart {DATA.FRAME} tab des parts
                                        #   de chaques pop sur chaque ressources
                                        # lectureFichierSortieFreePascal.parametre() {local}
    tabPart <- lectureFichierSortieFreePascal.parametre(repertoire=repData,
                                                        serie=serie,rep=r,idSimul,TRUE,filtre="ALIM")
                                        # t {INT} (100) represente le tampon en fin de simul
                                        #     sur lequel on fait l'analyse
    tampon <- 100
    t.min <- max(nrow(tab)-nrow(tab)+1,nrow(tab)-tampon)
    t.max <- nrow(tab)
    vec.coi <- vector()
    for(t in t.min:t.max){
      vecAbond <- tab[t,]
      if (sum(vecAbond) != 0) vec.coi <- c(vec.coi,coi(t(tabPart),vecAbond))
      else vec.coi <- c(vec.coi,NA)
    }
    vecIndicateur <- c(mean(vec.coi,na.rm=TRUE),sd(vec.coi,na.rm=TRUE))
    if(reel){
                                        # tabParam {DATA.FRAME} data.frame des parametres de la simulation
                                        # lectureFichierSortieFreePascal.parametre() {local}
      tabParam <-  lectureFichierSortieFreePascal.parametre(repertoire=repData,
                                                            serie=serie,rep=r,
                                                            idSimul,FALSE)
                                        # indiceParam {INT} numéros de ligne correspondant aux valeur reel
      indiceParam <-  which(substring(rownames(tabParam),nchar(rownames(tabParam))-3,
                                      nchar(rownames(tabParam))) == "REEL")
                                        # vecParametreREEL {[FLOAT]} parametre reel calculer en cours de simulation
      vecParametreREEL  <-as.numeric(as.character(tabParam[indiceParam,1]))
                                            # update vecIndicateur
      vecIndicateur <- c(vecIndicateur,vecParametreREEL)
    }
                                        # uneSortieParam {DATA.FRAME} data.frame des indicateurs et parametres
                                        # initialisation et concatenation de uneSortieParam
    if(r == rmin) uneSortieParam <- vecIndicateur
    else uneSortieParam <- rbind(uneSortieParam,vecIndicateur)
  }
  tabParametreGlobalDelphi <-
    lectureFichierSortieFreePascal.parametreGlobal(repertoire=repData,
                                                   serie=serie,idSimul)

#  for(i in 1:length(parametreValeur))
#    uneSortieParam <- cbind(uneSortieParam,parametreValeur[i])
  ##  if(length(parametreValeur) == 1) uneSortieParam <- cbind(uneSortieParam,parametreValeur[1])
  ##  if(length(parametreValeur) == 2) uneSortieParam <- cbind(uneSortieParam,
  ##             parametreValeur[1],parametreValeur[2])

  colonnesNom <-  c(paste(
                          rep(c("COI"),2),
                          100,
                          c("moy","sd"),sep=""),
                    tabParametreGlobalDelphi[grep("reel",colnames(tabParametreGlobalDelphi))])
  colnames(uneSortieParam) <- colonnesNom
  
  fichierSortieComplet = paste(repSortie,fichierDataSortie,sep="")

  cat("\n-->",fichierDataSortie,"\n")
  if(file.exist(fichierDataSortie,repSortie))
    write.table(uneSortieParam,fichierSortieComplet,
                row.names=FALSE,col.names=FALSE,
                sep=",",dec=".",append=TRUE)
  else
    write.table(uneSortieParam,row.names=FALSE,
                col.names=TRUE,fichierSortieComplet,
                sep=",",dec=".",append=FALSE)

  return(uneSortieParam)
}



coi <- function(matNich,vecAbond){
  vecAbond <- as.numeric(vecAbond)
  matNichJ <- t(matNich)
  tabAbond1 <- matrix(rep(vecAbond,length(vecAbond)),length(vecAbond),length(vecAbond))
  tabAbond2 <- matrix(rep(vecAbond,length(vecAbond)),length(vecAbond),length(vecAbond),byrow=TRUE)
  tabAbond <- tabAbond1*tabAbond2
  tabJ <- -(jaccard(matNichJ)-1)
  return(sum(tabAbond*tabJ)/sum(tabAbond))
}


grapheCOI <- function(){
  vecSc.rmean <- c(1013,1014,2013,2014)
  vecSc.rsd <- c(1015,1016,2015,2016) 
  vecSc.kmean <- c(1017,1018,2017,2018)
  vecSc.ksd <- c(1019,1020,2019,2020)
  ##vecSc.res <- c(1023,1024,2023,2024)
  vecSc.res <- c(1025,1026,2025,2026)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res,vecSc.rsd,vecSc.ksd)
  listTab <- vector("list",5)
  vecNames <- c("Growth factor mean","Mean quantity of ressources","Number of resources","Growth factor sd","Carring capacity sd")
  vecIndicateur <- "COI"
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
  repSortie <- paste(repGlobal,"Graph1/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie)
  colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,12,12,13,3,1,7,5,10,11,12,3,1,7,5,10,11,12),5,7,byrow=TRUE)
  ## colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,10,11,12,3,1,7,5,10,11,12),5,7,byrow=TRUE)

  for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
   ## browser()
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }
  colnames(listTab[[1]])[6] <- "rmean"
  colnames(listTab[[2]])[6] <- "kmean"
  colnames(listTab[[3]])[6] <- "ress"
  colnames(listTab[[4]])[6] <- "rsd"
  colnames(listTab[[5]])[6] <- "ksd"
##browser()
  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:4] <- vecIndicateur
    colnames(listTab[[p]])[5] <- vecNames[p]
    
  }

  vecColorLine <- c("red","red","blue","blue")
  vecColorPoint <- c("pink","pink","lightblue","lightblue")
  vecTypePoint <- c(1,3,1,3)
  vecTypeLine <- c(1,2,1,2)
  vecEpaisseurLine <- c(2.5,4,2.5,4)
  library(graphics)

  ## browser()
  



  
  fileNamePlot = paste(repSortie,"mean.png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")

  png(file=fileNamePlot, width = 900, height = 1600,
      units = "px",pointsize = 25)
 # x11()
  par(new=TRUE,mar=c(0,0,0,0))
  
  m = matrix(c(0:3,0,4:6,0,7:9,0,10:12,rep(0,4)),5,4,byrow=TRUE)
  layout(m,widths=c(1,rep(3,3)),heights=c(rep(3,4),1))
  layout.show(12)
  
  for (i in 1:4){

  
    ymax = 0
    for(p in 1:3)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 1
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
     
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
    axis(2)
    mtext(vecIndicateur[i],
          side = 2, las = 3, line = 2,cex = 0.8)
    
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8) 
    }

    if (i == 2)    mtext("Indicator",
          side = 2, las = 3, line = 3,adj = -0.2,cex = 1.2)
    
    box()

    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    
    p <- 2
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),axes=FALSE)
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8)
      mtext("Environment disturbing", 
            side = 1, las = 1, line = 5, adj = 0.5,cex=1.2)
      
    }
    
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub),vec.groupe.sub,mean)
      meanTab <- matrix(0,length(meanList),2)
      ## tendance <- lm(vec.y.sub~vec.x.sub)
      
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
      ##    browser()
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)])     
      
    }
    
    p <- 3
    
    vec.x <- 9-(as.vector(as.matrix(listTab[[p]][,6])))
   # vec.x <- (as.vector(as.matrix(listTab[[p]][,6])))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    ##   browser()
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),xlim=c(0,9),axes=FALSE)
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8) 
    }
    box()


    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
#browser()
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in which(summary(vec.groupe.sub)>0)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      

     
      if (i>2)
        meanTab <- meanTab[which(meanTab[,2]>0),]

      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    

  }
  dev.off()

  fileNamePlot = paste(repSortie,"sd.png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")

  png(file=fileNamePlot, width = 600, height = 1600,
      units = "px",pointsize = 25)
  par(new=TRUE,mar=c(0,0,0,0))
  m  <- matrix(c(0,1,2,0,3,4,0,5,6,0,7,8,0,0,0),5,3,byrow=TRUE)
  layout(m,widths=c(1,rep(3,3)),heights=c(rep(4,4),1))
  layout.show(8)

  for(i in 1:4){
    ymax <- 0
    for(p in 4:5)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 4
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
    axis(2)
    mtext(vecIndicateur[i],
          side = 2, las = 3, line = 2, adj = 0.5,cex=0.8)
    
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8) 
    }
    if (i == 2)    mtext("Indicator",
          side = 2, las = 3, line = 3,adj = -0.2,cex = 1.2)
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){

      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    
    p <- 5
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=
         "",
         ylim=c(0,ymax),axes=FALSE)
    
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8)
      mtext("Environment disturbing", 
            side = 1, las = 1, line = 5,adj=2,cex=1.2)
    }
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
  }
  dev.off()
}
