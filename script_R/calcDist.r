

                                        # barreDefillement(): void
                                        # -------------------------
                                        # rélisation d'un affichage console
                                        #  de l'avancement du process
                                        # + t {INT} indice incrémenter dans le process (boucle)
                                        # + nbRep {INT} nombre de boucle (t max)

barreDefilement <-  function(t,nbRep) {

  if (t == 1) cat("|")
  if (nbRep >= 10){
    if (nbRep >= 100){
      deuxpc <- nbRep %/% (100/2)
      if(t %% deuxpc == 0)
        if( t %% (deuxpc * 25) == 0)cat("|")
        else
          if( t %% (deuxpc * 5) == 0)cat(":")
          else
            if(t == nbRep)  cat("|")
            else cat(".")
    }
    else{
      dixpc <- nbRep %/% (100/10)
      if(t %% dixpc == 0)
        
        if( t %% (dixpc * 5) == 0)
          if(t == nbRep)  cat("|")
          else
            cat(":")
      
        else cat(".")
    }
  }
  else
    if (t == nbRep) cat("|")
    else cat(".")
}


          



monsieurPropre <-  function(vec){
  vec <- gsub("-",";",vec)
  vec <- gsub("C;","",vec)
  return(vec)
}


calcdist <- function(A,remp){

  for (i in 1:length(A)){
    barreDefilement(i,length(A))
    tabRep <- matrix(as.numeric(strsplit(A,";")[[i]]),byrow=TRUE,length(strsplit(A,";")[[i]])/2,2)
  #  print(tabRep)
    cumul <- nrow(tabRep)
    start <- tabRep[1,]
    tabRep <- unique(tabRep)

    nbvisit <- nrow(tabRep)

    vecDist <- sqrt((tabRep[,1]-start[1])^2+
                    (tabRep[,2]-start[2])^2)

   # print(vecDist)
    distMax <- max(vecDist)

    classDist <- c(10,20,50,100,200,300,400,500,1000)

    for(d in 1:length(classDist)){

      dd <- classDist[d]
      if(d == 1){
        vecDistClass <-  length(vecDist[vecDist<= dd])
      }
      else {
        vecDistClass <-  c(vecDistClass,length(vecDist[vecDist<= dd])-sum(vecDistClass))
      }
      
    }
    
    sortieRep <- c(remp[i],cumul,nbvisit,distMax,vecDistClass)

    if(i == 1){
      sortie <- sortieRep
    }
    else {
      sortie <- rbind(sortie,sortieRep)
    }

  }
  row.names(sortie) <- 1:nrow(sortie)
  sortie <- data.frame(sortie)
  colnames(sortie) <- c("remp","cumul","nbVisit","distMax",paste("class",classDist,sep="_"))
  cat("  C'est Fini\n")
  return(sortie)
}
