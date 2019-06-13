
test.calMatrice <- function(affine=FALSE){
  vecTimeMed <- vector()
  vecTimeQuantUp <- vector()
  vecTimeQuantDown <- vector()
  vecn <- c(2,5,10,20,30,50,70,90,100,200,400,500,1000,2000,4000,6000,8000,10000,20000,40000,60000,80000,100000)
  for(n in vecn){
    cat(n,'')
    vecTime.r <- vector()
    for(r in 1:1000){
      h1 <- Sys.time()
      vec1 <- rnorm(n)
      vec2 <- rnorm(n)
      sum(vec1*vec2)
      h2 <- Sys.time()
      vecTime.r <- c(vecTime.r,h2-h1)
    }
    vecTimeMed <- c(vecTimeMed,median(vecTime.r))
    vecTimeQuantUp <- c(vecTimeQuantUp,quantile(vecTime.r,(1-0.025)))
    vecTimeQuantDown <- c(vecTimeQuantDown,quantile(vecTime.r,0.025))

  }
  cat("\n")

  vecTime <- data.frame(vecTimeMed,vecTimeQuantUp,vecTimeQuantDown)
  rownames(vecTime) <- vecn
  colnames(vecTime) <- paste(c("matMed","matQuantUp","matQuantDown"),affine,sep="")
  matplot(vecTime,type="l")

  nomfichier <- paste("./testPerformance/calMatrice_affine",affine,".csv",sep="")
  write.csv(vecTime,nomfichier)
  return(vecTime)
}


test.calBoucle <- function(affine=FALSE){
  vecTimeMed <- vector()
  vecTimeQuantUp <- vector()
  vecTimeQuantDown <- vector()
  vecn <- c(2,5,10,20,30,50,70,90,100,200,400,500,1000,2000,4000,6000,8000,10000,20000,40000,60000,80000,100000)
  for(n in vecn){
    cat(n,'')
    vecTime.r <- vector()
    for(r in 1:1000){

      h1 <- Sys.time()
      vec1 <- rnorm(n)
      vec2 <- rnorm(n)
      v <- 0
      for(i in 1:n)
      v <-  v+ (vec1[i] * vec2[i])

      h2 <- Sys.time()
      vecTime.r <- c(vecTime.r,h2-h1)
    }
    vecTimeMed <- c(vecTimeMed,median(vecTime.r))
    vecTimeQuantUp <- c(vecTimeQuantUp,quantile(vecTime.r,(1-0.025)))
    vecTimeQuantDown <- c(vecTimeQuantDown,quantile(vecTime.r,0.025))

  }
  cat("\n")

  vecTime <- data.frame(vecTimeMed,vecTimeQuantUp,vecTimeQuantDown)
  rownames(vecTime) <- vecn
  colnames(vecTime) <- paste(c("matMed","matQuantUp","matQuantDown"),affine,sep="")
  matplot(vecTime,type="l")

  nomfichier <- paste("./testPerformance/calBoucle_affine",affine,".csv",sep="")
  write.csv(vecTime,nomfichier)
  return(vecTime)
}

graphe <- function(){
  fichier <- paste("/home/romain/1_Recherche/script_R/testPerformance/calMatrice_affine",TRUE,".csv",sep="")
  tab <- read.csv(fichier)
  row.names(tab) <- tab[,1]
  tab <- tab[,-1]
  
  fichier <- paste("/home/romain/1_Recherche/script_R/testPerformance/calMatrice_affine",
                   FALSE,".csv",sep="")
  
  tab <- cbind(tab,read.csv(fichier)[,2:4])
  fichier <- paste("/home/romain/1_Recherche/script_R/testPerformance/calBoucle_affine",
                   TRUE,".csv",sep="")
  tab <- cbind(tab,read.csv(fichier)[,2:4])
  fichier <- paste("/home/romain/1_Recherche/script_R/testPerformance/calBoucle_affine",
                   FALSE,".csv",sep="")
  tab <- cbind(tab,read.csv(fichier)[,2:4])

  colnames(tab) <- paste(colnames(tab),rep(c("mat","boucl"),each=6),sep="_")
  matplot(rownames(tab),tab,type='l',lty=rep(c(1,2,2),4),col=rep(c("black","red","gray","orange"),each=3),lwd=rep(c(4,1,1,2,1,1),2))

  write.csv(tab,"/home/romain/1_Recherche/script_R/testPerformance/calPerf.csv")
  
}
