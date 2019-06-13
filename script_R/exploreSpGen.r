
testProtocoleTraject <- function(v,nomV,l){
  rep <- "/home/romain/1_Recherche/Model/community/CommunauteResultats/1015010/"
  a <- read.csv(paste(rep,"1015010-Pop1.csv",stringsAsFactors=FALSE))
  nbsp <- 100
  nbres <- 21
  tmax=2000
  nCol <-  c(2:(nbsp+1))
  debutSp <-1
  finSp <- 6+nbres
  tabsp <- as.data.frame(a[debutSp:finSp,nCol])
  rownames(tabsp) <- a[debutSp:finSp,1]
  debutDyn <- 8+nbres
  finDyn <- debutDyn + tmax
  dynTot <- a[debutDyn:finDyn,nCol]
  dim(dynTot)
  dynTot <- as.matrix(dynTot)
  for(j in 1:ncol(dynTot)){
    if (j == 1)
      matTot <-  as.numeric(as.character(dynTot[,j]))
    else 
      matTot <- cbind(matTot,as.numeric(as.character(dynTot[,j])))
  }
  colnames(matTot) <- colnames(dynTot)
  dynTot <-  matTot

  nomVarDynPop <- paste("abondTot_",nomV,v,sep="")
  assign(nomVarDynPop,dynPop(dynTot,tabsp,v,nomV,rep))
                

  nomVarRichSp <-  paste("richSp_",nomV,v,sep="")
  assign(nomVarRichSp,dynRichsp(dynTot,v,nomV,rep))
                

  nomVarCOI <-  paste("COI_",nomV,v,sep="")
  assign(nomVarCOI,COIt(dynTot,v,nomV,rep))
               

  nomVarCSI <-  paste("CSI_",nomV,v,sep="")
  assign(nomVarCOI,CSIt(dynTot,v,nomV,rep))

  if(length(l)<1){
    l <- list(abondTot=list(data.frame(get(nomVarDynPop))),
              richSp=list(data.frame(get(nomVarRichSp))),
              COI=list(data.frame(get(nomVarCOI))),
              CSI=list(data.frame(get(nomVarCSI))))
  }
  else {
    l$abonTot <- cbind(l$abontTot,get(nomVarDynPop))
    l$richSp <- cbind(l$richSp,get(nomVarRichSp))
    l$COI <- cbind(l$COI,get(nomVarCOI))
    l$CSI <- cbind(l$CSI,get(nomVarCSI))
  }

  pleindePlot(l,rep)

  return(l)
  
}




dynPop <- function(dynTot,tabs,v=0,nomV,rep){

  matplot(dynTot,type='l',lty=1)
  nomfichier <- paste(rep,nomV,"_dynTot_",v,".png",sep="")
  savePlot(nomfichier,"png")


  
  
  plot(as.numeric(as.character(tabsp[3,1:100])),dynTot[2000,1:100],
       main=paste("v",v),xlab="niv Generalist",ylab="abondance")
  nomfichier <- paste(rep,nomV,"_abondanceGene_",v,".png",sep="")
  savePlot(nomfichier,"png")
  
  plot(as.numeric(as.character(tabsp[4,1:100])),dynTot[2000,1:100],
       main=paste("v",v),xlab="nombre de besoin",ylab="abondance")
  nomfichier <- paste(rep,nomV,"_abondanceBesoin",v,".png",sep="")
  savePlot(nomfichier,"png")


  sumDyn <- rowSums(dynTot,na.rm=TRUE)
  return()
}



dynRichSp <- function(dynTot,v,nomV){
  dynPA <- dynTot/dynTot
  sumDynPA <- rowSums(dynPA,na.rm=TRUE)
  plot(sumDynPA,type='l',lty=1,main=paste("v",v))
  nomfichier <- paste(rep,nomV,"_dynPA_"v,".png",sep="")
  savePlot(nomfichier,"png")
  return(sumDynPA)
}





# --------------------------------------
# COIt
# ==========

COIt <- function(dynTot,v,n,nomV){

  n <- read.csv("/home/romain/1_Recherche/Model/community/CommunauteResultats/1015010/1015010-Niche1.csv",
                stringsAsFactors=FALSE)
  n <- n[,1:23]
  n$pop <- substr(n$pop,start=4,stop=nchar(n$pop))
  rownames(n) <- paste(n$pop,n$an,sep="_")
  
  vecCOI <- vector()
  
  for (t in 1:2000) {
    if (t %% 1000 == 0) cat("\n")
    if(t %% 100 == 0)  cat(t,"")
    
    na <- subset(n,an==t)[,3:23]
    ab <- dynTot[t,]
    vecCOI <- c(vecCOI,coi(na,ab))
  }
  plot(vecCOI,type='l',main=paste(nomV,v))
  nomfichier <- paste(rep,nomV,"_CSI_",v,".png")
  savePlot(nomfichier,"png")

  
  
  return(vecCOI)
}


CSIt <- function(dynTot,tabsp,v,nomV){
  vecGen <- as.numeric(tabsp[3,])
  vecCSI <- vector()
  for (t in 1:2000) {
    if (t %% 1000 == 0) cat("\n")
    if(t %% 100 == 0)  cat(t,"")
    
    ab <- dynTot[t,]
    vecCSI <- c(vecCSI,calculCSI(ab,vecGen))
  }
  plot(vecCSI,type='l',main=paste(nomV,v))
  nomfichier <- paste(rep,nomV,"_CSI_",v,".png")
  savePlot(nomfichier,"png")
  
  cat("\n")
  return(vecCSI)

}

pleindePlot <- function(l){
  v <- l$
  attach(v) 
  rep <- "/home/romain/1_Recherche/Model/community/CommunauteResultats/1015010/"
  matplot(v[,c(1,3,5,6,7)],type='l',lty=1,pch=20,cex=0.5,col=gray((1:6)/6))
  nomfichier <- paste(rep,"matplot.png")
  savePlot(nomfichier,"png")
  boxplot(v[1800:2000,])
  nomfichier <- paste(rep,"boxplot.png")
  savePlot(nomfichier,"png")
  hist(v0[1800:2000])
  nomfichier <- paste(rep,"histv0.png")
  savePlot(nomfichier,"png")
  hist(v1[1800:2000])
  nomfichier <- paste(rep,"histv1.png")
  savePlot(nomfichier,"png")
  hist(v2[1800:2000])
  nomfichier <- paste(rep,"histv2.png")
  savePlot(nomfichier,"png")
  hist(v4[1800:2000])
  nomfichier <- paste(rep,"histv4.png")
  savePlot(nomfichier,"png")
  hist(v5[1800:2000])
  nomfichier <- paste(rep,"histv5.png")
  savePlot(nomfichier,"png")
  hist(v10[1800:2000])
  nomfichier <- paste(rep,"histv10.png")
  savePlot(nomfichier,"png")
  hist(v19[1800:2000])
  nomfichier <- paste(rep,"histv19.png")
  savePlot(nomfichier,"png")
  

  plot(v0,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv0.png")
  savePlot(nomfichier,"png")
  plot(v1,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv1.png")
  savePlot(nomfichier,"png")
  plot(v2,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv2.png")
  savePlot(nomfichier,"png")
  plot(v4,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv4.png")
  savePlot(nomfichier,"png")
  plot(v5,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv5.png")
  savePlot(nomfichier,"png")
  plot(v10,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv10.png")
  savePlot(nomfichier,"png")
  plot(v19,type='b',pch=20,cex=0.5)
  nomfichier <- paste(rep,"plotTv19.png")
  savePlot(nomfichier,"png")




}








procedure.pascal.competition <- function(){
  N <- c(20,100,10)
  tx <- c(0.2,0.1,0.5)
  p <- N*tx
  sum(p)
  k <- 10
  ex <- sum(p)-k
  P2 <- N/sum(N)
  P2
  N2 <- P2 * k
  N2
  sum(N2)
  p2 <- N2*tx
  sum(p2)
  
  
  P3 <- p /sum(p)
  P3
  p3 <- P3 * k
  p3
  sum(p3)
  N3 <-  p3/tx
  sum(N3)
  sum(N3*tx)
  
  N3 <- (N*k)/(k+ex)
}

