
spres <-  function(nbsp,nbresTot,nbres,nbreq = 5, alea = FALSE,show=FALSE){
  
  a <- sample(c(rep(0,nbresTot-nbres),rep(1,nbres)))
  if (show)print(a)
  n <-  0
  for (i in 1:nbsp){
    if (alea) 
      nbreqLoc <-  round(runif(1,1,2*(nbreq-1)))    
    else
      nbreqLoc <- nbreq

    req <- sample(a)[1:nbreqLoc] 
    n <- n+ prod(req)
    if (show) cat(i,n,req,"\n")
  }
  return(n)
}

test <-  function(){
  nbres <- 9:1
  nbsp <- c(100,300,600)#,1500,5000)
  dataTest <-  matrix(0,length(nbres),length(nbsp)*2)

  rownames(dataTest) <- nbres
  colnames(dataTest) <- paste(rep(c("f","t"),length(nbsp)),rep(nbsp,each=2),sep="")


  for (r in nbres){
    for (s in nbsp) {
      dataTest[which(nbres==r),which(colnames(dataTest)==paste("f",s,sep=""))] <-  spres(s,9,r,alea = FALSE)
      dataTest[which(nbres==r),which(colnames(dataTest)==paste("t",s,sep=""))] <-  spres(s,9,r,alea = TRUE)
    }
  }
  matplot(dataTest,type="l")
  print(dataTest)
  return(dataTest)

}
