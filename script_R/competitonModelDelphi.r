
competitionV1 = function(popBesoin,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   popVec = as.vector(popCourante[,dim(pop)[2]])
   a = rbinom(1,1,0.5)+1
   popVec[a]  = popVec[a]-1
   popCourante = t(rbind(popVec,popVec))
   pop = rbind(pop,popVec)
   besoin = popBesoin*popCourante
   besoinVar = colSums(besoin)
   surPop.l = max(besoinVar - ressource)
  }
  #print(pop)
  #matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}

competitionV2 = function(popBesoin,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   popVec = as.vector(popCourante[,dim(pop)[2]])
   a = rbinom(1,1,0.5)+1
   retrait = runif(1,1,max(1,min(surPop.l,popVec[a])))
   popVec[a]  = popVec[a]- retrait
   popCourante = t(rbind(popVec,popVec))
   pop = rbind(pop,popVec)
   besoin = popBesoin*popCourante
   besoinVar = colSums(besoin)
   surPop.l = max(besoinVar - ressource)
  }
  #print(pop)
  #matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}

tiragePondere = function(popVec)
{
  return(as.numeric(runif(1)>(popVec[1]/sum(popVec)))+1)
}



competitionV1bis = function(popBesoin,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  ressourceLim = which(besoinVar-ressource == max(besoinVar - ressource))
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   popVec = as.vector(popCourante[,dim(pop)[2]])
   p = tiragePondere(popVec)
   retrait = 1
   popVec[p]  = popVec[p]- retrait
   popCourante = t(rbind(popVec,popVec))
   pop = rbind(pop,popVec)
   besoin = popBesoin*popCourante
   besoinVar = colSums(besoin)
   surPop.l = max(besoinVar - ressource)
   ressoureLim = which(besoinVar-ressource == max(besoinVar - ressource))
  }
#  print(pop)
#  matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}

competitionV2bis = function(popBesoin,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  ressourceLim = which(besoinVar-ressource == max(besoinVar - ressource))
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   popVec = as.vector(popCourante[,dim(pop)[2]])
   p = tiragePondere(popVec)
   retrait = runif(1,1,max(1,min(surPop.l,popVec[p])))
   popVec[p]  = popVec[p]- retrait
   popCourante = t(rbind(popVec,popVec))
   pop = rbind(pop,popVec)
   besoin = popBesoin*popCourante
   besoinVar = colSums(besoin)
   surPop.l = max(besoinVar - ressource)
   ressoureLim = which(besoinVar-ressource == max(besoinVar - ressource))
  }
#  print(pop)
#  matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}


competitionV1bis1 = function(popBesoin,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  ressourceLim = which(besoinVar-ressource == max(besoinVar - ressource))
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   popVec = as.vector(popCourante[,dim(pop)[2]])
   p = tiragePondere(popVec)
   retrait = popBesoin[p,ressourceLim]
   popVec[p]  = popVec[p]- retrait
   popCourante = t(rbind(popVec,popVec))
   pop = rbind(pop,popVec)
   besoin = popBesoin*popCourante
   besoinVar = colSums(besoin)
   surPop.l = max(besoinVar - ressource)
   ressoureLim = which(besoinVar-ressource == max(besoinVar - ressource))
  }
#  print(pop)
#  matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}

competitionV2bis1 = function(popBesoin,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  ressourceLim = which(besoinVar-ressource == max(besoinVar - ressource))
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   popVec = as.vector(popCourante[,dim(pop)[2]])
   p = tiragePondere(popVec)
   retrait = runif(1,1,max(1,min(surPop.l,popVec[p])))*popBesoin[p,ressourceLim]
   popVec[p]  = popVec[p]- retrait
   popCourante = t(rbind(popVec,popVec))
   pop = rbind(pop,popVec)
   besoin = popBesoin*popCourante
   besoinVar = colSums(besoin)
   surPop.l = max(besoinVar - ressource)
   ressoureLim = which(besoinVar-ressource == max(besoinVar - ressource))
  }
#  print(pop)
#  matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}



competitionV1bis2 = function(popBesoin,pop,ressource)
{
  pop = data.frame(pop1=c(50),pop2=c(100))
  ressource = c(50,75)
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  ressourceLim = which(besoinVar-ressource == max(besoinVar - ressource))
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
   while(surPop.l > 0)
    {
    popVec = as.vector(popCourante[,dim(pop)[2]])
    p = tiragePondere(popVec)
    retrait = popBesoin[p,ressourceLim]
    popVec[p]  = popVec[p]- retrait
    popCourante = t(rbind(popVec,popVec))
    pop = rbind(pop,popVec)
    surPop.l = surPop.l - retrait
    }
    besoin = popBesoin*popCourante
    besoinVar = colSums(besoin)
    surPop.l = max(besoinVar - ressource)
    ressoureLim = which(besoinVar-ressource == max(besoinVar - ressource))
  }
#  print(pop)
#  matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}

competitionV2bis2 = function(besoinPop,pop,ressource)
{
  besoin = popBesoin
  popVec = pop[dim(pop)[1],]
  popCourante = t(rbind(popVec,popVec))
  besoin = popBesoin*popCourante
  besoinVar = colSums(besoin)
  surPop = max(besoinVar - ressource)
  ressourceLim = which(besoinVar-ressource == max(besoinVar - ressource))
  surPop.l = surPop
  i = 0
  while(surPop.l > 0)
  {
    while(surPop.l > 0)
    {
      popVec = as.vector(popCourante[,dim(pop)[2]])
      p = tiragePondere(popVec)
      retrait = runif(1,1,max(1,min(surPop.l,popVec[p])))*popBesoin[p,ressourceLim]
      popVec[p]  = popVec[p]- retrait
      popCourante = t(rbind(popVec,popVec))
      pop = rbind(pop,popVec)
      surPop.l = surPop.l - retrait
    }
    besoin = popBesoin*popCourante
    besoinVar = colSums(besoin)
    surPop.l = max(besoinVar - ressource)
    ressoureLim = which(besoinVar-ressource == max(besoinVar - ressource))
  }
#  print(pop)
#  matplot(pop,type='l')
  return(cbind(pop[dim(pop)[1],],dim(pop)[1]))
}





V = function(repetition=200)
{
  frameRep = as.data.frame(matrix(,1,6))
  colnames(frameRep) = c("V1_pop1","V1_pop2","V1_nbIt","V2_pop1","V2_pop2","V2_nbIt")
  frameBesoin = t(data.frame(pop1 = c(0.2,0.2),pop2 = c(0.8,0.8) ,row.names = c("var1","var2")))
  pop = data.frame(pop1=c(100),pop2=c(100))
  ressource = c(50,50)

  for(r in 1:repetition)
  {
     if (r %% 10 == 0) 
     {
      cat(r,' ')
      flush.console()
     }
     if (r %% 100 == 0)
     {
      cat("\n")     
      flush.console()
     }
     data1 = as.data.frame(competitionV1bis(frameBesoin,pop,ressource))
     data2 = as.data.frame(competitionV2bis(frameBesoin,pop,ressource))
     data12 = cbind(data1,data2)
     colnames(data12) = colnames(frameRep)
    frameRep = rbind(frameRep,data12)  
  }
  frameRepT = frameRep
  frameRep = as.data.frame(matrix(,1,6))
  colnames(frameRep) = c("V1_pop1","V1_pop2","V1_nbIt","V2_pop1","V2_pop2","V2_nbIt")
  for(r in 1:repetition)
  {
     if (r %% 10 == 0) 
     {
      cat(r,' ')
      flush.console()
     }
     if (r %% 100 == 0)
     {
      cat("\n")     
      flush.console()
     }
     data1 = as.data.frame(competitionV1bis1(frameBesoin,pop,ressource))
     data2 = as.data.frame(competitionV2bis1(frameBesoin,pop,ressource))
     data12 = cbind(data1,data2)
     colnames(data12) = colnames(frameRep)
     frameRep = rbind(frameRep,data12)  
  }
  colnames(frameRep) = paste('Bis',colnames(frameRep),sep='')
#  frameRepT = frameRep
  frameRepT = cbind(frameRepT,frameRep)
  frameRep = as.data.frame(matrix(,1,6))
  colnames(frameRep) = c("V1_pop1","V1_pop2","V1_nbIt","V2_pop1","V2_pop2","V2_nbIt")
  for(r in 1:repetition)
  {
     if (r %% 10 == 0) 
     {
      cat(r,' ')
      flush.console()
     }
     if (r %% 100 == 0)
     {
      cat("\n")     
      flush.console()
     }
     data1 = as.data.frame(competitionV1bis2(frameBesoin,pop,ressource))
     data2 = as.data.frame(competitionV2bis2(frameBesoin,pop,ressource))
     data12 = cbind(data1,data2)
     colnames(data12) = colnames(frameRep)
     frameRep = rbind(frameRep,data12)  
  }
  colnames(frameRep) = paste('Bis2',colnames(frameRep),sep='')
  frameRepT = cbind(frameRepT,frameRep)
  
  frameRepT = frameRepT[-1,]
  
#  boxplot(frameRepT[,c(1,2,4,5,7,8,10,11,13,14,16,17)])
  boxplot(frameRepT[,c(1,2,4,5,7,8,10,11)])
  cat("\n")
  
  return(frameRepT)
}




tab = V()
#tab = cbind(tab1,tab2)
#i=1
#colTab = i+c(0,3,6,9)
#boxplot(tab[,colTab])

