#################################################
#   Presence absence 
#################################################


lecture = function()
{
  nomFich = "D:/Divers Boss/CIGNIG/cignigLL_AGE.csv"
  cat(" <---   ",nomFich,"\n")
  flush.console()
  return(read.csv(nomFich))
}

nettoyageBaguage = function(tabl)
{
  baguage = paste(unique(tabl$BAGUE[which(tabl$ACTION == "B")]))
  tabl.baguage = subset(tabl,tabl$BAGUE %in% baguage)
  return(tabl.baguage)  
}


preparePA = function(dataBag,val)
{
  d = dataBag
  bag = data.frame(BAGUE=unique(d$BAGUE))
  pa = matrix(data = val,nrow=length(unique(d$BAGUE)),ncol=(max(d$ANNEE)-min(d$ANNE)+1))
  colnames(pa) = sort(as.numeric(unique(d$ANNE)))
  d.b = d[which(d$ACTION=="B"),]
  ageBaguage = rep(NA,nrow(bag))
  for (i in 1:nrow(bag))
  {
    if (d.b$AGE[which(d.b$BAGUE == bag[i,1])]=="1A" |d.b$AGE[which(d.b$BAGUE == bag[i,1])]=="PUL" )
      ageBaguage[i] = 1
    else
      ageBaguage[i] = 2 
  }
  pa = as.data.frame(cbind(bag,ageBaguage,pa))
  return(pa)
}

presenceAnnee = function(tablBag,tablepresence)
{
  for (i in 1:nrow(tablBag))
  {
    tablepresence[
      which(tablepresence[,1]==tablBag$BAGUE[i]),
      which(colnames(tablepresence)==tablBag$ANNEE[i])
      ] = 1           
  }
 return(tablepresence)
}



presenceAnnee.age = function(tablBag,tablepresence)
{
  for (i in 1:nrow(tablBag))
  {
#browser()
    tablepresence[
      which(tablepresence[,1]==tablBag$BAGUE[i]),
      which(colnames(tablepresence)==tablBag$ANNEE[i])
      ] = 1           
  }
  
 return(tablepresence)
}


ageAnnee = function(tablBag,tablepresence)
{
  for (i in 1:nrow(tablBag))
  {
    tablepresence[
      which(tablepresence[,1]==tablBag$BAGUE[i]),
      which(colnames(tablepresence)==tablBag$ANNEE[i])
      ] = paste(tablBag$Age[i])
  }
 return(tablepresence)
}





actionAnnee = function(tablBag,tablepresence)
{
  for (i in 1:nrow(tablBag))
  {
    tablepresence[
      which(tablepresence[,1]==tablBag$BAGUE[i]),
      which(colnames(tablepresence)==tablBag$ANNEE[i])
      ] = paste(tablBag$ACTION[i])
  }
 return(tablepresence)
}



encounte = function(tabl)
{
#browser()
  tabl = tabl[,-1]
  histoire = paste(tabl[1,],collapse="")

  for (i in 2:nrow(tabl))
    histoire = c(histoire,paste(tabl[i,],collapse=""))
  
  unic.histoire = unique(histoire)
  nb.unic.histoire = rep(0,length(unic.histoire))
  
  for(i in 1: length(histoire))
    nb.unic.histoire[which(unic.histoire==histoire[i])]=
      nb.unic.histoire[which(unic.histoire==histoire[i])] + 1
  
  tablo = cbind(unic.histoire,nb.unic.histoire)
  
  return(tablo)
}

encounteGroupe = function(tabl)
{
#browser()
  tablHist = tabl[,-(1:2)]
  histoire = paste(tablHist[1,],collapse="")
  for (i in 2:nrow(tabl))
    histoire = c(histoire,paste(tablHist[i,],collapse=""))
  unic.histoire = unique(histoire)
  nb.unic.histoire1 = rep(0,length(unic.histoire))
  nb.unic.histoire = cbind(nb.unic.histoire1,nb.unic.histoire1)
  histoire = data.frame(HISTOIRE = histoire,AGE = tabl$ageBaguage)
  for(i in 1: length(unic.histoire))
    for( a in 1:2)
    {
print(a)
      sousTablAge = subset(histoire,AGE==a)
      nb.unic.histoire[i,a] = 
        length(which(sousTablAge$HISTOIRE==unic.histoire[i]))
print(nb.unic.histoire[i,a])
    }
  tablo = cbind(unic.histoire,nb.unic.histoire)
  return(tablo)
}


ecrire = function(tabl,nom,repertoire = "D:/Divers Boss/CIGNIG/")
{ 
  nomFich = paste(repertoire,nom,".txt",sep='')
  write.table(tabl,nomFich,quote=FALSE,row.names=FALSE)
  cat("   --->   ",nomFich,"\n")
  flush.console()
}



d = lecture()
d2 = nettoyageBaguage(d)
pa0 = preparePA(d2,0)
#edit(pa0)
#paNA = preparePA(d,NA)
#pa2 = presenceAnnee(d,pa0)
#pa3 = ageAnnee(d,paNA)
#pa4 = actionAnnee(d,paNA)
#ecrire(pa2,"presence")
#ecrire(pa3,"age")
#ecrire(pa4,"action")
#pa2.age = presenceAnnee.age(pa0)

#
#
enc = encounteGroupe(pa2)
 repertoire = "D:/Divers Boss/CIGNIG/"
 nomFich = paste(repertoire,"histoireCIGNIG_AGE",".INP",sep='')
write.table(enc,nomFich,quote=FALSE,sep = ' ', eol = ';\n',row.names=FALSE,col.names=FALSE)
#