############################################
#     Moyenne pondére dans tableau reclassé
############################################



attach(ip)
ip = ip[order(CODE_PRA),]

for(pra in (unique(CODE_PRA))
{
  sousip=ip[which(CODE_PRA==pra),]
  moyenne =  weighted.mean(sousip$IP,sousip$AREA_COM)
  moyenneRep = rep(moyenne,nrow()sousip)
 if(pra == (unique(CODE_PRA)[1]))
    vecMoyenne = moyenneRep
  else
    vecMoyenne = c(vecMoyenne,moyenneRep)
}

ip = cbind(ip,vecMoyenne)

detach(ip)