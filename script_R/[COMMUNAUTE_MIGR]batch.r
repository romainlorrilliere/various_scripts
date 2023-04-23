######################################################
# Réalisation du batch de simulation du modèle Delphi
######################################################


# 1 - initialisation du fichier de paramêtres
# 2 - simulation
# 3 - traitement des sorties


normaleTirage = function(vec,n=1)
  return(rnorm(n,vec[1],vec[2]) )

rangeTirage = function(vec,n=1)
  return(runif(n,vec[1],vec[2]) )
                                                                                                                     

initParam = function( repertoire = "D:/Recherche/ModelDelphi/communauteInput/",
                      typeInit = "moyenne",ressourceDep = TRUE,paramModifNom,parmaModifValeur,serie=0,nb=0)
{
  op <- options()
  options(scipen=3)
  fichierParam = paste(repertoire,"parametresInputR.csv",sep="")
  param = read.csv(fichierParam)
# browser() 
  for(i in 1:length(paramModifNom))
    param[which(param$Variable == paramModifNom[i]),3]= parmaModifValeur[i]
  if(serie != 0 & nb != 0)
    param[which(param$Variable == "{idSimulNUM}"),3]= paste(serie,"00",nb,sep="")
  if (ressourceDep)
      param = param[which(param$ressourceDependante==TRUE),]
  else
      param = param[which(param$ressourceIndependante==TRUE),]
      
  if (typeInit == "moyenne")
    tabParam = data.frame(as.character(param$moyenne,nsmall=0,scientific = F),param$Variable)
  else
    if (typeInit == "normal")
      tabParam = data.frame(as.character(apply(cbind(param$moyenne,param$ecartType),1,normaleTirage),nsmall=0,scientific = F),param$Variable)    
    else 
      if(typeInit == "range")
        tabParam = data.frame(as.character(apply(cbind(param$rangeMin,param$rangeMax),1,rangeTirage),nsmall=0,scientific = F),param$Variable)    
      else
        cat(" !!! ERREUR TYPE INITIALISATION NON RECONNU !!!")
  

  fichierSortie = paste(repertoire,"parametresInput.txt",sep="")
  write.table(tabParam,fichierSortie,sep=" ",dec=".",col.names = FALSE,row.names = FALSE,quote = FALSE)
#  cat("<-- ",fichierSortie," \n")
#  flush.console()
  options(op) 
}


modelDelphi  = function(repertoire="D:/Recherche/ModelDelphi/" )
{


  modele = "ModelCommuntyLightMigration"
  commande = paste(repertoire,modele,".exe",sep="")
  system(commande,ignore.stderr = TRUE,wait=FALSE)
  flush.console()
}




#####################


mainProgramme = function()
{
  #repBatch = paste(getwd(),"/",sep="")
  serieBatch = 1
  imm = c(seq(0,0.1,by=0.01),seq(0.1,1,by=0.1))
  for(i in 1:length(imm))
  {
    cat("\n -> ",i,"\n")
    flush.console()
    initParam(paramModifNom = c("{initE_txImmMoy}","{initE_txImmSd}"),parmaModifValeur=c(imm[i],imm[i]/10),serie=serieBatch,nb=i)#,repertoire=repBatch)
    modelDelphi()#(repertoire=repBatch)
    
  }

}

mainProgramme()