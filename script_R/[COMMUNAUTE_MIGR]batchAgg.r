######################################################
# Réalisation du batch de simulation du modèle Delphi
######################################################


# 1 - initialisation du fichier de paramêtres
# 2 - simulation
# 3 - traitement des sorties


normaleTirage = function(vec,n=1)
  return(rnorm(n,vec[1],vec[2]) )

rangeTirage = function(vec,n=1)
  return(runif(n,vec[1],vec[2]))
                                                                                                                     

initParam = function( repertoire = "D:/Recherche/ModelDelphi/communauteInput/",
                      typeInit = "moyenne",ressourceDep = TRUE,paramModifNom=vector(),parmaModifValeur=vector(),serie=0,nb=0)
{
  op <- options()
  options(scipen=3)
  fichierParam = paste(repertoire,"communauteInput/parametresInputR.csv",sep="")
  param = read.csv(fichierParam)

  if(length(paramModifNom)>0) 
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
  {
    if (typeInit == "normal")
        valeur = ifelse(param$batch=="TRUE",apply(cbind(param$moyenne,param$ecartType),1,normaleTirage),as.numeric(param$moyenne))
    else 
      if(typeInit == "range")
        valeur = ifelse(param$batch=="TRUE",apply(cbind(param$rangeMin,param$rangeMax),1,rangeTirage),as.numeric(param$moyenne))    
      else
          cat(" !!! ERREUR TYPE INITIALISATION NON RECONNU !!!")
   #   browser() 
    valeur = ifelse((as.numeric(param[,3]) %% 1 == 0), round(valeur), valeur)
  }       
       
  listParamDependant = c("{initV_capaciteRessourceHabMoyDep}","{initV_capaciteRessourceHabSdDep}","{initV_capaciteRessourceMHabMoyDep}","{initV_capaciteRessourceMHabSdDep}","{initV_capaciteRessourceAlimMoyDep}","{initV_capaciteRessourceAlimSdDep}")
  listParamPere = c("{init_nbHabitat}","{initV_capaciteRessourceHabMoyDep}","{init_nbMicroHabitat}","{initV_capaciteRessourceMHabMoyDep}","{init_nbRessourceAlimentaire}","{initV_capaciteRessourceAlimMoyDep}")
       
  for (i in 1:length(listParamDependant))
  {
    if(i %% 2 != 0)
      valeur[which(as.character(param$Variable)==listParamDependant[i])] = 1/(as.numeric(as.character(valeur[which(as.character(param$Variable)==listParamPere[i])]))+1)
    else
      valeur[which(as.character(param$Variable)==listParamDependant[i])] = as.numeric(as.character(valeur[which(as.character(param$Variable)==listParamPere[i])]))/10

  }
  
  listParamPere = c("{init_nbHabitat}","{init_nbMicroHabitat}","{init_nbRessourceAlimentaire}")
  listParamDependant = c("{initVB_nbHabitatParSp}","{initVB_nbMicroHabitatParSp}","{initVB_nbRessourceAlimentaireParSp}")
  for (i in 1:length(listParamDependant))
  {
     valeur[which(as.character(param$Variable)==listParamDependant[i])] = round(runif(1,1,valeur[which(as.character(param$Variable)==listParamPere[i])]))
  }
 
 
 
  tabParam = data.frame(as.character(valeur,nsmall=0,scientific = F),param$Variable)  
    
 # browser()     
       
  fichierSortie = paste(repertoire,"communauteInput/parametresInput.txt",sep="")
  write.table(tabParam,fichierSortie,sep=" ",dec=".",col.names = FALSE,row.names = FALSE,quote = FALSE)
#  cat("<-- ",fichierSortie," \n")
#  flush.console()
  options(op) 
}


modelDelphi  = function(repertoire="D:/Recherche/ModelDelphi/" )
{
  modele = "ModelCommuntyLightMigrationAgg"
  commande = paste(repertoire,modele,".exe",sep="")
  system(commande,ignore.stderr = TRUE,show.output.on.console=FALSE)
  

}

lectureFichierSortieDelphi.pop = function(repertoire,serie,param,rep)
{


  nomFichier  = paste(repertoire,"CommunauteResultats/","complet",serie,"00",param,"/",serie,"00",param,"Meca-Pop",rep,".csv",sep="")
  tab = read.delim(nomFichier,header=FALSE,stringsAsFactors=FALSE, sep = "|")
  debutTab = min(which(substr(tab[,1],1,5)=="annee"))
  tab1=as.data.frame(tab[debutTab:nrow(tab),] ,default.stringsAsFactors=FALSE)
  tab1[,1]= as.character(tab1[,1])

  tab2 =as.vector(strsplit(tab1[1,1],split=",")[[1]])
 
  for(i in 2:nrow(tab1)){
    tab2 = rbind(tab2,as.vector(strsplit(tab1[i,1],split=",")[[1]]))
  }
# browser() 
  ncolTab =max(which(substr(tab2[1,],1,3)== "pop"))
  tabOut = tab2[,1:ncolTab]
  colnames(tabOut) = tabOut[1,]
  tabOut = tabOut[-1,]
  tabOut = apply(tabOut,2,as.numeric)

  return(tabOut)
  
}
  
lectureFichierSortieDelphi.parametre = function(repertoire,serie,param,rep)
{
  nomFichier  = paste(repertoire,"CommunauteResultats/","complet",serie,"00",param,"/",serie,"00",param,"Meca-Pop",rep,".csv",sep="")
  tab = read.csv(nomFichier)
  finTab = min(which(tab[,1]=="annee"))+1
  ncolTab = ncol(tab)
  tabOut = tab[1:debutTab,1:ncolTab]
  return(tabOut)
}

resumeSortie = function(repGlobal="D:/Recherche/ModelDelphi/" ,serie,nbRep,parametreNom,parametreValeur,resumeDetailler = FALSE, supprimeSortieDelphi = TRUE)
{
  repertoireSortie = paste(repGlobal,"CommunauteResultats/",parametreNom,"_",serie,"/",sep="")
  dir.create(repertoireSortie, showWarnings = FALSE)
  cat("[CREATE] :",repertoireSortie,"\n")
  
  repertoirePlot = paste(repertoireSortie,"Graphes/",sep="")
  dir.create(repertoirePlot, showWarnings = FALSE)
  cat("[CREATE] :",repertoirePlot,"\n")

#  browser()

  
  
  fichierParamGlobale = paste(repertoireSortie,parametreNom,"_",serie,".csv",sep="")
  

  for(p in 1:length(parametreValeur))
  {
     cat( "p:",p,"\n")
     for(n in 1:nbRep)
    {
      cat(n," ")
      if (n%% 50 == 0) cat("\n") 
      flush.console()
      repertoire =  paste(repGlobal,"CommunauteResultats/complet",serie,"00",p,"/",sep="")
      nomFichier = paste(repertoire,serie,"00",p,"Meca-Pop",n,".csv",sep="")
      tab= lectureFichierSortieDelphi.pop(repGlobal,serie,p,n)
      tabPresence = ifelse(tab<1,0,1)
      vecSum = rowSums(tab)
      vecNbPop = rowSums(tabPresence)
      
      if (n == 1)
      { 
        tabSum = vecSum
        tabNbPop = vecNbPop
      }
      else
      {
        tabSum = cbind(tabSum,vecSum)
        tabNbPop = cbind(tabNbPop,vecNbPop)
      }
      
      parametre = read.csv(paste(repertoire,serie,"00",p,"paramatre.csv",sep="")) 
       if (file.exists(fichierParamGlobale)) write.table(parametre,fichierParamGlobale,sep=",",dec=".",append=TRUE,row.names = FALSE, col.names= FALSE) 
      else write.table(parametre,fichierParamGlobale,sep=",",dec=".",append=FALSE,row.names = FALSE, col.names= TRUE)
      if (supprimeSortieDelphi) unlink(repertoire,recursive=TRUE) 
      
        
    }
    cat("\n")
    
    if (resumeDetailler)
    {
      fichierSortie = paste(repertoireSortie,"Sum",serie,"00",p,".csv",sep="")
      write.csv(tabSum,fichierSortie)
      cat("<--",fichierSortie,"\n")   
  
      fichierSortie = paste(repertoireSortie,"NbPop",serie,"00",p,".csv",sep="")
      write.csv(tabNbPop,fichierSortie)
      cat("<--",fichierSortie,"\n")   
      flush.console()
    }
    
    resum = cbind(
                  apply(tabSum,1,mean),
                  apply(tabSum,1,sd),
                  apply(tabSum,1,median),
                  apply(tabSum,1,quantile,0.025),
                  apply(tabSum,1,quantile,0.975),
                  apply(tabNbPop,1,mean),
                  apply(tabNbPop,1,sd),
                  apply(tabNbPop,1,median),
                  apply(tabNbPop,1,quantile,0.025),
                  apply(tabNbPop,1,quantile,0.975),
                  parametreValeur[p])
   
    variables = c("moy","sd","median","95sup","95inf")
    resum = as.data.frame(resum)
    colnames(resum) = c(paste("abond",variables,sep="_"),paste("presence",variables,sep="_"),parametreNom)
    
    fichierSortie = paste(repertoireSortie,"Resum",serie,"00",p,".csv",sep="")
    write.csv(resum,fichierSortie)
    cat("<--",fichierSortie,"\n")   
    flush.console()
  # browser()
    fichierPlot = paste(repertoirePlot,"Plot_Resum",serie,"00",p,sep="")
    plot.new()
    m  = matrix(c(2,1),2,1)
    layout(m)
    layout.show(2)
    
    plot.couleur = c("darkred","red","gray","gray")
    plot.typeLigne = c(2,1,3,3)
    plot.epaisseur = c(1.5,2,1,1)
    parDef <- par(mar=c(5,4,0,2))
    matplot(resum[,c(1,3,4,5)],type = "l",
            col = plot.couleur, lty = plot.typeLigne, lwd = plot.epaisseur,
            ann = FALSE,axes = TRUE)
    
    mtext("Nb indivus", 
      side = 2, las = 3, line = 3, adj = 0.5)
    mtext("Years", 
      side = 1, las = 1, line = 2, adj = 0.5)  

    par(mar = c(0,4,4,2))
    matplot(resum[,c(6,8,9,10)],type = "l",
            col = plot.couleur, lty = plot.typeLigne, lwd = plot.epaisseur,
            ann = FALSE,axes = FALSE)
    axis(2)
    box()
    mtext("Nb especes", 
      side = 2, las = 3, line = 3, adj = 0.5)
    mtext(paste("Abondance & Diversite",parametreNom,":",parametreValeur[p]), 
      side = 3, las = 1, line = 1, adj = 0.5, col="darkred", cex = 1.2)  

    
    savePlot(fichierPlot,"png")
    cat("<==",fichierPlot,"\n")   
    flush.console()
    par(parDef)
 
  }


}

resumeSortieParStep = function(repGlobal="D:/Recherche/ModelDelphi/" ,serie,nbRep,parametreNom,parametreValeur)
{

  
}


#####################
# MAIN
####################


mainProgramme = function()
{
  #browser()
  nbBatch = 21:300
  cat("======================================\n")
  cat("|     BATCH MODELE DELPHI COMPET.     |\n") 
  cat("======================================\n")
  cat("nombre de repetitions : ", length(nbBatch)," (",min(nbBatch),"->",max(nbBatch),")\n") 
  cat("--------------------------------------------\n")
  
  repBatch = paste(getwd(),"/",sep="")
  serieBatch = 2
  #imm = c(seq(0,0.1,by=0.01),seq(0.1,1,by=0.1))

  
  for(i in nbBatch)
  {
    cat(i," ")
    if (i %% 20 == 0) cat("\n")
    flush.console()
    initParam(typeInit = "range",serie=serieBatch,nb=i,repertoire=repBatch)
    modelDelphi(repertoire=repBatch)

  }
   cat("\n")
   flush.console()
}



#####################
# LANCEMENT
####################

repertoire = "/home/romain/Model/community/BatchRomain/"

#mainProgramme()

resumeSortie(repGlobal=repertoire ,serie = 2,nbRep = 100,parametreNom = "parametre",parametreValeur = 1:178,resumeDetailler = FALSE, supprimeSortieDelphi = FALSE)
