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
                      typeInit = "moyenne",ressourceDep = TRUE,paramModifNom=vector(),
                      parmaModifValeur=vector(),serie=0,nb=0)
{
  op <- options()
  options(scipen=3)
  #browser()
  fichierParam = paste(repertoire,"communauteInput/parametresInputR.csv",sep="")
  param = read.csv(fichierParam)

  if(length(paramModifNom)>0) 
  for(i in 1:length(paramModifNom))
    param[which(param$Variable == paramModifNom[i]),3]= parmaModifValeur[i]
    
    id_simul = format(Sys.time(), "%H%M%S")# format(Sys.time(), "%Y-%m-%d%H%M%S")
    print(id_simul)
     param[which(param$Variable == "{idSimulNUM}"),3]= id_simul
    
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
   #  
    valeur = ifelse((as.numeric(param[,3]) %% 1 == 0), round(valeur), valeur)
       
   browser()      


  
  
  listParamPere = c("{init_nbHabitat}","{init_nbMicroHabitat}","{init_nbRessourceAlimentaire}")
  listParamDependant = c("{initVB_nbHabitatParSp}","{initVB_nbMicroHabitatParSp}","{initVB_nbRessourceAlimentaireParSp}")
  for (i in 1:length(listParamDependant))
  {
     valeur[which(as.character(param$Variable)==listParamDependant[i])] = round(runif(1,1,valeur[which(as.character(param$Variable)==listParamPere[i])]))
  }
 
 
 
  tabParam = data.frame(as.character(valeur,nsmall=0,scientific = F),param$Variable)  
  }  
 # browser()     
       
  fichierSortie = paste(repertoire,"communauteInput/parametresInput.txt",sep="")
  write.table(tabParam,fichierSortie,sep=" ",dec=".",col.names = FALSE,row.names = FALSE,quote = FALSE)
#  cat("<-- ",fichierSortie," \n")
#  flush.console()
  options(op) 
  return(id_simul)
}


modelDelphi  = function(repertoire="D:/Recherche/ModelDelphi/" )
{
  modele = "ModelCommuntyLightMigrationAgg"
  commande = paste(repertoire,modele,".exe",sep="")
  system(commande,ignore.stderr = TRUE,show.output.on.console=FALSE)
 }

lectureFichierSortieDelphi.pop = function(repertoire,serie,param,repet)
{
  idParam = paste(format(Sys.time(), "%y-%m-%d"),"-",param,sep="")
  nomFichier  = paste(repertoire,"CommunauteResultats/",idParam,"/",idParam,"Meca-Pop",repet,".csv",sep="")
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
  rownames(tabOut) = tabOut[,1]
  tabOut = tabOut[,-1]
  tabOut = apply(tabOut,2,as.numeric)

  return(tabOut)
  
}
  
lectureFichierSortieDelphi.parametre = function(repertoire,serie,param,repet)
{
  idParam = paste(format(Sys.time(), "%y-%m-%d"),"-",param,sep="")
  nomFichier  = paste(repertoire,"CommunauteResultats/",idParam,"/",idParam,"Meca-Pop",repet,".csv",sep="")
  tab = read.csv(nomFichier)
  finTab = min(which(tab[,1]=="annee"))+1
  ncolTab = ncol(tab)
  tabOut = tab[1:debutTab,1:ncolTab]
  return(tabOut)
}

resumeSortie = function(repGlobal="D:/Recherche/ModelDelphi/",parametreNomID,parametreValeur,resumeDetailler = FALSE, supprimeSortieDelphi = TRUE)
{
  parametreNom = paste(format(Sys.time(), "%y-%m-%d"),"-",parametreNomID,sep="") 
  repertoireSortie = paste(repGlobal,"CommunauteResultats/",sep="")
  dir.create(repertoireSortie, showWarnings = FALSE)
  cat("[CREATE] :",repertoireSortie,"\n")
  
  repertoirePlot = paste(repertoireSortie,"Graphes/",sep="")
  dir.create(repertoirePlot, showWarnings = FALSE)
  cat("[CREATE] :",repertoirePlot,"\n")

  #browser()
  fichierParamGlobale = paste(repertoireSortie,parametreNom,".csv",sep="")
  repertoire =  paste(repGlobal,"CommunauteResultats/",parametreNom,"/",sep="")
  parametre = read.csv(paste(repertoire,parametreNom,"paramatre.csv",sep="")) 
  nbRep = parametre$init_nbSimul[1]
  nbSp = parametre$init_nbSp[1]
  for(n in 1:nbRep)
    {
      cat(n," ")
      if (n%% 20 == 0) cat("\n") 
      flush.console()
      nomFichier = paste(repertoire,parametreNom,"Meca-Pop",n,".csv",sep="")
      tab= lectureFichierSortieDelphi.pop(repGlobal,serie,parametreNomID,n)
     # browser()
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
      

       if (file.exists(fichierParamGlobale)) write.table(parametre,fichierParamGlobale,sep=",",dec=".",append=TRUE,row.names = FALSE, col.names= FALSE) 
      else write.table(parametre,fichierParamGlobale,sep=",",dec=".",append=FALSE,row.names = FALSE, col.names= TRUE)
      if (supprimeSortieDelphi) unlink(repertoire,recursive=TRUE) 
      
        
    }
    cat("\n")
    
    if (resumeDetailler)
    {
      fichierSortie = paste(repertoireSortie,"Sum",parametreNom,".csv",sep="")
      write.csv(tabSum,fichierSortie)
      cat("<--",fichierSortie,"\n")   
  
      fichierSortie = paste(repertoireSortie,"NbPop",parametreNom,".csv",sep="")
      write.csv(tabNbPop,fichierSortie)
      cat("<--",fichierSortie,"\n")   
      flush.console()
    }
    if(nbRep > 1)
    {
    #browser()
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
                  apply(tabNbPop,1,quantile,0.975))#,     parametreValeur[p])
   
    variables = c("moy","sd","median","95sup","95inf")
    resum = as.data.frame(resum)
    colnames(resum) = c(paste("abond",variables,sep="_"),paste("presence",variables,sep="_"))
    
    fichierSortie = paste(repertoireSortie,"Resum",parametreNom,".csv",sep="")
    write.csv(resum,fichierSortie)
    cat("<--",fichierSortie,"\n")   
    flush.console()
  # browser()
    fichierPlot = paste(repertoirePlot,"Plot_Resum",parametreNom,sep="")
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
    mtext(paste("Abondance & Diversite",parametreNom,":",parametreValeur), 
      side = 3, las = 1, line = 1, adj = 0.5, col="darkred", cex = 1.2)  

    
    savePlot(fichierPlot,"png")
    cat("<==",fichierPlot,"\n")  
    }
    else
    {
      resum = cbind(tabSum,tabNbPop)
      resum = as.data.frame(resum)
      colnames(resum) = c("abond","presence")

      fichierSortie = paste(repertoireSortie,"Resum",parametreNom,".csv",sep="")
      write.csv(resum,fichierSortie)
      cat("<--",fichierSortie,"\n")   
      flush.console()
    # browser()
      fichierPlot = paste(repertoirePlot,"Plot_Resum",parametreNom,sep="")
      plot.new()
      m  = matrix(3:1,3,1)
      layout(m)
      layout.show(3)
       require(graphics)

      parDef <- par(mar=c(5,4,0,2))
      matplot(tab, type = 'l',lty = 1, col = palette(rainbow(nbSp)), 
        ann = FALSE,axes = TRUE)
      
        mtext("Abondance sp", 
        side = 2, las = 3, line = 3, adj = 0.5)
       mtext("Years", 
        side = 1, las = 1, line = 2, adj = 0.5)  
     
        box()
      plot.couleur = "darkred"#,"red","gray","gray")
      plot.typeLigne = 1#,1,3,3)
      plot.epaisseur = 1.5#,2,1,1)
      par(mar=c(0,4,0,2))
      plot(resum[,1],type = "l",
              col = plot.couleur, lty = plot.typeLigne, lwd = plot.epaisseur,
              ann = FALSE,axes = FALSE)
      axis(2)
      box()
      
      mtext("Abondance totale", 
        side = 2, las = 3, line = 3, adj = 0.5)
    
      par(mar = c(0,4,4,2))
      plot(resum[,2],type = "l",
              col = plot.couleur, lty = plot.typeLigne, lwd = plot.epaisseur,
              ann = FALSE,axes = FALSE)
      axis(2)
      box()
      mtext("Nb especes", 
        side = 2, las = 3, line = 3, adj = 0.5)
      mtext(paste("Abondance & Diversite",parametreNom,":",parametreValeur), 
        side = 3, las = 1, line = 1, adj = 0.5, col="darkred", cex = 1.2)  
    
      
      savePlot(fichierPlot,"png")
      cat("<==",fichierPlot,"\n")  
      
      

    }
     
    flush.console()
    par(parDef)
 
  


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
  nbBatch = 1
  cat("======================================\n")
  cat("|     BATCH MODELE DELPHI COMPET.     |\n") 
  cat("======================================\n")
  cat("nombre de repetitions : ", length(nbBatch)," (",min(nbBatch),"->",max(nbBatch),")\n") 
  cat("--------------------------------------------\n")
  
 # repBatch = paste(getwd(),"/",sep="")
 repBatch = "D:/Recherche/ModelDelphi/Batch/"
  serieBatch = 1
  #imm = c(seq(0,0.1,by=0.01),seq(0.1,1,by=0.1))

  
   cat("  - Init\n")
    flush.console()
    id_simul = initParam(typeInit = "moyenne",serie=serieBatch,nb=0,repertoire=repBatch)
    cat("  - Model\n")
     flush.console()
    modelDelphi(repertoire=repBatch)
    cat("  - Sorties\n")
     flush.console()
    resumeSortie(repGlobal=repBatch ,parametreNom = id_simul,parametreValeur = 1,resumeDetailler = FALSE, supprimeSortieDelphi = FALSE)

   cat("\n")
   flush.console()
}



#####################
# LANCEMENT
####################

#repertoire = "/home/romain/Model/community/BatchRomain/"

mainProgramme()


