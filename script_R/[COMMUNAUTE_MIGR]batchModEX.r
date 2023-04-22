##############################################################
## Réalisation du batch de simulation du modèle Free Pascal ##
##############################################################


## 1 - initialisation du fichier de paramêtres
## 2 - simulation
## 3 - traitement des sorties
##  a - fabrication de nouveaux fichier plus propre
##  b - fabrication des indicateur
##  c - affichage graphique

                                        # source de mes focntions génériques
source("/home/romain/1_Recherche/script_R/myRfunction.r")


                                        
                                        # initParam():void 
                                        #------------------------------------------------
                                        # initiation des parametres
                                        # fabrication du fichier .txt d'inportation dans le modèle Free Pascal

                                        # + repertoire: {PATH}("") repertoire d'excution
                                        # + repertoireParametre:{PATH}
                                        #  ("/home/romain/1_Recherche/Model/community/communauteInput/")
                                        #    chemin du model generique
                                        # + typeInit: {STRING} ("moyenne")
                                        #   type d'initialisation des paramètres du modèle
                                        #   "moyenne","normal","range"
                                        # + ressourceDep: {BOOL} (FALSE)
                                        #   les ressources sont elles dépendentes de leur ressource mère
                                        # + coVar: {FLOAT} (1) facteur de corélation entre les tirages by step
                                        # + varK: {INT} (0) boolean numérique variation de K
                                        # + varR: {INT} (0) boolean numérique variation de r
                                        # + paramModifNom: {[STRING]} ([]) vecteur des nom des parametres
                                        #     dont on modifie la valeur
                                        # + paramModifValeur: {[FLOAT]} vecteur des valeur des parametres
                                        # + serieBatch {INT} (0) identificateur batch
                                        # + idSimul {INT} (0)  identificateur Simulation

initParam <- function( repertoire = "",
                       repertoireParametre = "/home/romain/1_Recherche/Model/community/communauteInput/",
                      typeInit = "moyenne",ressourceDep = FALSE,coVar=1,varK= 0,varR= 0,
                      paramModifNom=vector(),paramModifValeur=vector(),serieBatch=0,idSimul=0)
{
                                        # op {LIST} options() {base}
  op <- options()
                                        # ??? histoire d'écriture des nombres ???
  options(scipen=3)
                                        # fichierParam : {STRING} construction fichier input parametre
  fichierParam <- paste(repertoireParametre,"parametresInputR.csv",sep="")
                                        # param {DATA.FRAME} parmetre par default
  param <- read.csv(fichierParam,stringsAsFactors=FALSE)

                                        # mise à jour des parametres de paramModifNom
  if(length(paramModifNom)>0) 
    for(i in 1:length(paramModifNom))
      param[which(param$Variable == paramModifNom[i]),3] <- paramModifValeur[i]

                                        # mise à jour de ID de la simulation
  if(serie != 0)
    param[which(param$Variable == "{idSimulNUM}"),3] <-
      as.numeric(paste(serie,"00",idSimul,sep=""))
  
                                        # mise à jour des valeur de varK,varR et coVar
  param[which(param$Variable =="{init_variation_r}"),3] <-varR
  param[which(param$Variable =="{init_variation_k}"),3] <-varK
  param[which(param$Variable =="{init_tauxCovariance}"),3] <-coVar

                                        # selection des parametres correspondant
                                        #     au bon type de parenté des ressource
  if (ressourceDep)
    param <- param[which(param$ressourceDependante==TRUE),]
  else
    param <- param[which(param$ressourceIndependante==TRUE),]

  
                                        # tirage des valeur des parmètres
                                        #   en fonction du type d'initialisation
                                        # moyenne : la valeur moyenne
                                        # normale : une valeur tiré dans une loi normale
                                        # range : un valeur tiré dans une loi uniforme
  if (typeInit == "moyenne")
    valeur <- data.frame(as.character(param$moyenne,nsmall=0,scientific = F),param$Variable)[,1]
  else  {
    if (typeInit == "normal")
                                        # normaleTirage() {myRfunction}
      valeur <- ifelse(param$batch=="TRUE",
                       apply(cbind(param$moyenne,param$ecartType),1,normaleTirage),
                       as.numeric(param$moyenne))
    else 
      if(typeInit == "range")
                                        # rangeTirage() {myRfunction}
        valeur <- ifelse(param$batch=="TRUE",
                         apply(cbind(param$rangeMin,param$rangeMax),1,rangeTirage),
                         as.numeric(param$moyenne))    
      else
        cat(" !!! ERREUR TYPE INITIALISATION NON RECONNU !!!")
    valeur = ifelse((as.numeric(param[,3]) %% 1 == 0), round(valeur), valeur)
  }

                                        # gestion des liens de parenté des ressources
  if(ressourceDep) {
    listParamDependant = c(
      "{initV_capaciteRessourceHabMoyDep}",
      "{initV_capaciteRessourceHabSdDep}",
      "{initV_capaciteRessourceMHabMoyDep}",
      "{initV_capaciteRessourceMHabSdDep}",
      "{initV_capaciteRessourceAlimMoyDep}",
      "{initV_capaciteRessourceAlimSdDep}")
    listParamPere = c(
      "{init_nbHabitat}",
      "{initV_capaciteRessourceHabMoyDep}",
      "{init_nbMicroHabitat}",
      "{initV_capaciteRessourceMHabMoyDep}",
      "{init_nbRessourceAlimentaire}",
      "{initV_capaciteRessourceAlimMoyDep}")
    for (i in 1:length(listParamDependant))  {
      if(i %% 2 != 0)
        valeur[which(as.character(param$Variable)==listParamDependant[i])] <-
          1/(as.numeric(as.character(valeur[which(as.character(param$Variable)==listParamPere[i])]))+1)
      else
        valeur[which(as.character(param$Variable)==listParamDependant[i])] <-
          as.numeric(as.character(valeur[which(as.character(param$Variable)==listParamPere[i])]))/10
    }
    
    listParamPere <- c("{init_nbHabitat}",
                       "{init_nbMicroHabitat}",
                       "{init_nbRessourceAlimentaire}")
    listParamDependant <- c("{initVB_nbHabitatParSp}",
                            "{initVB_nbMicroHabitatParSp}",
                            "{initVB_nbRessourceAlimentaireParSp}")
  }
                                        # tabParam {DATA.FRAME} des parametres
                                        #    au format input pour free pascal
  tabParam <- data.frame(as.character(valeur,nsmall=0,scientific = F),param$Variable)    
 
  fichierSortie <- paste(repertoire,"communauteInput/parametresInput.txt",sep="")
  write.table(tabParam,fichierSortie,sep=" ",dec=".",col.names = FALSE,row.names = FALSE,quote = FALSE)
                                        # ????
  options(op) 
}




                                        # model.FreePascal():void
                                        # -------------------------------------------------------------
                                        # procedure de lancement du model Free Pascal

                                        # + repertoire {STRING} ("") repertoire ou se trouve l'executable
                                        #      par default à la racine du modèle
                                        #      executable dans le même repertoire que le fichier .r
                                        # + nVersionModel {INT} numéros de la version du model free pascal

model.FreePascal <- function(repertoire="",nVersionModel){
                                        # model {STRING} nom du fichier executable
  modele <- paste("ModelCommunity_V",nVersionModel,sep="")
                                        # commande {STRING} commande d'execution
  commande <- paste("./",modele,sep="")
                                        # system() {base} execution
  system(commande,ignore.stderr = FALSE)
}





                                        # lectureFichierSortieFreePascal.pop():DATA.FRAME
                                        # ------------------------------------------------
                                        # extrait du fichier très complexe de sortie du modèle Free Pascal
                                        # un data frame de la dynamique des biomasses des populations

                                        # + repertoire : {PATH} repertoire générale du batch
                                        # + serie: {INT} identifiant du Batch
                                        # + idSimul: {INT} identifiant de la simulation
                                        # + rep:{INT} indentification
                                        # -> {DATA.FRAME}


lectureFichierSortieFreePascal.pop <-  function(repertoire="/home/romain/Model/community/BatchRomain/",
                                            serie=3,idSimul,rep){
                                        # nomFichier {PATH.FILE} construction du nom du fichier
  nomFichier <- paste(repertoire,"CommunauteResultats/",
                      serie,"0",idSimul,"/",
                      serie,"0",idSimul,"Meca-Pop",rep,".csv",sep="")
                                        # tab {DATA.FRAME}
                                        # importation du fichier en 1 tableau d'une colonne tout en STRING
  tab <- read.delim(nomFichier,header=FALSE,stringsAsFactors=FALSE, sep = "|")
                                        # debutTab {INT} recherche du mot clé "annee"
                                        #   pour trouver le début des données dynamiques
  debutTab <- min(which(substr(tab[,1],1,5)=="annee"))
                                        # fabrication du tableau en plusieur étapes
                                        # tab1 {DATE.FRAME} d'une seul colonne
  tab1 <- as.data.frame(tab[debutTab:nrow(tab),] ,default.stringsAsFactors=FALSE)
  tab1[,1] <- as.character(tab1[,1])

                                        # tab2 {DATA.FRAME} une colonne par pop
                                        # construction de tab2 ligne par ligne
  tab2 <- as.vector(strsplit(tab1[1,1],split=",")[[1]])
  
  for(i in 2:nrow(tab1)){
    tab2 <-  rbind(tab2,as.vector(strsplit(tab1[i,1],split=",")[[1]]))
  }
  
                                        # ncolTab {INT} nombre de colonne
                                        # recherche de la derniere colonne où "pop" est écrit
  ncolTab <- max(which(substr(tab2[1,],1,3)== "pop"))

                                        # tab {DATA.FRAME} lectute du même fichier
                                        # seul l'entête nous interresse ici la lecture est plus simple
  tab <-  read.csv(nomFichier,stringsAsFactor=FALSE,header=TRUE)
  
                                        # masse {[FLOAT]} vecteur de la masse par pop
  masse <- tab[which(tab[,1]=="v_biomasse"),2:ncolTab]

                                        # tabOut {DATA.FRAME} de sortie
  tabOut <- tab2[,1:ncolTab]
                                        # mise en forme de tabOut
  colnames(tabOut) <- tabOut[1,]
  tabOut <- tabOut[-1,]
  rownames(tabOut) <- tabOut[,1]
  tabOut <- tabOut[,-1]
                                        # tabOut = biomasse par pop
  tabOut <- apply(tabOut,2,as.numeric)
  for(p in 1:ncol(tabOut)){
    tabOut[,p] <- tabOut[,p]*as.numeric(masse[p])
  }

  return(tabOut)
}

                                        # lectureFichierSortieFreePascal.parametre():DATA.FRAME
                                        # ------------------------------------------------------
                                        # extrait du fichier très complexe de sortie du modèle Free Pascal
                                        # un data frame des parametres
                                        # + repertoire : {PATH} repertoire générale du batch
                                        # + serie: {INT} identifiant du Batch
                                        # + idSimul: {INT} identifiant de la simulation
                                        # -> {DATA.FARME}

lectureFichierSortieFreePascal.parametre <- function(repertoire,serie,param,rep,idSimul,fonctio=TRUE)
{
  nomFichier <- paste(repertoire,"CommunauteResultats/",
                      serie,"0",idSimul,"/",
                      serie,"0",idSimul,"Meca-Pop",rep,".csv",sep="")
   tab <- read.csv(nomFichier,row.names=1)
  if (fonctio){
    ncolTab <- max(which(substr(colnames(tab),1,3)== "pop")) 
    finTab <- max(which(substr(rownames(tab),1,8)== "txConso_"))
    debutTab <- min(which(substr(rownames(tab),1,8)== "txConso_"))
    tabOut <- tab[debutTab:finTab,1:ncolTab]
    tabPart <-  tabOut
    tabPart <- apply(tabPart,2,as.numeric)
    tabPart <- ifelse(is.na(tabPart),0,tabPart)
    tabPart <- as.data.frame(tabPart)
    rownames(tabPart) <- rownames(tabOut)
    tabOut <- tabPart
  }
  else {
    finTab <- min(which(rownames(tab)=="annee"))+1
    ncolTab <- ncol(tab)
    tabOut <- tab[1:finTab,1:ncolTab]
  }
 
  return(tabOut)
}



lectureFichierSortieFreePascal.parametreGlobal <- function(repertoire,serie,param,idSimul)
{
  nomFichier <- paste(repertoire,"CommunauteResultats/",serie,"0",idSimul,"/",serie,"0",idSimul,"parametre.csv",sep="")
  tab <- read.csv(nomFichier)
  return(tab)
}



                                        # resumeSortieParStep():DATA.FRAME
                                        # --------------------------------
                                        # resume les données sortie du modèle Free Pascal
                                        # et edit un fichier resumer
                                        # + repGlobal {PATH} chemin de la racine du modèle
                                        # + serieBatch {INT} identifiant batch
                                        # + nbRep {INIT} nombre de répétition monte carlos
                                        # + parametreNom {[STRING]} ([]) vecteur des nom des parametres
                                        #          dont on modifie la valeur
                                        # + parametreValeur  {[FLOAT]} vecteur des valeur des parametres
                                        # + reel {BOOL} (TRUE) utilisation des valeur reel des parametre
                                        #               les valeurs reel sont les valeurs calculers en cours de simulations
                                        #               part le modèle Free Pascal
                                        # + idSimul {INT}(0) Identificateur de la simulation 

                                        # 1 - ouverture du fichier de sortie
                                        # 2 - fabrication du resumer
                                        # 3 - fabrication des indcateurs


resumeSortieParStep <- function(repGlobal,serie,
                                nbRep,parametreNom,parametreValeur,
                                reel = TRUE,idSimul=0){
                                        # repSortie {PATH} repertoire des sorties de la fonction
  repSortie = paste(repGlobal,"CommunauteResultatsResum/",sep="")
                                        # fichierDataSortie {PATH.FILE} fichier de sortie
  fichierDataSortie <- paste("Summarydata",serie,".csv",sep="")
                                        # rmin {INT} début de l'incrément de répétion 
  rmin <-  1
  for(r in rmin:nbRep){
                                        # barreDeffilement() {myRfunction}
                                        #     affichage d'une barre de défilement
    barreDefilement(r,nbRep)
    
                                        # tab {DATA.FRAME} tab de ma dynamique des population
                                        # lectureFichierSortieFreePascal.pop() {local}
    tab <- lectureFichierSortieFreePascal.pop(repertoire=repGlobal,
                                          serie=serie,param=parametreValeur,rep=r,idSimul)
    
                                        # tabPart {DATA.FRAME} tab des parts
                                        #   de chaques pop sur chaque ressources
                                        # lectureFichierSortieFreePascal.parametre() {local}
    tabPart <- lectureFichierSortieFreePascal.parametre(repertoire=repGlobal,
                                                    serie=serie,param=parametreValeur,rep=r,idSimul,TRUE)

                                        # t {INT} (100) represente le tampon en fin de simul sur lequel on fait l'analyse
    t <- 100
                                        # tabIndicateur {DATA.FRAME} fichier d'importation dans fonction indicateurByStep()
    tabIndicateur <- tab[(nrow(tab)-t):nrow(tab),]
                                        # vecIndicateur {[FLOAT]} indicateurByStep() {local}
                                        #   calcul l'ensemble des indicateurs
                                        #      biomasse, nombre,
                                        #      simpsonBiodiv, simpsonBiodivFonctionnelpond
    vecIndicateur <- indicateurByStep(tabIndicateur,tabPart,t)

    if( reel){
      tabParam <-  lectureFichierSortieFreePacal.parametre(repertoire=repGlobal,
                                                    serie=serie,param=parametreValeur,rep=r,idSimul,FALSE)

      indiceParam <-  which(substring(rownames(tabParam),nchar(rownames(tabParam))-3,nchar(rownames(tabParam))) == "REEL")

      vecParametreREEL  <-as.numeric(as.character(tabParam[indiceParam,1]))
      

      vecIndicateur <- c(vecIndicateur,vecParametreREEL)
    }
    
    
    if(r == rmin) uneSortieParam <- vecIndicateur
    else uneSortieParam <- rbind(uneSortieParam,vecIndicateur)
  }

  tabParametreGlobalDelphi  <-lectureFichierSortieDelphi.parametreGlobal(repertoire=repGlobal,
                                                                         serie=serie,param=parametreValeur,idSimul)

  if(length(parametreValeur) == 1) uneSortieParam <- cbind(uneSortieParam,parametreValeur[1])
  if(length(parametreValeur) == 2) uneSortieParam <- cbind(uneSortieParam,
               parametreValeur[1],parametreValeur[2])
  if (reel){
    colnames(uneSortieParam) <-
      c(paste(
              rep(c("Nb","Biomasse",
                    "Shannon","ShannonPielou",
                    "Simpson","SimpsonBiodiv","SimpsonRecip",
                    "SimpsonFonct","SimpsonFonctBiodiv","SimpsonFonctRecip",
                    "SimpsonFonctPond","SimpsonFonctPondBiodiv","SimpsonFonctPondRecip"),each=2),
              100,
              rep(c("moy","sd"),4),sep=""),
        paste(parametreNom,"REEL",sep=""),
        parametreNom)
  }
  else {
    colnames(uneSortieParam) <-
      c(paste(
              rep(c("Nb","Biomasse",
                    "Shannon","ShannonPielou",
                    "Simpson","SimpsonBiodiv","SimpsonRecip",
                    "SimpsonFonct","SimpsonFonctBiodiv","SimpsonFonctRecip",
                    "SimpsonFonctPond","SimpsonFonctPondBiodiv","SimpsonFonctPondRecip"),each=2),
              100,
              rep(c("moy","sd"),4),sep=""),
        parametreNom)
  }
  

  fichierSortieComplet = paste(repSortie,fichierDataSortie,sep="")
  
  if(file.exist(fichierDataSortie,repSortie))
    write.table(uneSortieParam,fichierSortieComplet,
                row.names=FALSE,col.names=FALSE,
                sep=",",dec=".",append=TRUE)
  else
    write.table(uneSortieParam,row.names=FALSE,
                col.names=TRUE,fichierSortieComplet,
                sep=",",dec=".",append=FALSE)
}

                                        # indicateurByStep():MATRIX(FLOAT) 
                                        #-----------------------------------
                                        # calcul des indcateurs
                                        # possibilité de calculer les indicateurs suivants :
                                        #       Nb,Biomasse,
                                        #       Shannon,ShannonPielou,
                                        #       Simpson,SimpsonBiodiv,SimpsonRecip,
                                        #       SimpsonFonct,SimpsonFonctBiodiv,SimpsonFonctRecip,
                                        #       SimpsonFonctPond,SimpsonFonctPondBiodiv,SimpsonFonctPondRecip
                                        # pour cette version nous calculons :
                                        #       Nb,Biomasse,
                                        #       SimpsonBiodiv,SimpsonFonctPondBiodiv

                                        # + tabAbond: {DATA.FRAME} des biomasse des pop
                                        # + tabPart: {DATA.FRAME} des part en ressources des pop
                                        # + range: {INT} correspond au nombre d'années
                                        #  sur lesquelles sont fait les calculs
                                        # -> {MATRIX[FLOAT]}
                             
indicateurByStep <- function(tabAbond,tabPart,range){
  tab <-  tabAbond
                                        # vecNb {[INT]} nombre d'espèce par années
                                        # calculNbSp(){myRfunction}
  vecNb <- apply(tab,1,calculNbSp)
                                        # vecBiomasse {[FLOAT]} biomasse totale par années
                                        # calculBiomasse() {myRfunction}
  vecBiomasse <- apply(tab,1,calculBiomasse)
  
                                        # vecSimpson {[FLOAT]} indice de Simpson par année
                                        # calculSimpson() {myRfunction}
  vecSimpson <- apply(tab,1,calculSimpson,methode="")

                                        # vecSimpson.div {[FLOAT]} indice de diversité de Simpson par année
  vecSimpson.div <- apply(tab,1,calculSimpson,methode="diversite")

                                        # vecSimpson.rec {[FLOAT]} indice réciproque de Simpson par année
  vecSimpson.rec <- apply(tab,1,calculSimpson,methode="reciproque")

                                        # vecShannon {[FLOAT]} indice de Shannon par année
                                        # calculShannon() {myRfunction}
  vecShannon <- apply(tab,1,calculShannon,methode="")

                                        # vecShannon.pielou {[FLOAT]} indice de Shannon Pielou par année
  vecShannon.pielou <- apply(tab,1,calculShannon,methode="pielou")

                                        # vecSimpsonFonct {[FLOAT]} indice de Simpson fonctionnel par année
                                        # calculSimpsonFonct() {myRfunction}
  vecSimpsonFonct <- apply(tab,1,calculSimpsonFonct,tabPart,methode="",pond=FALSE)

                                        # vecSimpsonFonct.div {[FLOAT]} indice de diversité de Simpson
                                        #      fonctionnel par année
  vecSimpsonFonct.div <- apply(tab,1,calculSimpsonFonct,tabPart,methode="diversite",pond=FALSE)

                                        # vecSimpsonFonct.rec {[FLOAT]} indice réciproque de Simpson
                                        #       fonctionnel par année
  vecSimpsonFonct.rec <- apply(tab,1,calculSimpsonFonct,tabPart,methode="reciproque",pond=FALSE)

                                        # vecSimpsonFonctPond {[FLOAT]} indice de Simpson
                                        #      pondéré par les besoins en ressource
                                        #      fonctionnel par année
  vecSimpsonFonctPond <- apply(tab,1,calculSimpsonFonct,tabPart,methode="",pond=TRUE)

                                        # vecSimpsonFonct.div {[FLOAT]} indice de diversité de Simpson
                                        #      pondéré par les besoins en ressource
                                        #      fonctionnel par année
  vecSimpsonFonctPond.div <- apply(tab,1,calculSimpsonFonct,tabPart,methode="diversite",pond=TRUE)

                                        # vecSimpsonFonct.rec {[FLOAT]} indice réciproque de Simpson
                                        #       pondéré par les besoins en ressource
                                        #       fonctionnel par année
  vecSimpsonFonctPond.rec <- apply(tab,1,calculSimpsonFonct,tabPart,methode="reciproque",pond=TRUE)

                                        # tabnew {DATA.FRAME} de l'ensemble des vecteurs
  tabnew <-  cbind(vecNb,vecBiomasse,
                   vecShannon,vecShannon.pielou,
                   vecSimpson,vecSimpson.div,vecSimpson.rec,
                   vecSimpsonFonct,vecSimpsonFonct.div,vecSimpsonFonct.rec,
                   vecSimpsonFonctPond,vecSimpsonFonctPond.div,vecSimpsonFonctPond.rec)
                                        # vecMean {[FLOAT]} vecteur des moyennes
                                        #  colMeans() {base}
  vecMean <- colMeans(tabnew)
                                        # vecSd {[FLOAT]} vecteur des ecarts type
  vecSd <- apply(tabnew,2,sd)
                                        # vec {[FLOAT]} vecteur de sortie toutes les valeurs
                                        #  pour chaque indicateur moyenne puis sd
  vec <- c(
           vecMean[1],vecSd[1],
           vecMean[2],vecSd[2],
           vecMean[3],vecSd[3],
           vecMean[4],vecSd[4],
           vecMean[5],vecSd[5],
           vecMean[6],vecSd[6],
           vecMean[7],vecSd[7],
           vecMean[8],vecSd[8],
           vecMean[9],vecSd[9],
           vecMean[10],vecSd[10],
           vecMean[11],vecSd[11],
           vecMean[12],vecSd[12],
           vecMean[13],vecSd[13])
                                        # vec -> {MATRIX[FLOAT]} matrice de 1 ligne
  vec <- t(as.matrix(vec))
                                        # fabrication des nom de colonnes
  colnames(vec) = paste(
            rep(c("Nb","Biomasse",
                  "Shannon","ShannonPielou",
                  "Simpson","SimpsonBiodiv","SimpsonRecip",
                  "SimpsonFonct","SimpsonFonctBiodiv","SimpsonFonctRecip",
                  "SimpsonFonctPond","SimpsonFonctPondBiodiv","SimpsonFonctPondRecip"),each=2),
            range,
            rep(c("moy","sd"),4),sep="")
  return(vec)
}





resum.plot <- function(repGlobal,serie,nbColParametre){
  
  repSortie = paste(repGlobal,"CommunauteResultatsResum/",sep="")
  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")
  fichierData <- paste(repSortie,fichierDataSortie,sep="")

  d <-  read.csv(fichierData)
  for(p in 1:nbColParametre){
    vec.param <- d[,(ncol(d)-p+1)]
    nom.param <-  colnames(d)[(ncol(d)-p+1)]
    
    for(i in 1:(length(colnames(d))-nbColParametre)){
      titreY = colnames(d)[i]
      titreX = nom.param
      titre =  paste("serie:",serie,"\n ",titreY," / ",titreX,sep="")
      
      plot(vec.param,d[,i],
           type='p',pch=20,lwd=2,col="blue",
           main = titre,xlab = titreX,ylab=titreY)

      tendance=smooth.spline(vec.param,d[,i],df=6)
      lines(tendance,col="red",lwd=2,lty=1)

      fichier.plot = paste(repSortie,"s",serie,"_",titreY,"-",titreX,sep="")
      savePlot(fichier.plot,"png")
    }
  }
}


maketabMean <- function(tab,n){

 if(nrow(tab) %% n == 0){
   for( i in 1:(nrow(tab)%/%n)){
     vecMean <- apply(tab[(((i -1)*n+1):(i*n)),],2,mean)
     if(i == 1) tabMean <- vecMean
     else tabMean <- rbind(tabMean,vecMean)
   }
   rownames(tabMean) <- 1:nrow(tabMean)
   tabMean <- as.data.frame(tabMean)
   colnames(tabMean) <- colnames(tab)
 }
return(tabMean)
}

 

resum.plot3D <- function(repGlobal,serie,parametre1,parametre2){
  
  repSortie = paste(repGlobal,"CommunauteResultatsResum/",sep="")
  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")
  fichierData <- paste(repSortie,fichierDataSortie,sep="")


  d <-  read.csv(fichierData)

  library(graphics)

  par(bg="Black",col="white")

  vec.x1 <- d[,which(colnames(d)== parametre1)]
  vec.z1 <- d[,which(colnames(d)== parametre2)]
  titreX1 = parametre1
  titreZ1 = parametre2

  vec.z2 <- d[,which(colnames(d)== parametre1)]
  vec.x2 <- d[,which(colnames(d)== parametre2)]
  titreZ2 = parametre1
  titreX2 = parametre2

  
  for(i in 1:(ncol(d)-4)){

    palette(heat.colors(100))
   
    titreY =  colnames(d)[i]
    vec.y <- d[,i]
    
    titre =  paste("serie:",serie,"\n ",titreZ1,": ",titreX1," ",titreY,sep="")
    
    couleurValeur <-trunc((vec.z1 - min(vec.z1))/(max(vec.z1)-min(vec.z1))*100)
    couleurValeur <- (couleurValeur * -1)+max(couleurValeur)

    plot(vec.x1,vec.y,
         type='p',pch=20,cex=0.1,col=couleurValeur,
         main = titre,xlab = titreX1,ylab=titreY,col.axis="white",col.lab="white",col.main="white")

    tendance=smooth.spline(vec.x1,vec.y,df=6)
    lines(tendance,col="blue",lwd=2,lty=1)
 
  
    fichier.plot = paste(repSortie,"s",serie,"_",titreY,"-",titreX1,"+",titreZ1,sep="")
    savePlot(fichier.plot,"png")

    titre =  paste("serie:",serie,"\n ",titreZ2,": ",titreX2," ",titreY,sep="")
    
    couleurValeur <-trunc((vec.z2 - min(vec.z2))/(max(vec.z2)-min(vec.z2))*100)
    couleurValeur <- (couleurValeur * -1)+max(couleurValeur)

    plot(vec.x2,vec.y,
         type='p',pch=20,cex=0.1,col=couleurValeur,
         main = titre,xlab = titreX2,ylab=titreY,col.axis="white",col.lab="white",col.main="white")

    tendance=smooth.spline(vec.x2,vec.y,df=6)
    lines(tendance,col="blue",lwd=2,lty=1)
   
    fichier.plot = paste(repSortie,"s",serie,"_",titreY,"-",titreX2,"+",titreZ2,sep="")
    savePlot(fichier.plot,"png")
 }
}


test <- function(repertoire,serie,rep,idSimul){
  d <- lectureFichierSortieDelphi.pop(repertoire,serie,param=1,rep,idSimul)
  tabPart <- lectureFichierSortieDelphi.parametre(repertoire=repGlobal,
                                                  serie=serie,param=parametreValeur,rep=r,idSimul,FALSE)

  d <-  d[-1,]
  vecBiomasse <- apply(d,1,calculBiomasse)
  vecSimpson <- apply(d,1,calculSimpson,"divesite")
  vecShannon <- apply(d,1,calculShannon)
  vecSimpsonFonct <- apply(d,1,calculSimpsonFonct,tabPart,"reciproque",pond=FALSE)
  vecSimpsonFonctPond <- apply(d,1,calculSimpsonFonct,tabPart,"reciproque",pond=TRUE)
  
  ##matplot(cbind(d,vecBiomasse),t='l')
  plot(vecSimpson,t='l')
  ##plot(vecShannon,t='l')
  ##matplot(cbind(vecSimpsonFonct,vecSimpsonFonctPond),t='l')
}


estimDateFin <- function(h1,repTot,repNow){
  h2 <- Sys.time()
  diffSec <- difftime(h2,h1,units="secs")
  timeByStep <- diffSec / repNow
  timeToEnd <- (repTot - repNow) * timeByStep
  dateEnd <- format(Sys.time()+ as.numeric(timeToEnd,units="secs") ,format="%d/%m/%y %H:%M")
  timeEstimate <-  round(as.numeric(timeToEnd,units="hours"),1)
  if (timeEstimate < 1){
    timeEstimate <-  round(as.numeric(timeToEnd,units="mins"))
    cat("  * Estim:",timeEstimate,"minute(s) -> Fin batch:",dateEnd,"*\n") 
  
  }
  else  cat("  * Estim:",timeEstimate,"heure(s) -> Fin batch:",dateEnd,"*\n") 

}

compareScenar <- function(numScenar1,numScenar2,parametre1,parametre2){

  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"

  repSortie <- paste(repGlobal,"CompB",numScenar1,"B",numScenar2,"/",sep="")
##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie)
  
  repScenar <-  paste(repGlobal,"BatchModel",numScenar1,"/",sep="")
  fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",numScenar1 + 2,"00",sep="")
  fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
  d1 <-  read.csv(fichierData)

  d1 <-  data.frame(d1,sc=as.factor(numScenar1))

  repScenar <-  paste(repGlobal,"BatchModel",numScenar2,"/",sep="")
  fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",numScenar2 + 2,"00",sep="")
  fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
  d2 <-  read.csv(fichierData)
  d2 <-  data.frame(d2,sc=as.factor(numScenar2))

  d <- rbind(d1,d2)

  library(graphics)

  vecColorLine = c("black","red","blue","white","orange","green","chocolate","violet")

  vecColorPoint =  c("gray","pink","lightblue","ivory","lightyellow","lightgreen","sandybrown","lavender")
  
  par(bg="Black",col="white")

  vec.x1 <- d[,which(colnames(d)== parametre1)]
  vec.z1 <- d[,which(colnames(d)== parametre2)]
  titreX1 = parametre1
  titreZ1 = parametre2

  vec.z2 <- d[,which(colnames(d)== parametre1)]
  vec.x2 <- d[,which(colnames(d)== parametre2)]
  titreZ2 = parametre1
  titreX2 = parametre2

  serie <-  paste(numScenar1,numScenar2,sep="-")
  
  for(i in 1:(ncol(d)-5)){
   
    titreY <- colnames(d)[i]
    vec.y <- d[,i]
    
    titre <- paste("serie:",serie,"\n ",titreZ1,": ",titreX1," ",titreY,sep="")

    vecColor <- vecColorPoint[d$sc] 
    plot(vec.x1,vec.y,
         type='p',pch=20,cex=0.1,col=vecColor,
         main = titre,xlab = titreX1,ylab=titreY,col.axis="white",col.lab="white",col.main="white")
 
    vec.x1.sub1 <- vec.x1[which(d$sc==numScenar1)]
    vec.y.sub1 <- vec.y[which(d$sc==numScenar1)]
    vec.x1.sub2 <- vec.x1[which(d$sc==numScenar2)]
    vec.y.sub2 <- vec.y[which(d$sc==numScenar2)]

    tendance <- smooth.spline(vec.x1,vec.y,df=6)
    lines(tendance,col="gray",lwd=1,lty=2)
    tendance <- smooth.spline(vec.x1.sub2,vec.y.sub2,df=6)
    lines(tendance,col=vecColorLine[numScenar2],lwd=2,lty=1)
    tendance <- smooth.spline(vec.x1.sub1,vec.y.sub1,df=6)
    lines(tendance,col=vecColorLine[numScenar1],lwd=2,lty=1)

    
    fichier.plot = paste(repSortie,"s",serie,"_",titreY,"-",titreX1,"+",titreZ1,sep="")
    savePlot(fichier.plot,"png")

    titre =  paste("serie:",serie,"\n ",titreZ2,": ",titreX2," ",titreY,sep="")

    vecColor <- vecColorPoint[d$sc] 
 
    plot(vec.x2,vec.y,
         type='p',pch=20,cex=0.1,col=vecColor,
         main = titre,xlab = titreX2,ylab=titreY,col.axis="white",col.lab="white",col.main="white")

    vec.x2.sub1 <- vec.x2[which(d$sc==numScenar1)]
    vec.y.sub1 <- vec.y[which(d$sc==numScenar1)]
    vec.x2.sub2 <- vec.x2[which(d$sc==numScenar2)]
    vec.y.sub2 <- vec.y[which(d$sc==numScenar2)]
  
    tendance <- smooth.spline(vec.x2,vec.y,df=6)
    lines(tendance,col="gray",lwd=1,lty=2)
    tendance <- smooth.spline(vec.x2.sub2,vec.y.sub2,df=6)
    lines(tendance,col=vecColorLine[numScenar2],lwd=2,lty=1)
    tendance <- smooth.spline(vec.x2.sub1,vec.y.sub1,df=6)
    lines(tendance,col=vecColorLine[numScenar1],lwd=2,lty=1)

    fichier.plot = paste(repSortie,"s",serie,"_",titreY,"-",titreX2,"+",titreZ2,sep="")
  
    savePlot(fichier.plot,"png")
 } 

}

testTraject <-  function(rep,s,p,repet,id){
 tab <-  lectureFichierSortieDelphi.pop(repertoire = rep,serie = s,param = p,rep = repet,idSimul=id)
 x11()
 matplot(tab,type='l',pch=1,lty=1)
}

testTrajectSum <- function(rep,s,p,repet,id){
 tab <-  lectureFichierSortieDelphi.pop(repertoire = rep,serie = s,param = p,rep = repet,idSimul=id)
 vec = apply(tab1,1,sum)
 x11()
 plot(vec,type='l',pch=1,lty=1)
 
}


                                        # mainBatch():void
                                        # -----------------
                                        # void fonction MAIN qui réalise les batchs
                                        # + versionModel : {INT} version du modèle
                                        # + nBatch : {INT} numéros du batch
                                        # + nombreRep : {INT} (300)
                                        #        nombre de répétition du monté carlos
                                        # + repertoireInpout : {STRING} (fichier générique)
                                        #        fichier d'importation des paramètres
                                        # + paramModifSerieNom : {[STRING]}
                                        #        vecteur des nom des parametres testés
                                        # + codeParam : {[STRING]}
                                        #        vecteur des codes pour affichage écran
                                        # + paramValeur : {[[FLOAT]]}
                                        #        list des vecteurs des valeurs des parametres testé
                                        # + parametreNomSortie : {[STRING]}
                                        #        vecteur des nom des parametre en sortie
                                        # + parametreGraph {[STING]}
                                        #        vecteur des paramaetres affiché des les graphes
                                        # + valeurCor = {[FLOAT]}(1)
                                        #        facteur de corélation pour la variation temporelle
  
  
mainBatch <- function(versionModel,nBatch,nombreRep=300,
                      repertoireInput = "/home/romain/1_Recherche/Model/community/communauteInput/",
                      paramModifSerieNom,
                      codeParam,
                      paramValeur,
                      parametreNomSortie,
                      parametreGraph,
                      valeurCor = 1)
                          
  
  {
                                        # repBatch : {STRING} recupération du repertoire de travail
    repBatch <- paste(getwd(),"/",sep="")
                                        # nomBatch : {STRING} construction du nom (ref) du batch
    nomBatch <- paste(versionModel,"0",nBatch)
                                        # varParam : {CHAR} construction du indentifiant parametre
    varParam <- strsplit(codeParam[1]," ")[[1]][1]
                                        # nbJeuxParam : {INT} calcul du nombre de juex de parametre
    nbJeuxParam <-  length(paramValeur[[1]]) * length(paramValeur[[2]])
                                        # id : {INT} identifiant incrémenté du jeux de parametres
    id <-  1
    
    cat("===================================================\n")
    cat("|     BATCH MODELE DELPHI COMPET. MODEL ",nomBatch,"     |\n") 
    cat("===================================================\n")
                                        # dateDebut : {TIME} renseigne sur l'heure de début de simul
                                        # Sys.time() {base} renvoie heure machine
    dateDebut <- Sys.time()
                                        # affichage description batch
    cat("\n * Debut  Batch:",format(dateDebut,format="%d/%m/%y %H:%M"),"*\n")
    cat("\n nombre de jeux de parametres : ",nbJeuxParam,
        "\n nombre de repetition : ",nombreRep,
        "\n    ",codeParam[1],":(",min(paramValeur[[1]]),"->",max(paramValeur[[1]]),")",
        "\n    ",codeParam[2],":(",min(paramValeur[[2]]),"->",max(paramValeur[[2]]),")",
        "\n facteur de corelation: ",valeurCor) 
    cat("\n -----------------------------------------------\n")
                                        # mise à jour de dateDebut
    dateDebut <- Sys.time()
                                        # double boucle de scan des deux paramètre
                                        # p1 : {FLOAT} parametre 1
                                        # p2 : {FLOAT} parametre 2
    for(p1 in paramValeur[[1]]){
      for (p2 in paramValeur[[2]]){
                                        # numerosJeuxParam : {INT}
                                        #   calcul de son numéros parmis les nbJeuxParam
        numerosJeuxParam <- which(paramValeur[[2]] == p2) +
          (which(paramValeur[[1]] == p1)-1)*length(paramValeur[1]) 
        cat(" (",id,"/",nbJeuxParam,") ->  ",codeParam[1],": ",p1," | ",codeParam[2],": ",p2,"\n",sep="")

                                        # initParam(){local}
                                        #     fabrique le fichier d'input du modèle FPC
         initParam(typeInit = "moyenne",
                   serieBatch = nomBatch,repertoireParametre = repertoireInput, repertoire=repBatch,
                   paramModifNom = c(paramModifSerieNom,"{init_nbSimul}"),
                   paramModifValeur = c(p1,p2,nombreRep),idSimul = id,coVar=valeurCov,varK= as.numeric(varParam=="k"),varR= as.numeric(varParam=="r"))

        cat("     Sim     ")
                                        # model.FreePascal() {local}
                                        #   Lance le modèle FreePascal
        model.FreePascal(repertoire=repBatch,nModel)

        cat("\n     Summary ")

                                        # resumeSortieParStep() {local}
                                        #  réalise le résumer de la simulation
                                        #  calcul l'ensemble des indicateurs
                                        #      biomasse, nombre,
                                        #      simpsonBiodiv, simpsonBiodivFonctionnelpond
        resumeSortieParStep(repGlobal = repBatch ,
                            serie = serieBatch, nbRep = nombreRep,
                            parametreNom = parametreNomSortie,
                            parametreValeur = c(p1,p2),idSimul = id)
        cat("\n")

         estimDateFin(dateDebut,nbJeuxParam,id)
        id <-  id + 1
      }
    }
    
    cat("\n   ->  Plot    ")
    
    resum.plot3D(repGlobal = repBatch ,serie = serieBatch,
                 parametre1=parametreGraph[1],parametre2=parametreGraph[2])


    dateFin <- Sys.time()
    cat("\n * Fin  Batch:",format(dateFin,format="%d/%m/%y %H:%M"),"*\n",
        "--> Running time: ",difftime(dateFin,dateDebut,units="auto"),"\n")

    cat("\n================================\n")
    cat("|    FIN DU BATCH. MODEL ",nModel,"     |\n") 
    cat("================================\n")
    
    
  }

	


## compareScenar(numScenar1 = 5, numScenar2 = 6,parametre1="initV_capaciteRessourceAlimMoyREEL",parametre2="initV_capaciteRessourceAlimSdREEL")
## compareScenar(2,3,"initE_facteurCroissanceMoyREEL","initE_facteurCroissanceSdREEL")
