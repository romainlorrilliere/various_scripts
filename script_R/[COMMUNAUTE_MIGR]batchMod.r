##############################################################
## R�alisation du batch de simulation du mod�le Free Pascal ##
##############################################################


## 1 - initialisation du fichier de param�tres
## 2 - simulation
## 3 - traitement des sorties
##  a - fabrication de nouveaux fichier plus propre
##  b - fabrication des indicateur
##  c - affichage graphique

                                        # source de mes focntions g�n�riques
source("/home/romain/1_Recherche/script_R/myRfunction.r")




                                        # initParam():void 
                                        #------------------------------------------------
                                        # initiation des parametres
                                        # fabrication du fichier .txt d'inportation dans le mod�le Free Pascal

                                        # + repertoire: {PATH}("") repertoire d'excution
                                        # + versionM: {INT} version du mod�le
                                        # + serieBatch: {INT} version du batch
                                        # + repertoireParametre:{PATH}
                                        #  ("/home/romain/1_Recherche/Model/community/communauteInput/")
                                        #    chemin du model generique
                                        # + typeInit: {STRING} ("moyenne")
                                        #   type d'initialisation des param�tres du mod�le
                                        #   "moyenne","normal","range"
                                        # + ressourceDep: {BOOL} (FALSE)
                                        #   les ressources sont elles d�pendentes de leur ressource m�re
                                        # + coVar: {FLOAT} (1) facteur de cor�lation entre les tirages by step
                                        # + varK: {INT} (0) boolean num�rique variation de K
                                        # + varR: {INT} (0) boolean num�rique variation de r
                                        # + paramModifNom: {[STRING]} ([]) vecteur des nom des parametres
                                        #     dont on modifie la valeur
                                        # + paramModifValeur: {[FLOAT]} vecteur des valeur des parametres
                                        # + idSimul {INT} (0)  identificateur Simulation

initParam <- function( repertoire = "",versionM,serieBatch,
                      repertoireParametre = "/home/romain/1_Recherche/Model/community/communauteInput/",
                      typeInit = "moyenne",ressourceDep = FALSE,coVar=1,varK= 0,varR= 0,
                      paramModifNom=vector(),paramModifValeur=vector(),idSimul=0)
{
browser()
                                        # op {LIST} options() {base}
  op <- options()
                                        # ??? histoire d'�criture des nombres ???
  options(scipen=3)
                                        # fichierParam : {STRING} construction fichier input parametre
  fichierParam <- paste(repertoireParametre,"parametresInputR.csv",sep="")
                                        # param {DATA.FRAME} parmetre par default
  param <- read.csv(fichierParam,stringsAsFactors=FALSE)


  if(length(paramModifNom)>0) 
    for(i in 1:length(paramModifNom))
      param[which(param$Variable == paramModifNom[i]),3] <- paramModifValeur[i]

                                        # mise � jour de ID de la simulation
  if(serieBatch != 0){
    param[which(param$Variable == "{idSimulNUM}"),3] <- paste(serieBatch,idSimul,sep="0")
    param[which(param$Variable == "{idVersionNUM}"),3] <- versionM 
  }
  
                                        # mise � jour des valeur de varK,varR et coVar
  param[which(param$Variable =="{init_variation_r}"),3] <-varR
  param[which(param$Variable =="{init_variation_k}"),3] <-varK
  param[which(param$Variable =="{init_tauxCovariance}"),3] <-coVar

                                        # selection des parametres correspondant
                                        #     au bon type de parent� des ressource
  if (ressourceDep)
    param <- param[which(param$ressourceDependante==TRUE),]
  else
    param <- param[which(param$ressourceIndependante==TRUE),]

  
                                        # tirage des valeur des parm�tres
                                        #   en fonction du type d'initialisation
                                        # moyenne : la valeur moyenne
                                        # normale : une valeur tir� dans une loi normale
                                        # range : un valeur tir� dans une loi uniforme
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

                                        # gestion des liens de parent� des ressources
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
                                        # procedure de mise � jour et lancement du model Free Pascal

                                        # + repertoire {STRING} ("") repertoire ou se trouve l'executable
                                        #      par default � la racine du mod�le
                                        #      executable dans le m�me repertoire que le fichier .r
                                        # + nVersionModel {INT} num�ros de la version du model free pascal

model.FreePascal <- function(repertoire="",nVersionModel){

                                        # model {STRING} nom du fichier executable
  model <- paste("ModelCommunity_V",nVersionModel,sep="")
                                        # commande {STRING} commande d'update
  commande <- paste("cp /home/romain/1_Recherche/Model/community/",model," ",repertoire,sep="")#./Batch/BatchModel109/
                                        # system() {base} execution
  system(commande,ignore.stderr = FALSE)
                                        # commande {STRING} commande d'execution
  commande <- paste("./",model,sep="")
                                        # system() {base} execution
  system(commande,ignore.stderr = FALSE)
}





                                        # lectureFichierSortieFreePascal.pop():DATA.FRAME
                                        # ------------------------------------------------
                                        # extrait du fichier tr�s complexe de sortie du mod�le Free Pascal
                                        # un data frame de la dynamique des biomasses des populations

                                        # + repertoire : {PATH} repertoire g�n�rale du batch
                                        # + serie: {INT} identifiant du Batch
                                        # + idSimul: {INT} identifiant de la simulation
                                        # + rep:{INT} indentification
                                        # -> {DATA.FRAME}


lectureFichierSortieFreePascal.pop <-  function(repertoire="/home/romain/Model/community/BatchRomain/",
                                                serie=3,idSimul,rep){
                                        # nomFichier {PATH.FILE} construction du nom du fichier
## browser()
  nomFichier <- paste(repertoire,"CommunauteResultats/",
                      serie,"0",idSimul,"/",
                      serie,"0",idSimul,"Meca-Pop",rep,".csv",sep="")


  a <- read.csv(nomFichier,stringsAsFactors=FALSE)

  debutTab <- min(which(substr(a[,1],1,5)=="annee"))
  nbsp <- length(which(grep("pop",colnames(a))))
  nbres <- length(which(grep("res",colnames(a))))
  tmin <- as.integer(a[debutTab+1,1])
  tmax <- max(as.integer(a[,1]))
  
  nCol <-  c(2:(nbsp+1))
  debutDyn <- 8+nbres
  finDyn <- which(as.character)
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

##                                        # tab {DATA.FRAME}
##                                        # importation du fichier en 1 tableau d'une colonne tout en STRING
##  tab <- read.delim(nomFichier,header=FALSE,stringsAsFactors=FALSE, sep = "|")
##                                        # debutTab {INT} recherche du mot cl� "annee"
##                                        #   pour trouver le d�but des donn�es dynamiques
##  debutTab <- min(which(substr(tab[,1],1,5)=="annee"))
##                                        # fabrication du tableau en plusieur �tapes
##                                        # tab1 {DATE.FRAME} d'une seul colonne
##  tab1 <- as.data.frame(tab[debutTab:nrow(tab),] ,default.stringsAsFactors=FALSE)
##  tab1[,1] <- as.character(tab1[,1])
##
##                                        # tab2 {DATA.FRAME} une colonne par pop
##                                        # construction de tab2 ligne par ligne
##  tab2 <- as.vector(strsplit(tab1[1,1],split=",")[[1]])
##  
##  for(i in 2:nrow(tab1)){
##    tab2 <-  rbind(tab2,as.vector(strsplit(tab1[i,1],split=",")[[1]]))
##  }
##  
##                                        # ncolTab {INT} nombre de colonne
##                                        # recherche de la derniere colonne o� "pop" est �crit
##  ncolTab <- max(which(substr(tab2[1,],1,3)== "pop"))
##
##                                        # tab {DATA.FRAME} lectute du m�me fichier
##                                        # seul l'ent�te nous interresse ici la lecture est plus simple
##  tab <-  read.csv(nomFichier,stringsAsFactor=FALSE,header=TRUE)
##  
##                                        # masse {[FLOAT]} vecteur de la masse par pop
##  masse <- tab[which(tab[,1]=="v_biomasse"),2:ncolTab]
##
##                                        # tabOut {DATA.FRAME} de sortie
##  tabOut <- tab2[,1:ncolTab]
##                                        # mise en forme de tabOut
##  colnames(tabOut) <- tabOut[1,]
##  tabOut <- tabOut[-1,]
##  rownames(tabOut) <- tabOut[,1]
##  tabOut <- tabOut[,-1]
##                                        # tabOut = biomasse par pop
##  tabOut <- apply(tabOut,2,as.numeric)
##  for(p in 1:ncol(tabOut)){
##    tabOut[,p] <- tabOut[,p]*as.numeric(masse[p])
##  }

  return(dynTot)
}

                                        # lectureFichierSortieFreePascal.parametre():DATA.FRAME
                                        # ------------------------------------------------------
                                        # extrait du fichier tr�s complexe de sortie du mod�le Free Pascal
                                        # un data frame des parametres
                                        # + repertoire : {PATH} repertoire g�n�rale du batch
                                        # + serie: {INT} identifiant du Batch
                                        # + idSimul: {INT} identifiant de la simulation
                                        # -> {DATA.FARME}

lectureFichierSortieFreePascal.parametre <- function(repertoire,serie,rep,idSimul)
{
  nomFichier <- paste(repertoire,"CommunauteResultats/",
                      serie,"0",idSimul,"/",
                      serie,"0",idSimul,"Meca-Pop",rep,".csv",sep="")

  a <- read.csv(nomFichier,stringsAsFactors=FALSE)

  debutTab <- min(which(substr(a[,1],1,5)=="annee"))
  nbsp <- length(which(grep("pop",colnames(a))))
  nbres <- length(which(grep("res",colnames(a))))
  tmin <- as.integer(a[debutTab+1,1])
  tmax <- as.integer(nrow(a),1)
  
  nCol <-  c(2:(nbsp+1))
  debutSp <-1
  finSp <- 6+nbres
  tabsp <- as.data.frame(a[debutSp:finSp,nCol])
  rownames(tabsp) <- a[debutSp:finSp,1]

  ## tab <- read.csv(nomFichier,row.names=1)
## if (fonctio){
##   ncolTab <- max(which(substr(colnames(tab),1,3)== "pop")) 
##   finTab <- max(which(substr(rownames(tab),1,8)== "txConso_"))
##   debutTab <- min(which(substr(rownames(tab),1,8)== "txConso_"))
##   tabOut <- tab[debutTab:finTab,1:ncolTab]
##   tabPart <-  tabOut
##   tabPart <- apply(tabPart,2,as.numeric)
##   tabPart <- ifelse(is.na(tabPart),0,tabPart)
##   tabPart <- as.data.frame(tabPart)
##   rownames(tabPart) <- rownames(tabOut)
##   tabOut <- tabPart
## }
## else {
##   finTab <- min(which(rownames(tab)=="annee"))+1
##   ncolTab <- ncol(tab)
##   tabOut <- tab[1:finTab,1:ncolTab]
## }
## if(filtre == "")  return(tabOut)
## else{
##   tabOut <- tabOut[grep(filtre,rownames(tabOut)),]
##   return(tabOut)
## }

return(tabsp)
}

                                        # lectureFichierSortieFreePascal.SSIt():DATA.FRAME
                                        # ------------------------------------------------------
                                        # evolution du SSI realise au court du temps
                                        # pour calculer le CSI r�alis� au court du temps
                                        # -> {DATA.FARME}

lectureFichierSortieFreePascal.SSIt <- function(repertoire,serie,rep,idSimul)
{
  nomFichier <- paste(repertoire,"CommunauteResultats/",
                      serie,"0",idSimul,"/",
                      serie,"0",idSimul,"Meca-SSI",rep,".csv",sep="")

  tabSSI <- read.csv(nomFichier,stringsAsFactors=FALSE)
  nbcol <- max(which(grep("pop",colnames(a))))
  tabSSI <- tab[,1:nbcol]
  return(tabSSI)
}

                                        # lectureFichierSortieFreePascal.nichet():DATA.FRAME
                                        # ------------------------------------------------------
                                        # evolution des niches au court du temps
                                        # pour le calcul de l'overlap
                                        # -> {DATA.FARME}

lectureFichierSortieFreePascal.nichet <- function(repertoire,serie,rep,idSimul)
{
  nomFichier <- paste(repertoire,"CommunauteResultats/",
                      serie,"0",idSimul,"/",
                      serie,"0",idSimul,"Meca-Niche",rep,".csv",sep="")

  tabniche <- read.csv(nomFichier,stringsAsFactors=FALSE)
  nbcol <- max(which(grep("res",colnames(tabniche))))
  tabniche <- tabniche[,1:nbcol]

  return(tabniche)
}



lectureFichierSortieFreePascal.parametreGlobal <- function(repertoire,serie,idSimul)
{
  nomFichier <- paste(repertoire,"CommunauteResultats/",serie,"0",idSimul,"/",serie,"0",
                      idSimul,"parametre.csv",sep="")
  tab <- read.csv(nomFichier)
  return(tab)
}



                                        # resumeSortieParStep():DATA.FRAME
                                        # --------------------------------
                                        # resume les donn�es sortie du mod�le Free Pascal
                                        # et edit un fichier resumer
                                        # + repGlobal {PATH} chemin de la racine du mod�le
                                        # + serieBatch {INT} identifiant batch
                                        # + nbRep {INIT} nombre de r�p�tition monte carlos
                                        # + parametreNom {[STRING]} ([]) vecteur des nom des parametres
                                        #          dont on modifie la valeur
                                        # + parametreValeur  {[FLOAT]} vecteur des valeur des parametres
                                        # + reel {BOOL} (TRUE) utilisation des valeur reel des parametre
                                        #               les valeurs reel sont les valeurs calculers en cours de simulations
                                        #               part le mod�le Free Pascal
                                        # + idSimul {INT}(0) Identificateur de la simulation 

                                        # 1 - ouverture du fichier de sortie
                                        # 2 - fabrication du resumer
                                        # 3 - fabrication des indcateurs


resumeSortieParStepLight <- function(repGlobal,serie,
                                     nbRep,parametreNom,parametreValeur,
                                     reel = TRUE,idSimul=0,parametreNomSortie=c()){
                                        # repSortie {PATH} repertoire des sorties de la fonction
  repSortie = paste(repGlobal,"CommunauteResultatsResum/",sep="")
                                        # fichierDataSortie {PATH.FILE} fichier de sortie
  fichierDataSortie <- paste("Summarydata",serie,".csv",sep="")

  rmin <-  1
  for(r in rmin:nbRep){
                                        # barreDeffilement() {myRfunction}
                                        # affichage d'une barre de d�filement
    barreDefilement(r,nbRep)
                                        # tab {DATA.FRAME} tab de ma dynamique des population
                                        # lectureFichierSortieFreePascal.pop() {local}
    tab <- lectureFichierSortieFreePascal.pop(repertoire=repGlobal,
                                              serie=serie,rep=r,idSimul)
    
                                        # tabPart {DATA.FRAME} tab des parts
                                        #   de chaques pop sur chaque ressources
                                        # lectureFichierSortieFreePascal.parametre() {local}
    tabPart <- lectureFichierSortieFreePascal.parametre(repertoire=repGlobal,
                                                        serie=serie,rep=r,idSimul)
                                        # vecSSI [INT] vecteur du nombre de switch
                                        # possible pas espece : correspond au SSI theorique
    vecSSI <- as.numeric(tabPart[3,])
    tabPart <- tabPart[which(grep("res",tabPart[,1])),]
                                        # tabSSIt {DATA.FRAME} tab des SSI au court du temps
    tabSSIt <- lectureFichierSortieFreePascal.SSIt(repertoire=repGlobal,
                                              serie=serie,rep=r,idSimul)
                                        # tabNichet {DATA.FRAME} tab des SSI au court du temps
    tabnichet <- lectureFichierSortieFreePascal.nichet(repertoire=repGlobal,
                                              serie=serie,rep=r,idSimul)

                                        # t {INT} (100) represente le tampon en fin de simul
                                        #   sur lequel on fait l'analyse
    t <- 100
                                        # tabIndicateur {DATA.FRAME} fichier d'importation
                                        #   dans fonction indicateurByStep()
    tabIndicateur <- tab[(nrow(tab)-t):nrow(tab),]
    
                                        # vecIndicateur {[FLOAT]} indicateurByStepLight() {local}
                                        #   calcul l'ensemble des indicateurs
                                        #      biomasse, nombre,
                                        #      simpsonBiodiv, CRI
    vecIndicateur <- indicateurByStepLight(tabIndicateur,tabPart,tabnichet,vecSSI,tabSSIt,t)
##browser()
    if( reel){
                                        # tabParam {DATA.FRAME} data.frame des parametres de la simulation
                                        # lectureFichierSortieFreePascal.parametre() {local}
      tabParam <-  lectureFichierSortieFreePascal.parametre(repertoire=repGlobal,
                                                            serie=serie,rep=r,idSimul,FALSE)

                                        # indiceParam {INT} num�ros de ligne correspondant aux valeur reel
      indiceParam <-  which(substring(rownames(tabParam),nchar(rownames(tabParam))-3,nchar(rownames(tabParam))) == "REEL")
                                        # vecParametreREEL {[FLOAT]} parametre reel calculer en cours de simulation
      vecParametreREEL  <-as.numeric(as.character(tabParam[indiceParam,1]))
      
                                        # update vecIndicateur
      vecIndicateur <- c(vecIndicateur,vecParametreREEL)
    }
    
                                        # uneSortieParam {DATA.FRAME} data.frame des indicateurs et parametres
                                        # initialisation et concatenation de uneSortieParam
    if(r == rmin) uneSortieParam <- vecIndicateur
    else uneSortieParam <- rbind(uneSortieParam,vecIndicateur)
  }

 # browser()
  tabParametreGlobalDelphi <- lectureFichierSortieFreePascal.parametreGlobal(repertoire=repGlobal,
                                                                             serie=serie,idSimul)

  for(i in 1:length(parametreValeur))
    uneSortieParam <- cbind(uneSortieParam,parametreValeur[i])
#  if(length(parametreValeur) == 1) uneSortieParam <- cbind(uneSortieParam,parametreValeur[1])
#  if(length(parametreValeur) == 2) uneSortieParam <- cbind(uneSortieParam,
#             parametreValeur[1],parametreValeur[2])

  if (reel){
  ##  browser()
    colonnesNom <-  c(paste(
              rep(c("Nb","Biomasse",
                    "SimpsonBiodiv","CRI","CSI","CSIreel","COI"),each=2),
              100,
              rep(c("moy","sd"),4),sep=""),
        parametreNomSortie,parametreNom)
        colnames(uneSortieParam) <- colonnesNom
  }
  else {
    colnames(uneSortieParam) <-
      c(paste(
              rep(c("Nb","Biomasse",
                    "SimpsonBiodiv","CRI","CSI","CSIreel","COI"),each=2),
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




# --------------------------------------
# calcul.COIt
# ==========

calcul.COIt <- function(dynTot,n,range){
  vecCOI <- vector()
  for (t in (nrow(dynTot)-range):nrow(dynTot)) {
    na <- subset(n,an==t)[,3:23]
    ab <- dynTot[as.character(t),]
    vecCOI <- c(vecCOI,coi(na,ab))
  }
  return(vecCOI)
}




calcul.CSIt <- function(dynTot,tabSSIt,range){
  vecCSI <- vector()
  for(t in  (nrow(dynTot)-range):nrow(dynTot)) {
    ssi.t <- tabSSIt[as.character(t),]
    abond.t <- dynTot[as.character(t),]
    vecCSI <- c(vecCSI,calculCSI(abond.t,ssi.t))
  }
  return(vecCSI)
}



  
                                        # indicateurByStepLight():MATRIX(FLOAT) 
                                        #-----------------------------------
                                        # calcul des indcateurs
                                        # pour cette version nous calculons :
                                        #       Nb,Biomasse,
                                        #       SimpsonBiodiv

                                        # + tabAbond: {DATA.FRAME} des biomasse des pop
                                        # + tabPart: {DATA.FRAME} des part en ressources des pop
                                        # + range: {INT} correspond au nombre d'ann�es
                                        #  sur lesquelles sont fait les calculs
                                        # -> {MATRIX[FLOAT]}

indicateurByStepLight <- function(tabAbond,tabPart,tabPart_t,vecSSI,tabCSI_t,range){

  tab <-  tabAbond
                                        # vecNb {[INT]} nombre d'esp�ce par ann�es
                                        # calculNbSp(){myRfunction}
  vecNb <- apply(tab,1,calculNbSp)
                                        # vecBiomasse {[FLOAT]} biomasse totale par ann�es
                                        # calculBiomasse() {myRfunction}
  vecBiomasse <- apply(tab,1,calculBiomasse)
  
                                        # vecSimpson.div {[FLOAT]} indice de diversit� de Simpson par ann�e
                                        # calculSimpson() {myRfunction}
  vecSimpson.div <- apply(tab,1,calculSimpson,methode="diversite")
  
                                        # vecSRI {[FLOAT]} SRI par esp�ces
  vecSRI <-  apply(tabPart,2,calculSRI)
                                        # vecCRI {[FLOAT]} nombre de besoin moyen par individus
                                        # caluclCRI() {myRfuncuion}
##  browser()
  vecCRI <- apply(tab,1,calculCRI,vecSRI)

  vecCSI <- apply(tab,1,calculCSI,vecSSI)

  vecCSIreel <- calcul.CSIt(tab,tabSSI_t,range)
  
  vecCOI <- calcul.COIt(tab,tabPart_t,range)

                                        # tabnew {DATA.FRAME} de l'ensemble des vecteurs
  tabnew <-  cbind(vecNb,vecBiomasse,vecSimpson.div,vecCRI,vecCSI,vecCSIreel,vecCOI)
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
           vecMean[7],vecSd[7])  
                                        # vec -> {MATRIX[FLOAT]} matrice de 1 ligne
  vec <- t(as.matrix(vec))
                                        # fabrication des nom de colonnes
  colnames(vec) = paste(
            rep(c("Nb","Biomasse",
                  "SimpsonBiodiv","CRI","CSI","CSIreel","COI"),each=2),
            range,
            rep(c("moy","sd"),3),sep="")
  return(vec)
}



                                        # resum.plot3D():void
                                        # ---------------------
                                        # resume sous forme de graphe tous les indcateurs
                                        # + repGlobal
                                        # + serie
                                        # + parametre1
                                        # + parametre2

resum.plot3D <- function(repGlobal,serie,parametre){
 ## browser()
  repSortie = paste(repGlobal,"CommunauteResultatsResum/",sep="")
  fichierDataSortie <- paste("Summarydata",serie,".csv",sep="")
  fichierData <- paste(repSortie,fichierDataSortie,sep="")

  d <-  read.csv(fichierData)



  vec.x1 <- d[,which(colnames(d)== parametre[1])]
  vec.z1 <- d[,which(colnames(d)== parametre[2])]
  titreX1 = parametre[1]
  titreZ1 = parametre[2]

  vec.z2 <- d[,which(colnames(d)== parametre[1])]
  vec.x2 <- d[,which(colnames(d)== parametre[2])]
  titreZ2 = parametre[1]
  titreX2 = parametre[2]

  titre =  paste("serie:",serie,sep="")  
  
  for(i in 1:(ncol(d)-4)){

    
    
    titreY =  colnames(d)[i]
    vec.y <- d[,i]
    
    
    plot3D.degrader.smooth(repSortie,titre,titreX1,titreY,titreZ1,vec.x1,vec.y,vec.z1)    
    plot3D.degrader.smooth(repSortie,titre,titreX2,titreY,titreZ2,vec.x2,vec.y,vec.z2)    
    
  }
}


plot3D.degrader.smooth <- function(repSortie="",titre="",titreX="",titreY="",titreZ="",
                                   vec.x,vec.y,vec.z,backg="black",font.color="white",tendance.color="blue"){
  op <- par(bg=backg,col=font.color)
#browser()
  library(graphics)
  palette(heat.colors(100))
  
  couleurValeur <-trunc((vec.z - min(vec.z))/(max(vec.z)-min(vec.z))*100)
  couleurValeur <- (couleurValeur * -1)+max(couleurValeur)

  plot(vec.x,vec.y,
       type='p',pch=20,cex=0.1,col=couleurValeur,
       main = titre,sub=titreZ,xlab = titreX,ylab=titreY,
       col.axis=font.color,col.lab=font.color,
       cex.main = 2,   font.main= 2, col.main= font.color,
       cex.sub = 1, font.sub = 3, col.sub = "yellow")

  tendance=smooth.spline(vec.x,vec.y,df=6)
  lines(tendance,col=tendance.color,lwd=2,lty=1)
  
  fichier.plot = paste(repSortie,titre,"_",titreY,"-",titreX,"+",titreZ,sep="")
  savePlot(fichier.plot,"png")
  par(op)
  cat("       ==> ",fichier.plot,"\n")

  
}

                                        # test(): void
                                        # -------------
                                        # test les indicateurs sur une trajectoire
                                        # plot la dynamique selon une trajectoire
                                        # + repertoire : {PATH} repertoire g�n�rale du batch
                                        # + serie: {INT} identifiant du Batch
                                        # + idSimul: {INT} identifiant de la simulation
                                        # + rep: {INT} rep�titon de la simulation 

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

plot.multipleTendence <- function(repSortie="",serie,titre="",titreX="",titreY="",
                                  vec.x,vec.y,vec.sc,
                                  backg="white",font.color="black",tendance.color="gray",
                                  vecColorLine = c("red","blue","chocolate","turquoise","green"),
                                  vecColorPoint =  c("pink","lightblue","sandybrown","paleturquoise","lightgreen")) {
##browser()
  library(graphics)
  op <- par(bg=backg,col=font.color)
  
  vecColor <- vecColorPoint[as.numeric(vec.sc)] 
  plot(vec.x,vec.y,
       type='p',pch=20,cex=0.05,col=vecColor,
       main = titre,xlab = titreX,ylab=titreY,col.axis=font.color,col.lab=font.color,col.main=font.color,cex.main=1)

  tendance <- smooth.spline(vec.x,vec.y,df=6)
  ## tendance <- lm(vec.y~vec.x)

  lines(tendance,col=tendance.color,lwd=1,lty=2)

  numScenar <-as.factor(levels(vec.sc))
  
  for(s in 1:length(numScenar)){
    sc <- numScenar[s]
    vec.x.sub <- vec.x[which(vec.sc==sc)]
    vec.y.sub <- vec.y[which(vec.sc==sc)]
    
    tendance <- smooth.spline(vec.x.sub,vec.y.sub,df=6)
    ## tendance <- lm(vec.y.sub~vec.x.sub)
    ##    browser()
    lines(tendance,col= vecColorLine[as.numeric(sc)],lwd=2,lty=1)# vecColorLine[as.numeric(s)]
    
    
  }
  legend("topright", levels(vec.sc), col=vecColorLine[as.numeric(numScenar)],cex = 0.8,lty = 1, lwd = 2,box.col="black")
  
  fichier.plot = paste(repSortie,"s",serie,"_",titreY,"-",titreX,sep="")
  savePlot(fichier.plot,"png")
  par(op)
  cat("       ==> ",fichier.plot,"\n")
}

                                        # compareScenar():void
                                        # -----------------------
                                        # comparaison par plot de plusieur scenar
                                        # + numScenar  {[INT]} id scenar
                                        # + parametre1 {STRING} parametre graphe
                                        # + parametre2 {STRING} parametre graphe


compareScenar <- function(numScenar,parametre1,parametre2){

  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
##  browser()
  series <- paste(numScenar,collapse="B")

  repSortie <- paste(repGlobal,"Comp",series,"/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie)

  for(s in numScenar){
    repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
    fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
    fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
    d1 <-  read.csv(fichierData)
    d1 <-  data.frame(d1,sc=as.factor(s))
    if (numScenar[1]== s) d <-  d1
    else d <- rbind(d,d1)
  }
  ## d <- d[which(d[,which(colnames(d)==parametre1)]> 0.45 & d[,which(colnames(d)==parametre1)]< 0.55),]
 ## d <- d[which(d[,which(colnames(d)==parametre2)]> 3800 & d[,which(colnames(d)==parametre2)]< 4200),]

  unVec.x1 <- d[,which(colnames(d)== parametre1)]
  unTitreX1 <- parametre1

  unVec.x2 <- d[,which(colnames(d)== parametre2)]
  unTitreX2 <- parametre2

  unVec.sc <- d$sc
  
  serie <-  paste(numScenar,collapsesep="-")
  unTitre <- paste("serie:", paste(numScenar,collapse="-"),sep=" ")

  
  for(i in 1:(ncol(d))){
    
    unTitreY <- colnames(d)[i]
    unVec.y <- d[,i]
    

    plot.multipleTendence(repSortie,serie=series,titre=unTitre,titreX=unTitreX1,titreY=unTitreY,
                          vec.x=unVec.x1,vec.y=unVec.y,vec.sc=unVec.sc) 

    plot.multipleTendence(repSortie,serie=series,titre=unTitre,titreX=unTitreX2,titreY=unTitreY,
                          vec.x=unVec.x2,vec.y=unVec.y,vec.sc=unVec.sc) 
  } 

}

                                        # testTraject():void
                                        # ------------------
                                        # plot la dynamique des pop pour une trajectoire
                                        # + rep : {PATH} repertoire g�n�rale du batch
                                        # + s: {INT} identifiant du Batch
                                        # + p: {INT} identifiant parametre
                                        # + id: {INT} identifiant de la simulation
                                        # + repet: {INT}(1) rep�titon de la simulation
                                        #       repet = 1 r�p�tition de trajectoire compl�tes

testTraject <-  function(rep,s,p,repet=1,id){
  tab <-  lectureFichierSortieDelphi.pop(repertoire = rep,serie = s,param = p,rep = repet,idSimul=id)
  x11()
  matplot(tab,type='l',pch=1,lty=1)
}

                                        # testTrajectSum():void
                                        # ------------------
                                        # plot la dynamique total (biomasse)  pour une trajectoire
                                        # + rep : {PATH} repertoire g�n�rale du batch
                                        # + s: {INT} identifiant du Batch
                                        # + p: {INT} identifiant parametre
                                        # + id: {INT} identifiant de la simulation
                                        # + repet: {INT}(1) rep�titon de la simulation
                                        #       repet = 1 r�p�tition de trajectoire compl�tes

testTrajectSum <- function(rep,s,p,repet=1,id){
  tab <-  lectureFichierSortieDelphi.pop(repertoire = rep,serie = s,param = p,rep = repet,idSimul=id)
  vec = apply(tab1,1,sum)
  x11()
  plot(vec,type='l',pch=1,lty=1)
  
}

simulationUnJeuParam <- function(id=1,nbJeuxParam,unJeuParametre,
                                 codeParam,indiceSimule,versionModel,
                                 nBatch,repertoireInput,repBatch,paramModifSerieNom,
                                 nombreRep,valeurCor,paramNomSortie,initiationInput,
                                 simulationFreePascal,resumer,varParam,nomBatch,parametreNomSortie){
  parametre =  as.vector(as.matrix(unJeuParametre))
                                        # numerosJeuxParam : {INT}
                                        #   calcul de son num�ros parmis les nbJeuxParam
  cat(" (",id,"/",nbJeuxParam,") ->  ")

  for(i in (1:length(parametre))) {
    cat(codeParam[i],": ",parametre[i],sep="")
    if (i == length(parametre)) cat("\n")
    else cat(" | ")
  }

                                        # initParam(){local}
                                        #     fabrique le fichier d'input du mod�le FPC
  if(initiationInput)
    initParam(typeInit = "moyenne",versionM = versionModel,
              serieBatch = nBatch,repertoireParametre = repertoireInput, repertoire=repBatch,
              paramModifNom = c(paramModifSerieNom,
                "{init_nbSimul}","{idSimulNUM}","{idVersionNUM}"),
              paramModifValeur = c(parametre,nombreRep,
                paste(nBatch,id,sep="0"),versionModel),idSimul = id,
              coVar=valeurCor,
              varK= as.numeric(varParam=="k"),
              varR= as.numeric(varParam=="r"))
  if(simulationFreePascal){          
    cat("     Sim     ")
                                        # model.FreePascal() {local}
                                        #   Lance le mod�le FreePascal
    model.FreePascal(repertoire=repBatch,versionModel)
  }
  
  if(resumer){
    cat("\n     Summary ")

                                        # resumeSortieParStepLight() {local}
                                        #  r�alise le r�sumer de la simulation
                                        #  calcul l'ensemble des indicateurs
                                        #      biomasse, nombre,
                                        #      simpsonBiodiv, simpsonBiodivFonctionnelpond
    resumeSortieParStepLight(repGlobal = repBatch ,
                             serie = nomBatch, nbRep = nombreRep,
                             parametreNom = paramModifSerieNom,
                             parametreValeur = parametre,idSimul = id,reel = TRUE,
                             parametreNomSortie=parametreNomSortie)
  }
  
  
  


}
                                        # mainBatch():void
                                        # -----------------
                                        # void fonction MAIN qui r�alise les batchs
                                        # + versionModel : {INT} version du mod�le
                                        # + nBatch : {INT} num�ros du batch
                                        # + nombreRep : {INT} (300)
                                        #        nombre de r�p�tition du mont� carlos
                                        # + repertoireInpout : {STRING} (fichier g�n�rique)
                                        #        fichier d'importation des param�tres
                                        # + paramModifSerieNom : {[STRING]}
                                        #        vecteur des nom des parametres test�s
                                        # + codeParam : {[STRING]}
                                        #        vecteur des codes pour affichage �cran
                                        # + paramValeur : {[[FLOAT]]}
                                        #        list des vecteurs des valeurs des parametres test�
                                        # + parametreNomSortie : {[STRING]}
                                        #        vecteur des nom des parametre en sortie
                                        # + parametreGraph {[STING]}
                                        #        vecteur des paramaetres affich� des les graphes
                                        # + valeurCor = {[FLOAT]}(1)
                                        #        facteur de cor�lation pour la variation temporelle


mainBatch <- function(versionModel,nBatch,nombreRep=300,
                      repertoireInput = "/home/romain/1_Recherche/Model/community/communauteInput/",
                      paramModifSerieNom,
                      codeParam,
                      paramValeur,
                      parametreNomSortie,
                      parametreGraph,
                      graphReel = TRUE,
                      valeurCor = 1,
                      supressionFichierResumer = TRUE,
                      initiationInput = TRUE,
                      simulationFreePascal = TRUE,
                      resumer = TRUE,
                      graphe = TRUE,
                      parametreMatch=FALSE) {
##browser()
                                        # repBatch : {STRING} recup�ration du repertoire de travail
  repBatch <- paste(getwd(),"/",sep="")
                                        # nomBatch : {STRING} construction du nom (ref) du batch
  nomBatch <- paste(versionModel,"0",nBatch,sep="")
                                        # varParam : {CHAR} construction du indentifiant parametre
  varParam <- strsplit(codeParam[1]," ")[[1]][1]
                                        # combinaisonParam {DATAFRAME} ensemble des combinaison � tester
                                        #   makeTableCombinaison {myRfunction}
  combinaisonParam <- makeTableCombinaison(paramValeur,parametreContrainte = parametreMatch)
  colnames(combinaisonParam) <- paramModifSerieNom
                                        # nbJeuxParam : {INT} calcul du nombre de juex de parametre
  nbJeuxParam <-  nrow(combinaisonParam)
                                        # id : {INT} identifiant incr�ment� du jeux de parametres
  id <-  1
  
  cat("===================================================\n")
  cat("|     BATCH MODELE DELPHI COMPET. MODEL ",nomBatch,"     |\n") 
  cat("===================================================\n")
                                        # dateDebut : {TIME} renseigne sur l'heure de d�but de simul
                                        # Sys.time() {base} renvoie heure machine
  dateDebut <- Sys.time()
                                        # affichage description batch
  cat("\n * Debut  Batch:",format(dateDebut,format="%d/%m/%y %H:%M"),"*\n")
  cat("\n nombre de jeux de parametres : ",nbJeuxParam,
      "\n nombre de repetition : ",nombreRep)
  for(p in 1:length(codeParam)) cat("\n    ",codeParam[p],":(",min(paramValeur[[p]]),"->",max(paramValeur[[p]]),")")
  cat("\n facteur de corelation: ",valeurCor) 
  cat("\n -----------------------------------------------\n")
                                        # mise � jour de dateDebut
  dateDebut <- Sys.time()

                                        # suppresion du fichier de resumer
  if (supressionFichierResumer){
    fichier <- paste(repBatch,"CommunauteResultatsResum/Summarydata",nomBatch,".csv",sep="")
    commande <- paste("rm",fichier)
    system(commande,ignore.stderr = FALSE)
  }
                                        # boucle de scan des deux param�tre
  for(p in 1:nrow(combinaisonParam)){
   simulationUnJeuParam(p,nbJeuxParam,combinaisonParam[p,],
                        codeParam,indiceSimule,versionModel,
                        nBatch,repertoireInput,repBatch,
                        paramModifSerieNom,nombreRep,valeurCor,
                        paramNomSortie,initiationInput,simulationFreePascal,
                        resumer,varParam,nomBatch,parametreGraph)
   cat("\n")
   estimDateFin(dateDebut,nbJeuxParam,p)
     
      
  }
  if(graphe){
    cat("\n   ->  Plot    ")
    
    resum.plot3D(repGlobal = repBatch ,serie = nomBatch,
                 parametre = parametreGraph)

  }
  dateFin <- Sys.time()
  cat("\n * Fin  Batch:",format(dateFin,format="%d/%m/%y %H:%M"),"*\n",
      "--> Running time: ",format(difftime(dateFin,dateDebut,units="auto")),"\n")

  cat("\n===================================\n")
  cat("|    FIN DU BATCH. MODEL ",nomBatch,"     |\n") 
  cat("=====================================\n")
  
  
}


graphePapier1 <-  function(){
  vecSc.rmean <- c(1013,1014,2013,2014)
  vecSc.rsd <- c(1015,1016,2015,2016) 
  vecSc.kmean <- c(1017,1018,2017,2018)
  vecSc.ksd <- c(1019,1020,2019,2020)
  ##vecSc.res <- c(1023,1024,2023,2024)
  vecSc.res <- c(1025,1026,2025,2026)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res,vecSc.rsd,vecSc.ksd)
  listTab <-vector("list",5)
  vecNames <- c("Replacement rate mean\n(r_mean)","Caring capacity mean\n(K_mean)","Number of resource types\n(T)","Replacement rate sd\n(r_sd)","Carring capacity sd\n(K_sd)")
  vecIndicateur <-  c("Number of individuals","Number of species","CRI","Simpson")
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
  ## browser()

  #repSortie <- paste(repGlobal,"Graph1/",sep="")
  repSortie <- paste("/home/romain/1_Recherche/Article1/Graph1/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie)
  colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,12,12,13,3,1,7,5,10,11,12,3,1,7,5,10,11,12),5,7,byrow=TRUE)
  ## colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,10,11,12,3,1,7,5,10,11,12),5,7,byrow=TRUE)

  for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
   ## browser()
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }
  colnames(listTab[[1]])[6] <- "rmean"
  colnames(listTab[[2]])[6] <- "kmean"
  colnames(listTab[[3]])[6] <- "ress"
  colnames(listTab[[4]])[6] <- "rsd"
  colnames(listTab[[5]])[6] <- "ksd"
##browser()
  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:4] <- vecIndicateur
    colnames(listTab[[p]])[5] <- vecNames[p]
    
  }

  vecColorLine <- c("red","red","blue","blue")
  vecColorPoint <- c("pink","pink","lightblue","lightblue")
  vecTypePoint <- c(1,3,1,3)
  vecTypeLine <- c(1,2,1,2)
  vecEpaisseurLine <- c(2.5,4,2.5,4)
  library(graphics)

  ## browser()
  



  
  fileNamePlot = paste(repSortie,"mean.png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")

  png(file=fileNamePlot, width = 900, height = 1600,
      units = "px",pointsize = 25)
 # x11()
  par(new=TRUE,mar=c(0,0,0,0))
  
  m = matrix(c(0:3,0,4:6,0,7:9,0,10:12,rep(0,4)),5,4,byrow=TRUE)
  layout(m,widths=c(0.75,rep(3,3)),heights=c(rep(3,4),0.75))
  layout.show(12)
  
  for (i in 1:4){

  
    ymax = 0
    for(p in 1:3)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 1
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
     
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
    axis(2)
    mtext(vecIndicateur[i],
          side = 2, las = 3, line = 2,cex = 0.8)
    
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8) 
    }

 #   if (i == 2)    mtext("Indicator",
  #        side = 2, las = 3, line = 3,adj = -0.2,cex = 1.2)
    
    box()

    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    
    p <- 2
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),axes=FALSE)
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8)
 #     mtext("Constraints on the community", 
  #          side = 1, las = 1, line = 5, adj = 0.5,cex=1.2)
      
    }
    
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub),vec.groupe.sub,mean)
      meanTab <- matrix(0,length(meanList),2)
      ## tendance <- lm(vec.y.sub~vec.x.sub)
      
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
      ##    browser()
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)])     
      
    }
    
    p <- 3
    
    vec.x <- 9-(as.vector(as.matrix(listTab[[p]][,6])))
   # vec.x <- (as.vector(as.matrix(listTab[[p]][,6])))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    ##   browser()
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),xlim=c(0,9),axes=FALSE)
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8) 
    }
    box()


    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
#browser()
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in which(summary(vec.groupe.sub)>0)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      

     
      if (i>2)
        meanTab <- meanTab[which(meanTab[,2]>0),]

      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    

  }
  dev.off()

  fileNamePlot = paste(repSortie,"sd.png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")

  png(file=fileNamePlot, width = 600, height = 1600,
      units = "px",pointsize = 25)
  par(new=TRUE,mar=c(0,0,0,0))
  m  <- matrix(c(0,1,2,0,3,4,0,5,6,0,7,8,0,0,0),5,3,byrow=TRUE)
  layout(m,widths=c(0.75,rep(3,3)),heights=c(rep(4,4),1))
  layout.show(8)

  for(i in 1:4){
    ymax <- 0
    for(p in 4:5)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 4
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
    axis(2)
    mtext(vecIndicateur[i],
          side = 2, las = 3, line = 2, adj = 0.5,cex=0.8)
    
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8) 
    }
 #   if (i == 2)    mtext("Indicator",
 #         side = 2, las = 3, line = 3,adj = -0.2,cex = 1.2)
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){

      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    
    p <- 5
    vec.x <- as.vector(as.matrix(listTab[[p]][,5]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,6])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=
         "",
         ylim=c(0,ymax),axes=FALSE)
    
    if(i == 4){
      axis(1)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8)
  #    mtext("Constraints on the community", 
  #          side = 1, las = 1, line = 5,adj=2,cex=1.2)
    }
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[as.numeric(sc)],lty=vecTypeLine[as.numeric(sc)],lwd=vecEpaisseurLine[as.numeric(sc)])
    }
  }
  dev.off()
}



calculR_squarred <- function(s){
   repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
   
   repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
   fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
   fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
   d <-  read.csv(fichierData)
 ##  mod <- lm(d[,1]~log(d[,3]))
 ##  sumMod <- summary(mod)
 ##  print(sumMod)
 ##  print(sumMod$r.squared)
 ##  plot(log(d[,3]),d[,1])
 ##  abline(mod)
   print(cor.test(d[,1],d[,3],method="kendall"))
}


graphePPT.1 <-  function(){
  vecSc.rmean <- c(2013,1013)
  vecSc.kmean <- c(2017,1017)
  vecSc.res <- c(2023,1023)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res)
  listTab <-vector("list",3)
  vecNames <- c("Taux de croissance","Quantite de chaque\ntype de ressource","Nombre de types\ndifferents de ressources")
  vecIndicateur <-  c("Nombre d individus","Nombre d espece","CRI")
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
  ##  browser()

  repSortie <- paste(repGlobal,"ppt1/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie)
  colonneTab <- matrix(rep(c(3,1,7,9,11,12),3),3,6,byrow=TRUE)

for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }
  colnames(listTab[[1]])[5] <- "rmean"
  colnames(listTab[[2]])[5] <- "kmean"
  colnames(listTab[[3]])[5] <- "ress"

  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:3] <- vecIndicateur
    colnames(listTab[[p]])[4] <- vecNames[p]
    
  }

  vecColorLine = c("red","blue")
  vecColorPoint =  c("lightblue","pink")
  vecTypePoint = c(20,20)
  vecTypeLine = c(1,2)
  library(graphics)

  for (i in 1:3){

  fileNamePlot = paste(repSortie,"mean_",i,".png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")
  png(file=fileNamePlot, width = 900, height = 400,
      units = "px",pointsize = 25)
  
  par(new=TRUE,mar=c(1,0,1,0))

  m = matrix(c(0:3,rep(0,4)),2,4,byrow=TRUE)
  layout(m,widths=c(1,rep(4,3)),heights=c(3,1))
  layout.show(3)
  
    ymax = 0
    for(p in 1:3)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 1
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
     
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
    axis(2,cex=0.5)
    mtext(vecIndicateur[i],
          side = 2, las = 3, line = 2,cex = 0.8,font=2)
    
   
      axis(1,cex=0.5)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 
   

   
    
    box()

    numScenar <-  as.factor(vecSc.rmean)
  ##as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[s],lwd=vecEpaisseurLine[s],lty=vecTypeLine[s])
    }
    
    p <- 2
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),axes=FALSE)
   
      axis(1,cex=0.5)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 4, adj = 0.5,cex=0.8,font=2)
     
    
    box()
 ##   numScenar <-as.factor(levels(vec.sc))
    numScenar <-  as.factor(vecSc.kmean)
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub),vec.groupe.sub,mean)
      meanTab <- matrix(0,length(meanList),2)
      ## tendance <- lm(vec.y.sub~vec.x.sub)
      
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
      ##    browser()
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[s],lwd=vecEpaisseurLine[s],lty=vecTypeLine[s])     
      
    }
    
    p <- 3
    
    vec.x <- 9-(as.vector(as.matrix(listTab[[p]][,5])))+1
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    ##   browser()
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),xlim=c(1,9),axes=FALSE)
   
      axis(1,cex=0.5)
      mtext(vecNames[p], 
            side = 1, las = 1, line = 4,adj = 0.5,cex=0.8,font=2) 
   
    box()


 numScenar <-  as.factor(vecSc.res)
##  numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
#browser()
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in which(summary(vec.groupe.sub)>0)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
      ##    browser()
      lines(meanTab[,1],meanTab[,2],col= vecColorLine[s],lwd=vecEpaisseurLine[s],lty=vecTypeLine[s])
    }
  dev.off()    

  }



}



graphePNG1.SCB.mean <-  function(){
  vecSc.rmean <- c(2013,1013)
  vecSc.rsd <- c(2015,1015)
  vecSc.kmean <- c(2017,1017)
  vecSc.ksd <- c(2019,1019)
  vecSc.res <- c(2025,1025)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res,vecSc.rsd,vecSc.ksd)
  listTab <-vector("list",5)
  vecNames <- c("Growth factor mean","Mean quantity of ressources","Number of resources","Growth factor sd","Carring capacity sd")
  vecIndicateur <-  c("Biomass","number of species","CRI")

                                        # vecSc.rmean <- c(2013,1013)
                                        # vecSc.kmean <- c(2017,1017)
                                        # vecSc.res <- c(2023,1023)
                                        # listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res)
                                        # listTab <-vector("list",3)
                                        # vecNames <- c("Growth factor","Mean quantity of resources","Number of ressources")
                                        # vecIndicateur <-  c("Biomass","Nummber of species","CRI")
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
                                        #  browser()

  repSortie <- paste(repGlobal,"SCB/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie,showWarnings = FALSE)
  colonneTab <- matrix(c(3,1,7,9,11,12,3,1,7,9,11,12,3,1,7,12,12,13,3,1,7,10,11,12,3,1,7,10,11,12),
                       5,6,byrow=TRUE)
  ## colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,12,12,13,3,1,7,5,10,11,12,3,1,7,5,10,11,12),
  ##                       5,7,byrow=TRUE)

  for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }

  colnames(listTab[[1]])[5] <- "rmean"
  colnames(listTab[[2]])[5] <- "kmean"
  colnames(listTab[[3]])[5] <- "ress"
  colnames(listTab[[4]])[5] <- "rsd"
  colnames(listTab[[5]])[5] <- "ksd"
  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:3] <- vecIndicateur
    colnames(listTab[[p]])[4] <- vecNames[p]
    
  }

  
 
  vecColorLine = c("#a02c2cc8","#0b3352c8") # c("red","blue")
   vecColorLine2 = c("#a02c2c7a","#0b33527a")# c("lightblue","pink")

#  vecColorLine2 = c("#e9b0af","#61c4dc")# c("lightblue","pink")
  vecColorPoint = c("#61c4dc7a","#e9b0af7a")# c("lightblue","pink")
#  vecColorPoint = c("#0b335258","#a02c2c58") # c("red","blue")

  vecTypePoint = c(20,20)
  vecTypeLine = c(1,1)
  vecEpaisseurLine <- c(8,8)

  library(graphics)

  for (i in 1:2){
    fileNamePlot = paste(repSortie,"mean_",i,".png",sep="") # avec l'IC95
    cat("\n \n  <-- ",fileNamePlot,"\n")

    png(file=fileNamePlot, width = 1200, height = 800,pointsize = 25)
    savePar <- par(mar=c(0,0,0,0))
    
    
  #  m = matrix(c(0:1,rep(0,4)),2,4,byrow=TRUE)
  #  layout(m,widths=c(1,rep(4,3)),heights=c(3,0.5))
    ##  layout.show(3)
   m= matrix(1:2,1,2)
    layout(m)
    ymax = 0
    for(p in 1:3)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 1
    ##  browser()
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreY <- colnames(listTab[[p]])[5]
    titreX <- colnames(listTab[[p]])[i]
 
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
 #   axis(2,cex=0.8,font=2)
 #   mtext(vecIndicateur[i],
 #         side = 2, las = 3, line = 2,cex = 0.8,font=2)
  #  axis(1,cex=0.8,font=2)
 #   mtext(vecNames[p], 
 #         side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 
    box()

    numScenar <-  as.factor(vecSc.rmean)
    ##as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
  
#browser()
      minX <-  by(vec.x.sub, vec.groupe.sub, quantile,.025)
      minY <-  by(vec.y.sub, vec.groupe.sub, quantile,.025)
      minTab <- matrix(0,length(minX),2)
      for( j in 1:length(minX)){
        minTab[j,1]=minX[[j]][1]
        minTab[j,2]=minY[[j]][1]
      }
      lines(minTab[,1],minTab[,2],
            col= vecColorLine2[s],
            lwd=6,
            lty=2)

      maxX <-  by(vec.x.sub, vec.groupe.sub, quantile,.975)
      maxY <-  by(vec.y.sub, vec.groupe.sub, quantile, .975)
      maxTab <- matrix(0,length(maxX),2)
      for( j in 1:length(maxX)){
        maxTab[j,1]=maxX[[j]][1]
        maxTab[j,2]=maxY[[j]][1]
      }
      lines(maxTab[,1],maxTab[,2],
            col= vecColorLine2[s],
            lwd=6,
            lty=2)

      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[s],
            lwd=vecEpaisseurLine[s],
            lty=vecTypeLine[s])
      
    }
    
    p <- 2
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),axes=FALSE)
    
  #  axis(1,cex=0.8,font=2)
 #   mtext(vecNames[p], 
 #         side = 1, las = 1, line = 4, adj = 0.5,cex=0.8,font=2)
    
    box()
    ##   numScenar <-as.factor(levels(vec.sc))
    numScenar <-  as.factor(vecSc.kmean)
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub),vec.groupe.sub,mean)
      meanTab <- matrix(0,length(meanList),2)
      ## tendance <- lm(vec.y.sub~vec.x.sub)
      
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
   
      minX <-  by(vec.x.sub, vec.groupe.sub, quantile,.025)
      minY <-  by(vec.y.sub, vec.groupe.sub, quantile,.025)
      minTab <- matrix(0,length(minX),2)
      for( j in 1:length(minX)){
        minTab[j,1]=minX[[j]][1]
        minTab[j,2]=minY[[j]][1]
      }
      lines(minTab[,1],minTab[,2],
            col= vecColorLine2[s],
            lwd=6,
            lty=2)

      maxX <-  by(vec.x.sub, vec.groupe.sub, quantile,.975)
      maxY <-  by(vec.y.sub, vec.groupe.sub, quantile,.975)
      maxTab <- matrix(0,length(maxX),2)
      for( j in 1:length(maxX)){
        maxTab[j,1]=maxX[[j]][1]
        maxTab[j,2]=maxY[[j]][1]
      }
      lines(maxTab[,1],maxTab[,2],
            col= vecColorLine2[s],
            lwd=6,
            lty=2)
      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[s],
            lwd=vecEpaisseurLine[s],
            lty=vecTypeLine[s])     
 
    }
    
##    p <- 3
##    
##    vec.x <- 9-(as.vector(as.matrix(listTab[[p]][,5])))+1
##    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
##    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
##    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))
##
##    if (i>2){
##      ## browser()
##      vec.y.save <- vec.y
##      vec.x <- vec.x[which(vec.y>0)]
##      vec.sc <- vec.sc[which(vec.y>0)]
##      vec.groupe <- vec.groupe[which(vec.y>0)]
##      vec.y <- vec.y[which(vec.y>0)]
##    }
##    ##   browser()
##    vecColor <- vecColorPoint[as.numeric(vec.sc)]
##    vecType <- vecTypePoint[as.numeric(vec.sc)]
##    titreX <- colnames(listTab[[p]])[5]
##    titreY <- colnames(listTab[[p]])[i]
##
##    
##    plot(vec.x,vec.y,
##         type='p',cex=1.5,
##         col=vecColor,pch=vecType,
##         xlab = titreX,ylab="",
##         ylim=c(0,ymax),xlim=c(1,9),axes=FALSE)
##    
##    axis(1,cex=0.8,font=2)
## #   mtext(vecNames[p], 
## #         side = 1, las = 1, line = 3,adj = 0.5,cex=0.8,font=2)
##
##
##    box()
##
##
##    numScenar <-  as.factor(vecSc.res)
##    ##  numScenar <-as.factor(levels(vec.sc))
##    
##    for(s in 1:length(numScenar)){
##                                        #browser()
##      sc <- numScenar[s]
##      vec.x.sub <- vec.x[which(vec.sc==sc)]
##      vec.y.sub <- vec.y[which(vec.sc==sc)]
##      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
##      
##      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
##      meanTab <- matrix(0,length(meanList),2)
##      for( j in which(summary(vec.groupe.sub)>0)){
##        meanTab[j,1]=meanList[[j]][1]
##        meanTab[j,2]=meanList[[j]][2]
##      }
##      
##      ##    browser()
##      lines(meanTab[,1],meanTab[,2],
##            col= vecColorLine[s],
##            lwd=vecEpaisseurLine[s],
##            lty=vecTypeLine[s])
##    }
    dev.off()    

  }



  
  ## browser()
  i <- 3
  fileNamePlot = paste(repSortie,"mean_",i,".png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")
  png(file=fileNamePlot, width = 800, height = 800,pointsize = 25)
   par(mar=c(0,0,0,0))
  ymax = 0
  
  p <- 1
  vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
  vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
  vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
  vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

  
  ## browser()
  vec.y.save <- vec.y
  vec.x <- vec.x[which(vec.y>0)]
  vec.sc <- vec.sc[which(vec.y>0)]
  vec.groupe <- vec.groupe[which(vec.y>0)]
  vec.y <- vec.y[which(vec.y>0)]
  
  
  vecColor <- vecColorPoint[as.numeric(vec.sc)]
  vecType <- vecTypePoint[as.numeric(vec.sc)]
  titreX <- colnames(listTab[[p]])[5]
  titreY <- colnames(listTab[[p]])[i]
  
  plot(vec.x,vec.y,
       type='p',cex=1.5,
       col=vecColor,pch=vecType,
       xlab = "",ylab="",
       ylim=c(0,max(vec.y)),axes=FALSE)
#  axis(2,cex=0.8,font=2)
#  mtext(vecIndicateur[i],
#        side = 2, las = 3, line = 2,cex = 0.8,font=2)
#  axis(1,cex=0.8,font=2)
#  mtext(vecNames[p], 
#        side = 1, las = 1, line = 2, adj = 0.5,cex=0.8,font=2) 
  box()
  numScenar <-  as.factor(vecSc.rmean)
  ##as.factor(levels(vec.sc))
  
  for(s in 1:length(numScenar)){
    sc <- numScenar[s]
    vec.x.sub <- vec.x[which(vec.sc==sc)]
    vec.y.sub <- vec.y[which(vec.sc==sc)]
    vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
    
    meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
    meanTab <- matrix(0,length(meanList),2)
    for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
    minX <-  by(vec.x.sub, vec.groupe.sub, quantile,.025)
    minY <-  by(vec.y.sub, vec.groupe.sub, quantile,.025)
    minTab <- matrix(0,length(minX),2)
    for( j in 1:length(minX)){
      minTab[j,1]=minX[[j]][1]
      minTab[j,2]=minY[[j]][1]
    }
    lines(minTab[,1],minTab[,2],
          col= vecColorLine2[s],
          lwd=6,
          lty=2)
    
    maxX <-  by(vec.x.sub, vec.groupe.sub, quantile,.975)
    maxY <-  by(vec.y.sub, vec.groupe.sub, quantile,.975)
      maxTab <- matrix(0,length(maxX),2)
      for( j in 1:length(maxX)){
        maxTab[j,1]=maxX[[j]][1]
        maxTab[j,2]=maxY[[j]][1]
      }
      lines(maxTab[,1],maxTab[,2],
            col= vecColorLine2[s],
            lwd=6,lty=2)
      lines(meanTab[,1],meanTab[,2],
          col= vecColorLine[s],
          lwd=vecEpaisseurLine[s],
          lty=vecTypeLine[s])
  
  }
  
  dev.off()
  ##browser() 




}



graphePNG2.SCB.mean <-  function(){
  vecSc.rmean <- c(1013,2013)
  vecSc.rsd <- c(2015,1015)
  vecSc.kmean <- c(1017,2017)
  vecSc.ksd <- c(2019,1019)
  vecSc.res <- c(1025,2025)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res,vecSc.rsd,vecSc.ksd)
  listTab <-vector("list",5)
  vecNames <- c("Growth factor mean","Mean quantity of ressources","Number of resources","Growth factor sd","Carring capacity sd")
  vecIndicateur <-  c("Biomass","number of species","CRI")

                                        # vecSc.rmean <- c(2013,1013)
                                        # vecSc.kmean <- c(2017,1017)
                                        # vecSc.res <- c(2023,1023)
                                        # listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res)
                                        # listTab <-vector("list",3)
                                        # vecNames <- c("Growth factor","Mean quantity of resources","Number of ressources")
                                        # vecIndicateur <-  c("Biomass","Nummber of species","CRI")
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
                                        #  browser()

  repSortie <- paste(repGlobal,"SCB/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie,showWarnings = FALSE)
  colonneTab <- matrix(c(3,1,7,9,11,12,3,1,7,9,11,12,3,1,7,12,12,13,3,1,7,10,11,12,3,1,7,10,11,12),
                       5,6,byrow=TRUE)
  ## colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,12,12,13,3,1,7,5,10,11,12,3,1,7,5,10,11,12),
  ##                       5,7,byrow=TRUE)

  for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }

  colnames(listTab[[1]])[5] <- "rmean"
  colnames(listTab[[2]])[5] <- "kmean"
  colnames(listTab[[3]])[5] <- "ress"
  colnames(listTab[[4]])[5] <- "rsd"
  colnames(listTab[[5]])[5] <- "ksd"
  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:3] <- vecIndicateur
    colnames(listTab[[p]])[4] <- vecNames[p]
    
  }

  
   vecColorLine = c("#0b3352c8","#a02c2cc8") # c("red","blue")
   vecColorLine2 = c("#0b33527a","#a02c2c7a")# c("lightblue","pink")

#  vecColorLine2 = c("#e9b0af","#61c4dc")# c("lightblue","pink")
  vecColorPoint = c("#61c4dc7a","#e9b0af7a")# c("lightblue","pink")
#  vecColorPoint = c("#0b335258","#a02c2c58") # c("red","blue")

  vecTypePoint = c(20,20)
  vecTypeLine = c(1,1)
  vecEpaisseurLine <- c(8,8)

  library(graphics)

  for(i in 1:2){
    fileNamePlot = paste(repSortie,"sd_",i,".png",sep="") # avec l'IC95
    cat("\n \n  <-- ",fileNamePlot,"\n")
   png(file=fileNamePlot, width = 1200, height = 800,pointsize = 25)
#    x11()
  #  par(new=TRUE,mar=c(1,0,1,0))
     par(mar=c(0,0,0,0))
    m=matrix(1:2,1,2)
#    m = matrix(c(0:2,rep(0,3)),2,3,byrow=TRUE)
  #  layout(m,widths=c(1,rep(3,3)),heights=c(2,0.5))
    layout(m)
    ymax <- 0
    for(p in 4:5)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 4
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))
    #  browser()
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[4]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
 
    axis(2,cex=0.8,font=2)
#  mtext(vecIndicateur[i],
 #       side = 2, las = 3, line = 2,cex = 0.8,font=2)
  axis(1,cex=0.8,font=2)
#  mtext(vecNames[p], 
#        side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 

    
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){

      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
   for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
    minX <-  by(vec.x.sub, vec.groupe.sub, quantile,.025)
    minY <-  by(vec.y.sub, vec.groupe.sub, quantile,.025)
    minTab <- matrix(0,length(minX),2)
    for( j in 1:length(minX)){
      minTab[j,1]=minX[[j]][1]
      minTab[j,2]=minY[[j]][1]
    }
    lines(minTab[,1],minTab[,2],
          col= vecColorLine2[s],
          lwd=6,
          lty=2)
    
    maxX <-  by(vec.x.sub, vec.groupe.sub, quantile,.975)
    maxY <-  by(vec.y.sub, vec.groupe.sub, quantile,.975)
      maxTab <- matrix(0,length(maxX),2)
      for( j in 1:length(maxX)){
        maxTab[j,1]=maxX[[j]][1]
        maxTab[j,2]=maxY[[j]][1]
      }
      lines(maxTab[,1],maxTab[,2],
            col= vecColorLine2[s],
            lwd=6,lty=2)
      lines(meanTab[,1],meanTab[,2],
          col= vecColorLine[s],
          lwd=vecEpaisseurLine[s],
          lty=vecTypeLine[s])
    }
    
    p <- 5
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[4]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=
         "",
         ylim=c(0,ymax),axes=FALSE)
    
   axis(1,cex=0.8,font=2)
 # mtext(vecNames[p], 
 #       side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 

    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
         for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
    minX <-  by(vec.x.sub, vec.groupe.sub, quantile,.025)
    minY <-  by(vec.y.sub, vec.groupe.sub, quantile,.025)
    minTab <- matrix(0,length(minX),2)
    for( j in 1:length(minX)){
      minTab[j,1]=minX[[j]][1]
      minTab[j,2]=minY[[j]][1]
    }
    lines(minTab[,1],minTab[,2],
          col= vecColorLine2[s],
          lwd=6,
          lty=2)
    
    maxX <-  by(vec.x.sub, vec.groupe.sub, quantile,.975)
    maxY <-  by(vec.y.sub, vec.groupe.sub, quantile,.975)
      maxTab <- matrix(0,length(maxX),2)
      for( j in 1:length(maxX)){
        maxTab[j,1]=maxX[[j]][1]
        maxTab[j,2]=maxY[[j]][1]
      }
      lines(maxTab[,1],maxTab[,2],
            col= vecColorLine2[s],
            lwd=6,lty=2)
      lines(meanTab[,1],meanTab[,2],
          col= vecColorLine[s],
          lwd=vecEpaisseurLine[s],
          lty=vecTypeLine[s])
    }
    
 dev.off()
  }
 #browser()

  i <- 3
  fileNamePlot = paste(repSortie,"sd_",i,".png",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")
  png(file=fileNamePlot, width = 800, height = 800,pointsize = 25)
 # x11()
 par(mar=c(0,0,0,0))
#  browser()
  
  p <- 4
  vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
  vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
  vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
  vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))


  
  vecColor <- vecColorPoint[as.numeric(vec.sc)]
  vecType <- vecTypePoint[as.numeric(vec.sc)]
  titreX <- colnames(listTab[[p]])[5]
  titreY <- colnames(listTab[[p]])[i]
  
  plot(vec.x,vec.y,
       type='p',cex=1.5,
       col=vecColor,pch=vecType,
       xlab = "",ylab="",
      axes=FALSE)
  axis(2,cex=0.8,font=2)
#  mtext(vecIndicateur[i],
#        side = 2, las = 3, line = 2,cex = 0.8,font=2)
  
  
  axis(1,cex=0.8,font=2)
#  mtext(vecNames[p], 
#        side = 1, las = 1, line = 2, adj = 0.5,cex=0.8,font=2) 
  
  box()

  numScenar <-as.factor(levels(vec.sc)) ## as.factor(vecSc.rmean)
  ##
#  browser()
  for(s in 1:length(numScenar)){
    sc <- numScenar[s]
    vec.x.sub <- vec.x[which(vec.sc==sc)]
    vec.y.sub <- vec.y[which(vec.sc==sc)]
    vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
    
    meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
    meanTab <- matrix(0,length(meanList),2)
    for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
   for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
    minX <-  by(vec.x.sub, vec.groupe.sub, quantile,.025)
    minY <-  by(vec.y.sub, vec.groupe.sub, quantile,.025)
    minTab <- matrix(0,length(minX),2)
    for( j in 1:length(minX)){
      minTab[j,1]=minX[[j]][1]
      minTab[j,2]=minY[[j]][1]
    }
    lines(minTab[,1],minTab[,2],
          col= vecColorLine2[s],
          lwd=6,
          lty=2)
    
    maxX <-  by(vec.x.sub, vec.groupe.sub, quantile,.975)
    maxY <-  by(vec.y.sub, vec.groupe.sub, quantile,.975)
      maxTab <- matrix(0,length(maxX),2)
      for( j in 1:length(maxX)){
        maxTab[j,1]=maxX[[j]][1]
        maxTab[j,2]=maxY[[j]][1]
      }
      lines(maxTab[,1],maxTab[,2],
            col= vecColorLine2[s],
            lwd=6,lty=2)
      lines(meanTab[,1],meanTab[,2],
          col= vecColorLine[s],
          lwd=vecEpaisseurLine[s],
          lty=vecTypeLine[s])
    
    
  }
    
  dev.off()

  

}



graphePS.SCB.mean <-  function(){
  vecSc.rmean <- c(2013,1013)
  vecSc.rsd <- c(2015,1015)
  vecSc.kmean <- c(2017,1017)
  vecSc.ksd <- c(2019,1019)
  vecSc.res <- c(2025,1025)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res,vecSc.rsd,vecSc.ksd)
  listTab <-vector("list",5)
  vecNames <- c("Growth factor mean","Mean quantity of ressources","Number of resources","Growth factor sd","Carring capacity sd")
  vecIndicateur <-  c("Biomass","number of species","CRI")

                                        # vecSc.rmean <- c(2013,1013)
                                        # vecSc.kmean <- c(2017,1017)
                                        # vecSc.res <- c(2023,1023)
                                        # listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res)
                                        # listTab <-vector("list",3)
                                        # vecNames <- c("Growth factor","Mean quantity of resources","Number of ressources")
                                        # vecIndicateur <-  c("Biomass","Nummber of species","CRI")
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
                                        #  browser()

  repSortie <- paste(repGlobal,"SCB/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie,showWarnings = FALSE)
  colonneTab <- matrix(c(3,1,7,9,11,12,3,1,7,9,11,12,3,1,7,12,12,13,3,1,7,10,11,12,3,1,7,10,11,12),
                       5,6,byrow=TRUE)
  ## colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,12,12,13,3,1,7,5,10,11,12,3,1,7,5,10,11,12),
  ##                       5,7,byrow=TRUE)

  for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }

  colnames(listTab[[1]])[5] <- "rmean"
  colnames(listTab[[2]])[5] <- "kmean"
  colnames(listTab[[3]])[5] <- "ress"
  colnames(listTab[[4]])[5] <- "rsd"
  colnames(listTab[[5]])[5] <- "ksd"
  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:3] <- vecIndicateur
    colnames(listTab[[p]])[4] <- vecNames[p]
    
  }

  
 
  vecColorLine = c("red","blue")
  vecColorPoint =  c("pink","lightblue")
  vecTypePoint = c(20,20)
  vecTypeLine = c(1,2)
  vecEpaisseurLine <- c(4,4)

  library(graphics)

  for (i in 1:2){
    fileNamePlot = paste(repSortie,"mean_",i,".eps",sep="") # avec l'IC95
    cat("\n \n  <-- ",fileNamePlot,"\n")

    postscript(file=fileNamePlot, width = 900, height = 400,pointsize = 25)
    savePar <- par(mar=c(1,0,1,0))
    
    
    m = matrix(c(0:3,rep(0,4)),2,4,byrow=TRUE)
    layout(m,widths=c(1,rep(4,3)),heights=c(3,0.5))
    ##  layout.show(3)
    
    ymax = 0
    for(p in 1:3)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 1
    ##  browser()
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreY <- colnames(listTab[[p]])[5]
    titreX <- colnames(listTab[[p]])[i]
 
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
    axis(2,cex=0.5)
    mtext(vecIndicateur[i],
          side = 2, las = 3, line = 2,cex = 0.8,font=2)
    axis(1,cex=0.5)
    mtext(vecNames[p], 
          side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 
    box()

    numScenar <-  as.factor(vecSc.rmean)
    ##as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[s],
            lwd=vecEpaisseurLine[s],
            lty=vecTypeLine[s])
    }
    
    p <- 2
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),axes=FALSE)
    
    axis(1,cex=0.5)
    mtext(vecNames[p], 
          side = 1, las = 1, line = 4, adj = 0.5,cex=0.8,font=2)
    
    box()
    ##   numScenar <-as.factor(levels(vec.sc))
    numScenar <-  as.factor(vecSc.kmean)
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub),vec.groupe.sub,mean)
      meanTab <- matrix(0,length(meanList),2)
      ## tendance <- lm(vec.y.sub~vec.x.sub)
      
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
   
      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[s],
            lwd=vecEpaisseurLine[s],
            lty=vecTypeLine[s])     
      
    }
    
    p <- 3
    
    vec.x <- 9-(as.vector(as.matrix(listTab[[p]][,5])))+1
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

    if (i>2){
      ## browser()
      vec.y.save <- vec.y
      vec.x <- vec.x[which(vec.y>0)]
      vec.sc <- vec.sc[which(vec.y>0)]
      vec.groupe <- vec.groupe[which(vec.y>0)]
      vec.y <- vec.y[which(vec.y>0)]
    }
    ##   browser()
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[5]
    titreY <- colnames(listTab[[p]])[i]

    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab="",
         ylim=c(0,ymax),xlim=c(1,9),axes=FALSE)
    
    axis(1,cex=0.5)
    mtext(vecNames[p], 
          side = 1, las = 1, line = 3,adj = 0.5,cex=0.8,font=2)


    box()


    numScenar <-  as.factor(vecSc.res)
    ##  numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
                                        #browser()
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in which(summary(vec.groupe.sub)>0)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      
      ##    browser()
      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[s],
            lwd=vecEpaisseurLine[s],
            lty=vecTypeLine[s])
    }
    dev.off()    

  }



  
  ## browser()
  i <- 3
  fileNamePlot = paste(repSortie,"mean_",i,".eps",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")
  postscript(file=fileNamePlot, width = 300, height = 400,pointsize = 25)
  
  ymax = 0
  
  p <- 1
  vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
  vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
  vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
  vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

  
  ## browser()
  vec.y.save <- vec.y
  vec.x <- vec.x[which(vec.y>0)]
  vec.sc <- vec.sc[which(vec.y>0)]
  vec.groupe <- vec.groupe[which(vec.y>0)]
  vec.y <- vec.y[which(vec.y>0)]
  
  
  vecColor <- vecColorPoint[as.numeric(vec.sc)]
  vecType <- vecTypePoint[as.numeric(vec.sc)]
  titreX <- colnames(listTab[[p]])[5]
  titreY <- colnames(listTab[[p]])[i]
  
  plot(vec.x,vec.y,
       type='p',cex=1.5,
       col=vecColor,pch=vecType,
       xlab = "",ylab="",
       ylim=c(0,max(vec.y)),axes=FALSE)
  axis(2,cex=0.5)
  mtext(vecIndicateur[i],
        side = 2, las = 3, line = 2,cex = 0.8,font=2)
  axis(1,cex=0.5)
  mtext(vecNames[p], 
        side = 1, las = 1, line = 2, adj = 0.5,cex=0.8,font=2) 
  box()
  numScenar <-  as.factor(vecSc.rmean)
  ##as.factor(levels(vec.sc))
  
  for(s in 1:length(numScenar)){
    sc <- numScenar[s]
    vec.x.sub <- vec.x[which(vec.sc==sc)]
    vec.y.sub <- vec.y[which(vec.sc==sc)]
    vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
    
    meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
    meanTab <- matrix(0,length(meanList),2)
    for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
    lines(meanTab[,1],meanTab[,2],
          col= vecColorLine[s],
          lwd=vecEpaisseurLine[s],
          lty=vecTypeLine[s])
  }
  
  dev.off()
  ##browser() 


  
  for(i in 1:2){
    fileNamePlot = paste(repSortie,"sd_",i,".eps",sep="") # avec l'IC95
    cat("\n \n  <-- ",fileNamePlot,"\n")
    postscript(file=fileNamePlot, width = 900, height = 400,pointsize = 25)
    par(new=TRUE,mar=c(1,0,1,0))
    m = matrix(c(0:2,rep(0,3)),2,3,byrow=TRUE)
    layout(m,widths=c(1,rep(3,3)),heights=c(2,0.5))
    ymax <- 0
    for(p in 4:5)
      if (max(listTab[[p]][i])> ymax) ymax <- max(listTab[[p]][i])
    
    p <- 4
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[4]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=titreY,
         ylim=c(0,ymax),axes=FALSE)
##    browser()
    axis(2,cex=0.5)
  mtext(vecIndicateur[i],
        side = 2, las = 3, line = 2,cex = 0.8,font=2)
  axis(1,cex=0.5)
  mtext(vecNames[p], 
        side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 

    
    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){

      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[as.numeric(sc)],
            lty=vecTypeLine[as.numeric(sc)],
            lwd=vecEpaisseurLine[as.numeric(sc)])
    }
    
    p <- 5
    vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
    vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
    vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
    vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))
    
    vecColor <- vecColorPoint[as.numeric(vec.sc)]
    vecType <- vecTypePoint[as.numeric(vec.sc)]
    titreX <- colnames(listTab[[p]])[4]
    titreY <- colnames(listTab[[p]])[i]
    
    plot(vec.x,vec.y,
         type='p',cex=1.5,
         col=vecColor,pch=vecType,
         xlab = titreX,ylab=
         "",
         ylim=c(0,ymax),axes=FALSE)
    
   axis(1,cex=0.5)
  mtext(vecNames[p], 
        side = 1, las = 1, line = 3, adj = 0.5,cex=0.8,font=2) 

    box()
    numScenar <-as.factor(levels(vec.sc))
    
    for(s in 1:length(numScenar)){
      sc <- numScenar[s]
      vec.x.sub <- vec.x[which(vec.sc==sc)]
      vec.y.sub <- vec.y[which(vec.sc==sc)]
      vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
      
      meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
      meanTab <- matrix(0,length(meanList),2)
      for( j in 1:length(meanList)){
        meanTab[j,1]=meanList[[j]][1]
        meanTab[j,2]=meanList[[j]][2]
      }
      lines(meanTab[,1],meanTab[,2],
            col= vecColorLine[as.numeric(sc)],
            lty=vecTypeLine[as.numeric(sc)],
            lwd=vecEpaisseurLine[as.numeric(sc)])
    }
  }
  dev.off()

  i <- 3
  fileNamePlot = paste(repSortie,"sd_",i,".eps",sep="") # avec l'IC95
  cat("\n \n  <-- ",fileNamePlot,"\n")
  postscript(file=fileNamePlot, width = 300, height = 400,pointsize = 25)
  
  ymax = 0
  
  
  p <- 4
  vec.x <- as.vector(as.matrix(listTab[[p]][,4]))
  vec.y <- as.vector(as.matrix(listTab[[p]][,i]))
  vec.sc <- as.factor(as.vector(as.matrix(listTab[[p]]$sc)))
  vec.groupe <- as.factor(as.vector(as.matrix(listTab[[p]][,5])))

  
  ## browser()
  vec.y.save <- vec.y
  vec.x <- vec.x[which(vec.y>0)]
  vec.sc <- vec.sc[which(vec.y>0)]
  vec.groupe <- vec.groupe[which(vec.y>0)]
  vec.y <- vec.y[which(vec.y>0)]
  
  
  vecColor <- vecColorPoint[as.numeric(vec.sc)]
  vecType <- vecTypePoint[as.numeric(vec.sc)]
  titreX <- colnames(listTab[[p]])[5]
  titreY <- colnames(listTab[[p]])[i]
  
  plot(vec.x,vec.y,
       type='p',cex=1.5,
       col=vecColor,pch=vecType,
       xlab = "",ylab="",
       ylim=c(0,ymax),axes=FALSE)
  axis(2,cex=0.5)
  mtext(vecIndicateur[i],
        side = 2, las = 3, line = 2,cex = 0.8,font=2)
  
  
  axis(1,cex=0.5)
  mtext(vecNames[p], 
        side = 1, las = 1, line = 2, adj = 0.5,cex=0.8,font=2) 
  
  
  
  
  box()

  numScenar <-  as.factor(vecSc.rmean)
  ##as.factor(levels(vec.sc))
  
  for(s in 1:length(numScenar)){
    sc <- numScenar[s]
    vec.x.sub <- vec.x[which(vec.sc==sc)]
    vec.y.sub <- vec.y[which(vec.sc==sc)]
    vec.groupe.sub <-vec.groupe[which(vec.sc==sc)] 
    
    meanList <-  by(cbind(vec.x.sub,vec.y.sub), vec.groupe.sub, mean)
    meanTab <- matrix(0,length(meanList),2)
    for( j in 1:length(meanList)){
      meanTab[j,1]=meanList[[j]][1]
      meanTab[j,2]=meanList[[j]][2]
    }
    lines(meanTab[,1],meanTab[,2],col= vecColorLine[s],lwd=vecEpaisseurLine[s],lty=vecTypeLine[s])
  }
  
  dev.off()
  

}


## compareScenar(c(105,106,203,204),parametre1="initV_capaciteRessourceAlimMoyREEL",parametre2="initV_capaciteRessourceAlimSdREEL")
## compareScenar(c(102,103,201,202),"initE_facteurCroissanceMoyREEL","initE_facteurCroissanceSdREEL")
##compareScenar(c(1017,1018,2017,2018),parametre1="initV_capaciteRessourceAlimMoyREEL",parametre2="initV_capaciteRessourceAlimSdREEL")
##
## compareScenar(c(1013,1014,2013,2014),"initE_facteurCroissanceMoyREEL","initE_facteurCroissanceSdREEL")
## compareScenar(c(1015,1016,2015,2016),"initE_facteurCroissanceMoyREEL","initE_facteurCroissanceSdREEL")
## compareScenar(c(1017,1018,2017,2018),parametre1="initV_capaciteRessourceAlimMoyREEL",parametre2="initV_capaciteRessourceAlimSdREEL")
## compareScenar(c(1019,1020,2019,2020),parametre1="initV_capaciteRessourceAlimMoyREEL",parametre2="initV_capaciteRessourceAlimSdREEL")
## compareScenar(c(1021,1022,2021,2022),"initE_facteurCroissanceMoyREEL","X.init_nbRessourceAlimentaire.") 
## compareScenar(c(1023,1024,2023,2024),"initV_capaciteRessourceAlimMoyREEL","X.init_nbRessourceAlimentaire.")


calculRsquareSN <- function(){

  vecSc.rmean <- c(1013,1014,2013,2014)
  vecSc.rsd <- c(1015,1016,2015,2016) 
  vecSc.kmean <- c(1017,1018,2017,2018)
  vecSc.ksd <- c(1019,1020,2019,2020)
  ##vecSc.res <- c(1023,1024,2023,2024)
  vecSc.res <- c(1025,1026,2025,2026)
  listVecSc <- list(vecSc.rmean,vecSc.kmean,vecSc.res,vecSc.rsd,vecSc.ksd)
  listTab <-vector("list",5)
  vecNames <- c("Growth factor mean","Mean quantity of ressources","Number of resources","Growth factor sd","Carring capacity sd")
  vecIndicateur <-  c("Biomass","number of species","CRI","Simpson")
  repGlobal <- "/home/romain/1_Recherche/Model/Batch/"
  ## browser()

  repSortie <- paste(repGlobal,"Graph1/",sep="")
  ##  fichierDataSortie <- paste("Summarydata",serie,"00.csv",sep="")

  dir.create(repSortie)
  colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,12,12,13,3,1,7,5,10,11,12,3,1,7,5,10,11,12),5,7,byrow=TRUE)
  ## colonneTab <- matrix(c(3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,9,11,12,3,1,7,5,10,11,12,3,1,7,5,10,11,12),5,7,byrow=TRUE)

  for(p in 1:length(listVecSc)){
    for(s in listVecSc[[p]]){
      repScenar <-  paste(repGlobal,"BatchModel",s,"/",sep="")
      fichierDataSortie <- paste("CommunauteResultatsResum/Summarydata",s,sep="")
      fichierData <- paste(repScenar,fichierDataSortie,".csv",sep="")
      d1 <-  read.csv(fichierData)
      d1 <-  data.frame(d1,sc=as.factor(s))
      if ( listVecSc[[p]][1]== s) d <-  d1
      else d <- rbind(d,d1)
    }
   ## browser()
    listTab[[p]] <- d[sample(1:nrow(d)),colonneTab[p,]]
  }
  colnames(listTab[[1]])[6] <- "rmean"
  colnames(listTab[[2]])[6] <- "kmean"
  colnames(listTab[[3]])[6] <- "ress"
  colnames(listTab[[4]])[6] <- "rsd"
  colnames(listTab[[5]])[6] <- "ksd"
##browser()
  for(p in 1:length(listVecSc)){
    colnames(listTab[[p]])[1:4] <- vecIndicateur
    colnames(listTab[[p]])[5] <- vecNames[p]
    
  }

  vecColorLine <- c("red","red","blue","blue")
  vecColorPoint <- c("pink","pink","lightblue","lightblue")
  vecTypePoint <- c(1,3,1,3)
  vecTypeLine <- c(1,2,1,2)
  vecEpaisseurLine <- c(2.5,4,2.5,4)
  library(graphics)

  ## browser()

}

test.logNormal <- function(){
  vecSd <- (1:1000)/100
  vecSdOut <- vector()
  vecMeanOut <- vector()
  for (s in vecSd){
    vecG <- rlnorm(10000,meanlog=log(1.1),sdlog=log(s))
    ## summary(vecG)
    ## print(sqrt(var(vecG)))
    ## hist(vecG,density=TRUE)
    vecSdOut <- c(vecSdOut,sqrt(var(vecG)))
    vecMeanOut <- c(vecMeanOut,mean(vecG))
  }
  vecSdSub <- vecSd[which(vecSdOut<1.1)]
  vecSdOutSub <- vecSdOut[which(vecSdOut<1.1)]
  plot(vecSdOutSub,vecSdSub)
  mod <- lm(vecSd~vecSdOut)
  print(summary(mod))
  abline(mod)
}


test.normal <- function(n,m,s){
  mlog <- log(m)
  slog <- log(s)
  u <- runif(n)
  v <- runif(n)
 
  layout(matrix(1:2, 2,1))
  layout.show(2)

  cat('NORMAL\n-------\n')
  vecNorm <- (sqrt(-2*log(u))*cos(2*pi*v))*slog+mlog
  
  hist(vecNorm,breaks=n/10)
  print(summary(vecNorm))
  print(sd(vecNorm))

  cat('LOG-NORMAL\n----------\n')
  vecLogNorm <- log(m+s*v)

  hist(vecLogNorm,breaks=n/10)
  print(summary(vecNorm))
  print(sd(vecNorm))

    return(vecNorm)

}
