##########################################################
##   Script de recherche de l'equilibre
#########################################################


## 1- lire de fichier sortie des batchs

## 2- boucles de régression linéaire

## 3- recherche de la date d'équlibre

## 4- ficher de sortie

# indicateur a tester :
# ---------------------

# abondance moyennes des populations
# biomasse
# CRI
# nombre d especes
# simpson




source("/home/romain/1_Recherche/script_R/myRfunction.r")

source("/home/romain/1_Recherche/script_R/[COMMUNAUTE_MIGR]batchMod.r")


                                        # rechercheEquilibre():DATA.FRAME
                                        # ------------------------------------------------
                                        # 
                                        # + repertoire : {PATH} repertoire générale du batch
                                        # + serie: {INT} identifiant du Batch
                                        # + idSimul: {INT} identifiant de la simulation
                                        # + rep:{INT} indentification
                                        # -> {DATA.FRAME}


rechercheEquilibre <-  function(repertoire="/home/romain/1_Recherche/Model/Batch/BatchModel1013/",
                                               serie=1013,idSimul=1,rep=100){

cat("---------------------------\n")
cat("|      Recherche Teq       |\n")
cat("---------------------------\n\n")
  vecTeq = vector()
  timeStart = Sys.time()
  for(s in 1:10){
    cat("s: ",s,"\n")
    tabMC <- monteCarlo.nbEspeces(uneSimul = s)
    Teq <- regressionParametre(tab=tabMC)
    vecTeq <- c(vecTeq,Teq)
    estimDateFin(h1=timeStart,repTot=10,repNow=1)
  }
 
  cat(" --- Vecteur Teq ---\n")
  cat(vecTeq)

  return(vecTeq)

}



monteCarlo.nbEspeces <- function(repertoire="/home/romain/1_Recherche/Model/Batch/BatchModel1013/",
                                               uneSerie=1013,uneSimul=1,rep=100){
  vecSum <-  rep(0,2002)
  cat("  MonteCarlo sp ")

  for(r in 1:rep){
    barreDefilement(r,rep)
      tabDyn <- lectureFichierSortieFreePascal.pop(repertoire,serie = uneSerie, idSimul = uneSimul, rep = r)
##    browser()
    vecSum <-  vecSum + apply(tabDyn,1,calculNbSp)
    
  }
  vecSum <-  vecSum / rep
  tabSum = cbind(1:2002,vecSum)
  colnames(tabSum)=c("Y","S")
  tabSum = as.data.frame(tabSum)
  cat("\n")  
  return(tabSum)

}


regressionParametre <-  function(delais=50,pas=10,testEqui=0.01,tab){

  flag <- TRUE
  i <-  1
  cat("  Recherche Teq ")
  while (flag){
##    browser()
    barreDefilement(i,2002)
    tabRegression <-  tab[i:(i+delais),]
    regression.pValue <- regression(tabRegression)
    if(regression.pValue > testEqui){
      equi <- i
      flag <- FALSE
    }
    if ((i + delais)>nrow(tab)) flag <- FALSE
    i <-  i + pas
  }
  cat("\n  Teq = ",equi,"\n")
  return(equi)
}


regression <- function(tab){
  reg = lm(formula = S ~ Y, data = tab)
  reg.pValue = as.data.frame(summary(reg)$coefficients)[2,4]
  return(reg.pValue)
}

