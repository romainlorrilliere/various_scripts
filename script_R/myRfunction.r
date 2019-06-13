## #########################################################
##   myRfunction : Mes fonctions R génériques développées
## #########################################################



                                        # normaleTirage(): FLOAT
                                        # -----------------------
                                        # Tirage de n valeur dans une loi normale
                                        # de moyenne vec[1] de d'écart type vec[2]
                                        # + n {INT}
                                        # + vec {[FLOAT]}(length=2)
                                        # -> {FLAOT}
normaleTirage <- function(vec,n=1) 
  return(rnorm(n,vec[1],vec[2]))


                                        # rangeTirage(): FLOAT
                                        # ---------------------
                                        # tirage de n valeur dans une loi uniforme
                                        # entre vec[1] et vec[2]
                                        # + n {INT}
                                        # + vec {[FLOAT]}(length=2)
                                        # -> {FLOAT}
                                        
rangeTirage <- function(vec,n=1)
  return(runif(n,vec[1],vec[2]))



                                        # barreDefillement(): void
                                        # -------------------------
                                        # rélisation d'un affichage console
                                        #  de l'avancement du process
                                        # + t {INT} indice incrémenter dans le process (boucle)
                                        # + nbRep {INT} nombre de boucle (t max)

barreDefilement <-  function(t,nbRep) {

  if (t == 1) cat("|")
  if (nbRep >= 10){
    if (nbRep >= 100){
      deuxpc <- nbRep %/% (100/2)
      if(t %% deuxpc == 0)
        if( t %% (deuxpc * 25) == 0)cat("|")
        else
          if( t %% (deuxpc * 5) == 0)cat(":")
          else
            if(t == nbRep)  cat("|")
            else cat(".")
    }
    else{
      dixpc <- nbRep %/% (100/10)
      if(t %% dixpc == 0)
        
        if( t %% (dixpc * 5) == 0)
          if(t == nbRep)  cat("|")
          else
            cat(":")
      
        else cat(".")
    }
  }
  else
    if (t == nbRep) cat("|")
    else cat(".")
}


                                        # calculNbSp(): INT
                                        # ------------------
                                        # calcul le nombre d'espece
                                        # calcul dans vec le nombre de valeur > 0
                                        # + vec {[FLOAT]} abondance par poppulation
                                        # -> {INT}

calculNbSp <- function(vec){
  val <-length(subset(vec,vec>0))
  return(val)
}

                                        # calculBiomasse(): FLOAT
                                        # ------------------------
                                        # calcul la biomasse total
                                        # la somme du vecteur
                                        # + vec {[FLOAT]} abondance par poppulation
                                        # -> {FLOAT}

calculBiomasse <- function(vec){
  val <- sum(vec)
  return(val)
}

                                        # calculShannon(): FLOAT
                                        # -----------------------
                                        # calcul l'indice de Shannon
                                        # et celui de Pielou

                                        # + vec {[FLOAT]} abondance par poppulation
                                        # + methode {STRING} ("")
                                        #       "" ou "pielou"
                                        # -> {FLOAT}

calculShannon <- function(vec,methode="" ){
  if (sum(vec)>0){
    vec <- subset(vec,vec>0)
    s <-  length(vec)
    p <- vec/sum(vec)
    val <- -sum(p*log(p))
    if(methode=="pielou"){
      if (s > 1)
      val <- val/log(s)
      else
      val <- 0
    }
  }
  else val <- 0
  return(val)
}

                                        # calculSimpson():FLOAT
                                        # ---------------------
                                        # calcul de l'indice de Simpson
                                        # et des variante (biodiversité, réciproque)

                                        # + vec {[FLOAT]} abondance par poppulation
                                        # + methode {STRING} ("")
                                        #        "" ou "reciproque" ou "diversite"
                                        # -> {FLOAT}


calculSimpson <- function(vec,methode=""){
  vec = subset(vec,vec>0)
  if(sum(vec)>0) {
    p <- vec/sum(vec)
    val <- sum(p^2)
  }
  else {
    val = 1
  }
  if(methode == "") return(val)
  if(methode == "reciproque") return(1/val)
  if(methode == "diversite") return(1-val)
}

                                        # calculProportionCarre():FLOAT
                                        #------------------------------
                                        #

calculProportionCarre <- function(vec,total){
  return((sum(vec)/total)^2)
}

                                        # mutipicationVecteur():VECTOR(FLOAT)
                                        # ----------------------------------
                                        # multiplication cartésienne de 2 vecteurs même taille

multiplicationVecteur <- function(vec1,vec2) {
  return(vec1*vec2)
}

                                        # calculSimpsonFonct():FLOAT
                                        # -------------------------

                                        # NE FONCTIONNE PAS !!!!!!!!

                                        # calcul d'un indice fonctionnel de Simpson
                                        # et des variantes (biodiversité, réciproque)

                                        # + vecAbond{[FLOAT]} abondance par poppulation
                                        # + tabPart{MATRIX[FLOAT]} table des parts
                                        #      de besoin des pop en ressources
                                        # + methode {STRING} ("")
                                        #        "" ou "reciproque" ou "diversite"
                                        # + pond {BOOL} (FALSE) calcul pondére sur les besoins
                                        # -> {FLOAT}

calculSimpsonFonct <- function(vecAbond,tabPart,methode="",pond=FALSE){
  if (sum(vecAbond) > 0){
      if (pond==FALSE) {
        tabPart <- ifelse(tabPart>0,1,0)
      }
                                        # tabPartPoid {MATRIX[FLOAT]} poids biomasse de chaque pop sur les ressources
                                        # multiplicationVecteur() {local}
      tabPartPoid <- apply(tabPart,1,multiplicationVecteur,vecAbond)
                                        # Rtot {FLOAT} somme de tout la biomasse suporté par toutes les ressources
      Rtot <-  sum(tabPartPoid)
                                        # Df {FLOAT} simpson fonctionnel
                                        #   somme 
      Df <- sum(apply(tabPartPoid,1,calculProportionCarre,Rtot))
    }
  else
    Df <- 1
  if(methode == "") return(Df)
  if(methode == "reciproque") return(1/Df)
  if(methode == "diversite") return(1-Df)

}



                                        # calculConnectance():FLOAT
                                        # ----------------------------

                                        # Calucul de la connectance trophique d'une communauté

                                        # + vecAbond{[FLOAT]} abondance par poppulation
                                        # + tabPart{MATRIX[FLOAT]} table des parts
                                        #      de besoin des pop en ressources
                              
calculConnectance <- function(vecAbond,tabPart){
  if (sum(vecAbond) > 0){
                                        # on transforme la table des parts en table presence absence
    tabPart <- ifelse(tabPart>0,1,0)
                                        # on enlève les populations d'abondance nul
    tabPart <-  tabPart[,which(vecAbond > 0)]
                                        # nombre de connexion sur
                                        #  le nombre total de connexion possible
    connect <- sum(tabPart)/(nrow(tabPart)*ncol(tabPart))
    }
  else
    connect <- 0
  return(connect)
}


                                        # calculSRI():FLOAT
                                        # ------------------
                                        # calcul du Species Requirement Index
                                        # calcul sur le shema du SSI
                                        # + vec : {[FLOAT]} vec des besoins

                                        # 0 : l'espèce est dépendentes de toute les ressources
                                        


calculSRI <- function(vec){
  return(length(subset(vec,vec>0)))
}

  

                                        # calculCRI():FLOAT
                                        # ------------------
                                        # calcul du Community Requierement Index
                                        # calcul réalisé sur le shéma du CSI
                                        # + vecAbond : {[FLOAT]} vecteur des abondances
                                        # + tabPart : {matrix[FLOAT]} table des besoins


calculCRI <- function(vecAbond,vecSRI){
  if (sum(vecAbond)==0)
    return(0)
  else
    return(sum(vecSRI*vecAbond)/sum(vecAbond))
}








                                        # file.exist():BOOLEAN
                                        # ------------------------
                                        # test l'esitence d'un fichier dans un paths
                                        # + nameFile {STRING}
                                        # + repertoire {PATH}
                                        # -> {BOOL}

file.exist <- function(nameFile,repertoire)
  return(length(which(dir(repertoire)==nameFile))>0)



                                        # estimDateFin() :void
                                        # -----------------------
                                        # estime et affiche la duré de simulation restante
                                        # et la date et l'heure de la fin de simulation
                                        # utilise dans un process BOUCLE

                                        # + h1: {DATE} date de début de simulation
                                        # + repTot : {INT} nombre de répétitions total
                                        # + repNow : {INT} nombre de répétitions effectuées

estimDateFin <- function(h1,repTot,repNow){
                                        # h2 {DATE} date et heure de l'instant
  h2 <- Sys.time()
                                        #  diffSec {DIFFTIME} temps écoulé entre h1 et h2 en sec
  diffSec <- difftime(h2,h1,units="secs")
                                        # timeByStep {DIFFTIME} temps par step
  timeByStep <- diffSec / repNow
                                        # timeToEnd {DIFFTIME} estimation duré simulation
  timeToEnd <- (repTot - repNow) * timeByStep
                                        # dateEnd {DATE} estimation date de fin
  dateEnd <- format(Sys.time()+ as.numeric(timeToEnd,units="secs") ,format="%d/%m/%y %H:%M")
                                        # timeEstimate {FLOAT} nombre d'heures restantes
  timeEstimate <-  round(as.numeric(timeToEnd,units="hours"),1)
                                        # si moins d'1 heure
  if (timeEstimate < 1){
                                        # timeEstimata {FLOAT} nombre de minutes restantes
    timeEstimate <-  round(as.numeric(timeToEnd,units="mins"))
    cat("  * Estim:",timeEstimate,"minute(s) -> Fin batch:",dateEnd,"*\n") 
  
  }
  else  cat("  * Estim:",timeEstimate,"heure(s) -> Fin batch:",dateEnd,"*\n") 
}



                                        # makeTableCombinaison(): data.frame
                                        # -----------------------------------
                                        # fabrique la table de tous les combinaisons possible
                                        #    sans contrainte
                                        # + uneListe {LISTE} des séries à combiner

makeTableCombinaison <- function(uneListe,parmetreMatch=FALSE){
#browser()
  lengthVecteur = vector()
  for(i in 1:length(uneListe)) lengthVecteur <- c(lengthVecteur,length(uneListe[[i]]))
                                        # nbJeuxParam : {INT} calcul du nombre de juex de parametre
  nbCombine <-  prod(lengthVecteur)
  combinIndice <- data.frame(matrix(0,nbCombine,length(uneListe)))
  for(i in 1:length(uneListe)){
    if(i == length(uneListe))
      vec <- uneListe[[i]]
    else{
      lengthVecteurReste = vector()
      for(j in (i+1):length(uneListe))
        lengthVecteurReste <- c(lengthVecteurReste,length(uneListe[[j]]))
       nbCombineReste <-  prod(lengthVecteurReste)

     vec <- rep(uneListe[[i]],each=nbCombineReste)
    }
     combinIndice[,i] <- rep(vec,nbCombine%/%length(vec))
  }
colnames(combinIndice) <- names(uneListe)
return(combinIndice)
}


valeurInList <-  function(val,list){
  bool <-  sum(as.numeric(val == list))> 0
  return(bool)
}


vecteurInList <-  function(vec,list){
   vecBool <- vector()
    for(v in vec)
     vecBool <- c(vecBool,valeurInList(v,list))
    return(vecBool)
}


linesSmooth <- function(vecX,vecY){
  require(splines)
  yy <-predict(interpSpline(x, y))
  lines(yy)
}
