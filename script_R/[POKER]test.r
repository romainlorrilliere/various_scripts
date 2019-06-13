##' @param nbJoueur
##' @param mise

##' @return ...

calcul.gain <- function(nbJoueur,mise=100){
  
  joueur.mise <- rep(mise,nbJoueur)
  pot <-  sum(joueur.mise)
  
  gain <- rep(0,length(joueur.mise))

  joueurMilieu <- round(nbJoueur/2+0.1)
  gain[joueurMilieu] <- joueur.mise[1]*1.1
  ppart <- 0.6
  
  gain[1] <- round(pot*ppart)
 # if (length(joueur.mise)>6)
 #   for (i in (joueurMilieu+1):(nbJoueur-1) )
  for (i in 2:(nbJoueur-1) ){
    ppart=ppart*0.95
    gain[i] <- round(gain[i-1]*ppart)
  }

  reste <- pot - sum(gain)
  part <- reste/(joueurMilieu -1)
  ##ppart <- 0.5
  
#  for(i in 1:(joueurMilieu-1)){
#    gain[i] <- round(part*ppart)
#    for (j in 1:i)
#      gain[j] <- gain[j]+round((part*(1-ppart))/i)
#  }
      
  resultat <- gain- joueur.mise
  #gain[1] <- gain[1]-sum(resultat)
 # resultat <- gain- joueur.mise
  print(gain)
  
  print(sum(resultat))
  return(gain)
}
