

escalier <- function(H) {


get_g <- function(h) 65-(2*h)

vecH<- 10:30


vecg <- get_g(vecH)


nbmarche <- H %/% vecH

long <- vecg * nbmarche/100


d <- data.frame(hauteur = vecH * nbmarche, ajsutement = 100 %% vecH,longueur = long,
                nb_marche = nbmarche,
                contremarche = vecH,giron=vecg
                )


return(d)


}
#2.4/6
