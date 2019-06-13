##########################################
# prise en main de ESS
#######################################

nbMax = 1000
a = 1:nbMax
b = a + 3
for(i in a){
  c = a+b+rnorm(nbMax,10,10)
  b = c
  cat(i)
}
plot(a,b,type='l')



library(graphics)
vecValeur <- seq(0.5,2,length.out=10)
valeurTitre <- paste(round(vecValeur,2),collapse=" | ")
vecCol <- rep(c("tomato","violet","orange","chocolate","olivedrab"),2)
i <-  1
  plot(density(rnorm(10000,vecValeur[i],0.1), adjust = 10),col=vecCol[i],xlim= c(0,2.5),
        main = "distribution des lambda\n sd=0.1",ylab="",xlab="lambda",sub=valeurTitre)
for ( i in 2:10)
  lines(density(rnorm(10000,vecValeur[i],0.1), adjust = 10),col=vecCol[i])
 savePlot("LambaVsSd","png")
