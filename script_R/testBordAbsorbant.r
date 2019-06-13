pop1 <- 10
pop2 <- 20
pop3 <- 30
pop4 <- 50

pop <- data.frame(pop1,pop2,pop3,pop4)
vecAlea <- c(.5,1,2)

rangeTirage <- function(vec,n=1)
  return(runif(n,vec[1],vec[2]))


for(i in 1:100){
  alea <- vecAlea[round(runif(1,1,3),0)]
  cat(alea,"\n")
  pop[i,] <- pop[i,]*alea
  newPop <- pop[i,]*1
  newPop <- as.integer(trunc(newPop))
  for(p in (1:length(newPop)))
    newPop[p] <- rpois(1,newPop[p])
  pop <- rbind(pop,newPop)
}
cat("\n")
matplot(pop,type='l')
