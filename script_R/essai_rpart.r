# model A tout pas de ²
# ---------------------------

rm(list=ls())
repertoire = "D:/Recherche/Héron/RardeaOrsay/"
data85 = read.table(
paste(repertoire,"GLMFrance85.txt",sep=''), header = T)
data89 = read.table(
 paste(repertoire,"GLMFrance89.txt",sep=''), header = T)
data = rbind(data85,data89)
attach(data)

#detach(data)
#
#
model = glm(newCol ~ col  + eff + hs + hsMean + col1 + eff1 + col2 + eff2 + col3 + eff3 + col5 + eff5 + col10 + eff10 + colTotal + effTotal   ,  family=poisson)


model = glm(newCol ~  hs + hsMean + col + col1 + col2 + col3 + col5 + col10 + eff + eff1 + eff2 + eff3 + eff5 + eff10 + colTotal + I(col1^2) + I(col2^2) + effTotal +an + I(effTotal^2), family=poisson)


model = glm(newCol ~  hs + col + col1 + col2 + col3 + col5 + col10 + eff + eff1 + eff2 + colTotal + I(col1^2) + I(col2^2) + effTotal +an  + I(an^2)+ I(effTotal^2), family=poisson)
step(model)
#summary(model)

  
#
model = glm(newCol ~ col +  hs + hsMean + col1 + col2 + col3 + col5 + col10 + colTotal ,  family=poisson)

step(model)


model = glm(newCol ~  hs + hsMean + col + col1 + col2 + col3 + col5 + col10 + col + I(col1^2) + I(col2^2) + I(col3^2) + I(col5^2) + I(col10^2) , family=poisson)
step(model)

model = glm(newCol ~ col + I(col^2) + eff + I(eff^2) + hs + hsMean + col1 + I(col1^2) + eff1 + I(eff1^2) + col2 + I(col2^2)  + eff2 + I(eff2^2)  + col3 + I(col3^2)  + eff3 + I(eff3^2)  + col5 + I(col5^2)  + eff5 + I(eff5^2) + col10 + I(col10^2) + eff10 + I(eff10^2),family = poisson)  
step(model)

model = glm(newCol ~  + eff + I(eff^2) + hs + hsMean +  eff1 + I(eff1^2) +  eff2 + I(eff2^2)  + eff3 + I(eff3^2)  + eff5 + I(eff5^2) + eff10 + I(eff10^2),family = poisson)  
step(model)

model = glm(formula = newCol ~ col + I(col^2) + hs + hsMean + col1 + I(col1^2) + col2 + I(col2^2) + col3 + I(col3^2) + col5 + I(col5^2) + col10 + I(col10^2), family=poisson) 
step(model)

cat("========================================\n")

model = glm(formula = newCol ~ col + I((col + col1)^2) + hs + hsMean + col1 + col2 + I(col2^2) +  col3 + I(col3^2) + col5 + I(col5^2) + col10 + I(col10^2), family=poisson) 
step(model)


cat("========================================\n")

model = glm(formula = newCol ~ col + I((col + col1 + col2)^2) + hs + hsMean + col1 + col2 + col3 + I(col3^2) + col5 + I(col5^2) + col10 + I(col10^2), family=poisson) 
step(model)

cat("========================================\n")
model = glm(formula = newCol ~ col + I((col + col1 + col2 + col3)^2) +      hs + hsMean + col1 + col2 + col3 + col5 + I(col5^2) + I(col10^2),      family = poisson)
step(model)

cat("========================================\n")

model = glm(formula = newCol ~ col + I((col + col1 + col2 + col3 + col5)^2) +      hs + hsMean + col1 + col2 + col3 + col5 + I(col10^2),      family = poisson)
step(model)

cat("========================================\n")
model = glm(formula = newCol ~ col + I((col + col1 + col2 + col3 + col5 + col10)^2) +      hs + hsMean + col1 + col2 + col3 + col5 ,      family = poisson)
step(model)

cat("========================================\n")
model = glm(formula = newCol ~ col + I((col + col1 + col2 + col3 + col5)^2) + col*hs + hs + hsMean + col1 + col2 + col3 + col5 + I(col10^2), family = poisson)
step(model)

cat("========================================\n")



model = glm(newCol ~ col  +  hs + hsMean + col1 + col2 + col3 + col5 + col10 + colTotal   ,  family=poisson)



fit <- rpart(model)
fit2 <- rpart(newCol ~ col  +  hs + hsMean + col1 + col2 + col3 + col5 + col10 + colTotal   ,  method="poisson")#parms=list(prior=c(.65,.35), split='information'))
fit3 <- rpart(model,control=rpart.control(cp=.05))
par(mfrow=c(1,2), xpd=NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit,cex=0.6, use.n=TRUE)
plot(fit2)
text(fit2,cex=0.6, use.n=TRUE)
summary(fit)



Ensoleillement = c(rep("soleil",5),rep("couvert",4),rep("pluie",5))
Temperature = c(75,80,85,72,69,72,83,64,81,71,65,75,68,70)
Humidite = c(70,90,85,95,70,90,78,65,75,80,70,80,80,96)
Vent = c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE)
Jouer = c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE)

numeros = 1:14

donne = data.frame(numeros,Ensoleillement,Temperature,Humidite,Vent,Jouer)

length(Ensoleillement)
length(Temperature)
length(Humidite)
length(Vent)
length(Jouer)
length(numeros)

fit.joue = rpart(Humidite~Ensoleillement+Temperature+Jouer+Vent,method = "anova")
fit.joue

summary(fit.joue)
plot(fit.joue)
text(fit.joue,cex=0.7,use.n = TRUE)



a <-  "25-32;32-40"


for (i in 1:length(A)){
  tabRep <- matrix(as.numeric(strsplit(A,";")[[i]]),length(strsplit(A,";")[[i]])/2,2)
  cumul <- nrow(tabRep)
  start <- tabRep[1,]
  tabRep <- unique(tabRep)

  nbvisit <- nrow(tabRep)

  vecDist <- sqrt((tabRep[,1]-start[1])^2+
                  (tabRep[,2]-start[2])^2)

  distMax <- max(vecDist)

  classeDist <- c(10,20,50,100,200,300,400,500,1000)

  for(d in 1:length(classDist)){
    dd <- classDist[d]
    if(d == 1){
       vecDistClass <-  lenght(vecdist[vecDist<= dd])
     }
    else {
      vecDistClass <-  c(vecDistClass,lenght(vecdist[vecDist<= dd])-vecDistClass[d-1])
    }
    
  }
  
  sortieRep <- c(cumul,nbvisit,distMax,vecDistClass)

  if(i == 1){
    sortie <- sortieRep
  }
  else {
    sortie <- rbind(sortie,sortieRep)
  }

}


