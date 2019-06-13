####################################################
#   GLM STOC
####################################################


library(ade4)
library(mgcv)
library(spdep)
library(MASS)
library(nlme)


repertoire= "D:/Recherche/R/Greg/"

fichier = paste(repertoire,"r_obs_resid.txt",sep="")
obs = read.table(fichier,header=T)

memory.limit(size=4095)

obs$var=obs$avg.residualscarre.residuals.
#obsB=subset(obs,((anthro!="0"),(bois!="0"),(eau!="0"),(gdes_cult!="0"),(rural!="0"),(fragmentat!="0"))

names(obs)

especes = paste(unique(obs$LB_NOM))

for (sp in especes) 
{
#browser()
obsSp= obs[which(paste(obs[,1])==sp),]
mod=glm(obsSp$var~obsSp$anthro+obsSp$bois+obsSp$eau+obsSp$gdes_cult+obsSp$rural+obsSp$fragmentat)
print(sp)
print(summary(mod))
flush.console()
}


