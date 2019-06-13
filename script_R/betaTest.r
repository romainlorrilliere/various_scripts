#####################################
# Test de la fonction beta de FPC
#####################################

repertoire <-"/home/romain/1_Recherche/Model/community/"
fichier <-  paste(repertoire,"betaTest.txt",sep="")
tab <- read.delim(fichier,sep=" ")
vec <-  tab[,1]
hist(vec)
print(summary(vec))
print(sd(vec))
