# analyse des sorties du modele


repertoire= "D:/Recherche/Stage/"
fichier = paste(repertoire,"data.txt",sep="")
d = read.table(fichier,header=TRUE)

attach(d)

# ?glm
# ATTENTION FAMILY
model = glm(Score~TAUX_UTILE + TAUX_CONSOMMABLE + TAUX_CONSOMMABLE_C + AIRE_VISUELLE + MULT_NAISS + MULT_NAISS_PLAN + Nombre + Param_Naissance + Param_Mort + Seuil_Naissance + Masse_min + Nombre.1 + Capacite_chasse + Param_Naissance.1 + Param_Mort.1 + Seuil_Naissance.1 + Masse_min.1 + Vitesse + Perte + DetVsStoch + Nombre.2 + Capacite_chasse.1 + Param_Naissance.2 + Param_Mort.2 + Seuil_Naissance.2 + Masse_min.2 + Vitesse.1 + Perte.1 + DetVsStoch.1, family = poisson)

summary(model)


model2 = step(model)

summary(model2)


d2  = d[which(d$Score >= 5),]

model1a = glm(Score~AIRE_VISUELLE + log(Capacite_chasse) + Vitesse + DetVsStoch + log(Capacite_chasse.1) + Vitesse.1 + Vitesse*Vitesse.1 + DetVsStoch.1*Vitesse.1+ DetVsStoch.1)

 model3 = glm(Score~TAUX_CONSOMMABLE*Perte*Capacite_chasse +TAUX_CONSOMMABLE_C*Perte.1*Capacite_chasse.1 )

 
 
 library(rpart)
 
 part =  rpart(Score~TAUX_UTILE + TAUX_CONSOMMABLE + TAUX_CONSOMMABLE_C + AIRE_VISUELLE + MULT_NAISS + MULT_NAISS_PLAN + Nombre + Param_Naissance + Param_Mort + Seuil_Naissance + Masse_min + Nombre.1 + Capacite_chasse + Param_Naissance.1 + Param_Mort.1 + Seuil_Naissance.1 + Masse_min.1 + Vitesse + Perte + DetVsStoch + Nombre.2 + Capacite_chasse.1 + Param_Naissance.2 + Param_Mort.2 + Seuil_Naissance.2 + Masse_min.2 + Vitesse.1 + Perte.1 + DetVsStoch.1,method = "exp")



plot(part)
text(part,cex=0.6, use.n=TRUE)


detach(d)











































































































