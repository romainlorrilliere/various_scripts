#!/usr/bin/env Rscript

# Chargement des packages
require(arm)
require(glmmTMB)
#require(DHARMa)
require(data.table)

# Chargement des données du fichier
donneesBrutesMeteo <- fread(file="./data/v_analyse_meteo.csv", sep=",")

# Convertit la colonne 'site' (points d'écoute) en chaîne de caractères
donneesBrutesMeteo[,site := as.character(site)]

# Formule d'analyse suivant températures mini de l'hiver précédent
formule_tmini <- as.formula("abond ~ passage + tmini + (1|site) + (1|observateur)")

# Exécution et stockage de la formule
glm_tmini <- glmmTMB(formule_tmini,data=donneesBrutesMeteo,family="poisson")

# Résumé des résultats de la formule
sglm_tmini <- summary(glm_tmini)

# Affichage du résumé du modèle
print(sglm_tmini)

library(ggeffects)

var  <-  paste0("tmini [",paste0(seq(-14,-2,0.1),collapse = ","),"]")
var

ggpred <- ggpredict(glm_tmini,terms = var)
print(ggpred)
plot(ggpred)
