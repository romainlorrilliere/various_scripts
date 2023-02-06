#!/usr/bin/env Rscript

# Chargement des packages
require(arm)
require(ggplot2)
require(glmmTMB) ## [RL] pour modele un peu plus robuste
require(DHARMa)  ## [RL] pour fonction de teste de la qualitée d'ajustement de ton modele
require(data.table) ## [RL] dsl c'est une library un peu complique mais qui me premet de bcp plus rapidement ajuster le jeux de données

# Seuil d'occurence
seuilOccu <- 14

# Chargement des données du fichier
donneesBrutes <- fread(file="./data/v_analyse.csv",sep=",")
names(donneesBrutes)[1] <- "site"

donneesBrutes[ ,site := as.character(site)] #[RL] pour être sûr que les sites sont bien considéré comme des facteur

donneesBrutes[,annee_txt := as.character(annee)] #[RL] une variable en txt des annees pour les modèles facteurs
donneesBrutes[,passage_txt := as.character(passage)] #[RL] une variable en txt pour les passages pour tester si c'est plus performant



## [RL] il ne faut pas ajouter des 0 pour les sites qui n'ont pas été visités car la relation ne peux pas être linaire sur le nombre de visite si zéro visite 0 oiseaux hors l'ajustement d'un modèle ne peux pas retrouver ça
donneesBrutes <- donneesBrutes[passage >0,]

# Variable d'export
dateJour <- format(Sys.time(),"%Y%m%d-%H%M")

# Fonction d'analyse
analyse <- function(donneesBrutes) {

    # Seuil d'abondance
    seuilAbond <- NA

    # Seuil de significativité
    seuilSignif <- 0.05

    # Titres des facettes du graphique triple
    vpan <- c("Variations abondance","Occurrences","Abondances brutes")

    # Liste des années présentes dans le fichier à analyser
    annees <- sort(unique(donneesBrutes$annee))

    # Nombre d'années dans le fichier à analyser
    nbans <- length(annees)

    # Pas de temps (?)
    pasdetemps <- nbans-1

    # Première année de la période
    anneeDebut <- min(annees)

    # Dernière année de la période
    anneeFin <- max(annees)

    flush.console() ##[RL] cette ligne qui ne sert qu'à rafraichir l'affichage dans la console ne sert à rien ici tu peux l'enlever

    # Nombre de carrés suivis par année
    nb_carre <- tapply(ifelse(donneesBrutes$passage>0,1,0),donneesBrutes$annee,sum)

    # Nombre de carrés suivis par année (avec présence de pitchou)
    nb_carre_presence <- tapply(ifelse(donneesBrutes$abond>0,1,0),donneesBrutes$annee,sum)

    # Abondance brute par année
    abond <- tapply(donneesBrutes$abond,donneesBrutes$annee,sum)

    # GLM Tendances inter-annuelles

        # Définition de la formule
        formule1 <- as.formula("abond ~ passage + annee_txt +  (1|site)") # [RL] meilleur modele trouvé

        # Exécution et stockage de la formule
        glm1 <- glmmTMB(formule1,data=donneesBrutes,family="poisson")
    # Résumé des résultats de la formule
        sglm1 <- summary(glm1)
    print(sglm1)
    simulationOutput <- simulateResiduals(fittedModel = glm1, plot = F)
    testZeroInflation(simulationOutput)
    plot(simulationOutput) # [RL] c'est OK


        # Stocke les n (='nbans') dernières valeurs renvoyées la fonction 'glm1' (valeurs par années)
    coefan <- setDT(as.data.frame(tail(sglm1$coefficients$cond,pasdetemps)))


        # Vecteur des variations d'abondance par année 'back transformée' (?) [RL] oui car ce sont des valeur log
        # Concatène la valeur 1 (1e année de suivi) en tête de la variable 'coefan' et calcule l'exponentielle des valeurs
        coefannee <- c(1,exp(coefan[[1]]))

        erreuran <- coefan[[2]]

        # Vecteur des erreurs standards 'back transformées' (?)

        erreurannee1 <- c(0,erreuran)*coefannee

        # Intervalle de confiance à 95%
        pval <- c(1,coefan[[4]])

        # Calcul des intervalles de confiance
    glm1.sim <- as.data.frame(tail(confint(glm1),pasdetemps+1))
    glm1.sim <- glm1.sim[-nrow(glm1.sim),]
    setDT(glm1.sim)
            ic_inf_sim <- c(1,exp(glm1.sim[[1]]))
            ic_sup_sim <- c(1,exp(glm1.sim[[2]]))

		# Table pour la réalisation du graphique
        # Concaténation des vecteurs listés en paramètres
    tab1 <- data.frame(
        annees,
        val=coefannee,
        LL=ic_inf_sim,
        UL=ic_sup_sim,
        catPoint=ifelse(pval<seuilSignif,"significatif",NA),
        pval,
        courbe=vpan[1],
        panel=vpan[1])

        # Nettoyage des intervalles de confiance supérieurs très grands
        tab1$UL <- ifelse(nb_carre_presence==0,NA,tab1$UL)
        tab1$UL <- ifelse(tab1$UL==Inf,NA,tab1$UL)
        tab1$UL <- ifelse(tab1$UL>1.000000e+20,NA,tab1$UL)
        tab1$UL[1] <- 1
        tab1$val <-  ifelse(tab1$val>1.000000e+20,1.000000e+20,tab1$val)

        # Indice de surdispersion
        dispAn <- sigma(glm1)

        # Table de sauvegarde des résultats
        tabVariations <- data.frame(
            annee = tab1$annee,
            abondance_relative = round(tab1$val,3),
            IC_inferieur = round(tab1$LL,3),
            IC_superieur = round(tab1$UL,3),
            erreur_standard = round(erreurannee1,4),
            p_value = round(tab1$pval,3),
            significatif = !is.na(tab1$catPoint),
            nb_carre,
            nb_carre_presence,
            abondance = abond)


    # GLM Tendance générale sur la période

        # Définition de la formule
        formule2 <- as.formula(paste("abond ~ passage + annee +  (1|site)",sep=""))

     glm2 <- glmmTMB(formule2,data=donneesBrutes,family="poisson")
    # Résumé des résultats de la formule
        sglm2 <- summary(glm2)
    print(sglm2)
    simulationOutput <- simulateResiduals(fittedModel = glm2, plot = F)
    testZeroInflation(simulationOutput)
    plot(simulationOutput) # [RL] c'est OK


                                        # Tendance sur la période
    coef <- setDT(as.data.frame(sglm2$coefficients$cond))

        coefannee <- tail(coef[[1]],1)
        trend <- round(exp(coefannee),3)

        # Pourcentage de variation sur la période
        pourcentage <- round((exp(coefannee*pasdetemps)-1)*100,2)
        pval <- tail(coef[[4]],1)
        erreuran <- tail(coef[[2]],1)

        # Erreur standard
        erreurannee2 <- erreuran*exp(coefannee)

        # Intervalle de confiance à 95%

        # Calcul des intervalles de confiance
    glm2.sim <- as.data.frame(confint(glm2))
    glm2.sim <- glm2.sim[nrow(glm2.sim)-1,]
    setDT(glm2.sim)
        LL <- round(exp(glm2.sim[[1]]),3)
        UL <- round(exp(glm2.sim[[2]]),3)

        # Table pour la réalisation des figures
        tab1t <- data.frame(
                    Est=trend,
                    LL,
                    UL,
                    pourcent = pourcentage,
                    signif = pval<seuilSignif,
                    pval)

        trendsignif <- tab1t$signif

        pourcent <- round((exp(coefannee*pasdetemps)-1)*100,3)

        # Indice de surdispersion
        dispTrend <- sigma(glm2)

        # Classement en catégorie 'incertain'
        if(dispTrend > 2 | dispAn > 2 | median(nb_carre_presence)<seuilOccu) catIncert <- "Incertain" else catIncert <- "Bon"

        vecLib <-  NULL

        if(dispTrend > 2 | dispAn > 2 | median(nb_carre_presence)<seuilOccu) {
            if(median(nb_carre_presence)<seuilOccu) {
                vecLib <- c(vecLib,"Espèce trop rare")
            }
            if(dispTrend > 2 | dispAn > 2) {
                vecLib <- c(vecLib,"Déviance")
            }
        }

        raisonIncert <-  paste(vecLib,collapse=" et ")

  catEBCC <- NA
    catEBCC <- affectCatEBCC(trend =trend,pVal = pval,ICinf= LL,ICsup= UL)

        # Table complète de résultats
        tabTendance <- data.frame(
                        nombre_annees = pasdetemps,
                        premiere_annee = anneeDebut,
                        derniere_annee = anneeFin,
                        tendance = as.vector(trend),
                        IC_inferieur = as.vector(LL),
                        IC_superieur = as.vector(UL),
                        pourcentage_variation = as.vector(pourcent),
                        erreur_standard = as.vector(round(erreurannee2,4)),
                        p_value = round(pval,3),
                        significatif = trendsignif,categorie_tendance_EBCC= catEBCC,
                        mediane_occurrence = median(nb_carre_presence),
                        valide = catIncert,
                        raison_incertitude = raisonIncert)


    # Résultats de la fonction
    return(list(tabVariations,tabTendance))

}




    ## renvoie la categorie EBCC de la tendance en fonction
    ## trend l'estimateur de la tendance
    ## pVal la p value
    ## ICinf ICsup l intervalle de confiance a 95 pourcent
    affectCatEBCC <- function(trend,pVal,ICinf,ICsup){

        catEBCC <- ifelse(pVal>0.05,
                   ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
                   ifelse(trend<1,
                   ifelse(ICsup<0.95,"Fort déclin","Déclin modéré"),
                   ifelse(ICinf>1.05,"Forte augmentation","Augmentation modérée")))
        return(catEBCC)
    }



# Appel de la fonction d'analyse (qui inclut la fonction de création des graphiques) et stockage des résultats
results <- analyse(donneesBrutes)

# Tableau des variations interannuelles
tabVariations <- results[1]

# Tableau de tendance sur la période
tabTendance <- results[2]

# Export des tableaux de résultats
write.csv2(tabVariations,file=paste("./output/",dateJour,"-variations_2.csv",sep=""),row.names=FALSE)
write.csv2(tabTendance,file=paste("./output/",dateJour,"-tendance_2.csv",sep=""),row.names=FALSE,fileEncoding="LATIN1")

