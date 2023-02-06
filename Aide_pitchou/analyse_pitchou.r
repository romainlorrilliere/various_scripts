#!/usr/bin/env Rscript

# Chargement des packages de base
require(arm)
require(ggplot2)

# Chargement de packages additionnels (Romain L.)
require(glmmTMB) # Pour modèle un peu plus robuste
#require(DHARMa)  # Pour la fonction de test de la qualité d'ajustement du modèle (cf. lignes 99-103 et 174-178)
require(data.table) # Bibliothèque un peu compliquée, mais qui permet d'ajuster beaucoup plus rapidement le jeu de données

# Seuil d'occurence
seuilOccu <- 14

# Chargement des données du fichier
# donneesBrutes <- fread(file="./data/v_analyse.csv",sep=",")
donneesBrutes <- fread(file="./data/v_analyse_obs.csv",sep=",")

# Convertit la colonne 'site' (points d'écoute) en chaîne de caractères
donneesBrutes[,site := as.character(site)]

# Créé une nouvelle colonne 'annee_txt' (colonne 'annee' castée en chaîne de caractères pour les modèles facteurs)
donneesBrutes[,annee_txt := as.character(annee)]

# Créé une nouvelle colonne 'passage_txt' (colonne 'passage' castée en chaîne de caractères pour les modèles facteurs)
donneesBrutes[,passage_txt := as.character(passage)]

# Variable d'export
dateJour <- format(Sys.time(),"%Y%m%d-%H%M")

# Fonction de calcul de la catégorie EBCC de la tendance en fonction de :
#   - 'trend' = estimateur de la tendance
#   - 'pVal' = p-value
#   - 'ICinf' et 'ICsup' = intervalle de confiance à 95%
affectCatEBCC <- function(trend,pVal,ICinf,ICsup){
    catEBCC <-  ifelse(pVal>0.05,
                    ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
                        ifelse(trend<1,
                            ifelse(ICsup<0.95,"Fort déclin","Déclin modéré"),
                                ifelse(ICinf>1.05,"Forte augmentation","Augmentation modérée")
                    )
                )
    return(catEBCC)
}

# Fonction de création des graphiques
graphiques <- function(dgg, tab1t, vpan) {

    # Coordonnées des lignes horizontales (seuils) pour les abondances et les occurrences
    hline.data1 <- data.frame(z=c(1), panel=c(vpan[1]))
    hline.data2 <- data.frame(z=c(0,seuilOccu), panel=c(vpan[2],vpan[2]))
    hline.data3 <- data.frame(z=0, panel=vpan[3])

    # Titre général des graphiques
    titre <- "Fauvette pitchou"

    # Extraction des subsets du tableau de synthèse 'dgg'
    dggVar  <- subset(dgg,panel=="Variations abondance")
    dggOccu <- subset(dgg,panel=="Occurrences")
    dggAbon <- subset(dgg, panel=="Abondances brutes")

    # Calcul du pas de temps (nb d'années de suivi)
    # Conflit avec 'pasdetemps' définit dans 'analyse()'
    # Nb. d'années de suivi = 'nbans' déjà définie (14)
    # pasdetemps <- max(dgg$annee) - min(dgg$annee) + 1
    fig.nbans <- max(dgg$annee) - min(dgg$annee) + 1

    # Texte
    txtPente1 <- paste(
                    tab1t$Est,
                    ifelse(tab1t$signif," (1)"," (2)"),
                    " [",tab1t$LL," , ",tab1t$UL,"]",
                    ifelse(tab1t$signif, paste(ifelse(tab1t$pourcent>0," ; +"," ; - "), abs(tab1t$pourcent), "% en ", fig.nbans, " ans", sep=""),""),
                    sep="")

    # Affichage du texte
    # cat(txtPente1)

    # Table du texte de la tendance
    tabTextPent <- data.frame(
                        y=c(max(c(dggVar$val,dggVar$UL),na.rm=TRUE)*0.9),
                        x=median(dggVar$annee),
                        txt=c(txtPente1),
                        courbe=c(vpan[1]),
                        panel=c(vpan[1]))


    # Couleurs

    vecColPoint <- c("#ffffff","#eeb40f","#ee0f59")
    names(vecColPoint) <- c("Significatif","infSeuil","0")

    vecColCourbe <- c("#3c47e0","#5b754d","#55bb1d","#973ce0")
    names(vecColCourbe) <- c(vpan[1],"carre","presence",vpan[3])

    col <- c(vecColPoint,vecColCourbe)
    names(col) <- c(names(vecColPoint),names(vecColCourbe))


    # Graphique triple (3 facettes = panels)

        p.facettes <- ggplot(data=dgg, mapping=aes(x=annee, y=val))

        # Mise en forme générale des facettes

            # Composantes

            p.fac0.facettes           <- facet_grid(panel ~ ., scales="free") # 3 sous-graphiques (Variations abondances, Occurrences et Abondances brutes)
            p.fac0.ylab               <- ylab("") # Etiquette axe des ordonnées
            p.fac0.xlab               <- xlab("Année") # Etiquette axe des abcisses
            p.fac0.labs               <- labs(title=titre, subtitle=txtPente1) # Titres du graphique
            p.fac0.theme              <- theme(plot.title=element_text(face="bold", size=10), plot.subtitle=element_text(size=7.5))
            p.fac0.scalecolor         <- scale_colour_manual(values=col, name="", breaks=names(col[5:6]))
            p.fac0.scaleXcont         <- scale_x_continuous(breaks=min(dgg$annee):max(dgg$annee))
            p.fac0.axisTextSize       <- theme(axis.text=element_text(size = 6))
            p.fac0.axisTitleTextSize  <- theme(axis.title=element_text(size = 7.5, margin=margin(t=10)))
            p.fac0.legendPosition     <- theme(legend.position = c(0.15,0.4), legend.title=element_blank(), legend.text=element_text(size=7.5))
            p.fac0.legendCustom       <- scale_fill_discrete(labels=c('Point échantillonné', 'Point avec présence'))
            p.fac0.legendRow          <- guides(colour=guide_legend(nrow=1)) # Légende sur 1 ligne

        p.facettes <- p.facettes + p.fac0.facettes + p.fac0.ylab + p.fac0.xlab + p.fac0.labs + p.fac0.theme + p.fac0.scalecolor + p.fac0.scaleXcont + p.fac0.axisTextSize + p.fac0.axisTitleTextSize + p.fac0.legendPosition + p.fac0.legendCustom + p.fac0.legendRow


        # Facette 1 (variations d'abondance)

            # Composantes

            p.fac1.geom_hline    <- geom_hline(aes(yintercept=z), hline.data1, color="white", alpha=1, size=1.2) # Ligne horizontale à 1.0 (valeur témoin année 1 = 2008)
            p.fac1.ribbon        <- geom_ribbon(data=dggVar, mapping=aes(y=val,ymin=LL,ymax=UL), fill=col[vpan[1]], alpha=0.2) # Ombrage entre UL et LL
            p.fac1.line          <- geom_line(data=dggVar, mapping=aes(color=courbe), size=0.5) # Courbe
            p.fac1.point1        <- geom_point(data=dggVar, mapping=aes(color=courbe), size=0.5) # Points
            #p.fac1.point2        <- geom_point(data=dggVar, mapping=aes(color=catPoint, alpha=ifelse(!is.na(catPoint),1,0)), size=0.5) # Points blancs pour les années significatives
            #p.fac1.legend        <- scale_fill_discrete(labels=c('a','b'))

        #p.facettes <- p.facettes + p.fac1.geom_hline + p.fac1.ribbon + p.fac1.line + p.fac1.point1 + p.fac1.point2 + p.fac1.legend
        p.facettes <- p.facettes + p.fac1.geom_hline + p.fac1.ribbon + p.fac1.line + p.fac1.point1


        # Facette 2 (occurences)

            #tab2 <- subset(dgg,panel=="Occurrences")

            p.fac2.geom_hline   <- geom_hline(aes(yintercept=z), hline.data2, color="red", alpha=0.5, size=0.5) # Ligne de seuil à 14
            p.fac2.line         <- geom_line(data=dggOccu, mapping=aes(color=courbe), size=0.5, alpha=0.5) # Vert foncé : nb. points suivis ; vert clair : % points où l'espèce a été détectée
            p.fac2.point1       <- geom_point(data=dggOccu, mapping=aes(color=courbe), size=0.5, alpha=0.5)
            #p.fac2.point2       <- geom_point(data=dggOccu, mapping=aes(colour=catPoint, alpha=ifelse(!is.na(catPoint),1,0)), size=2)

        p.facettes <- p.facettes + p.fac2.geom_hline + p.fac2.line + p.fac2.point1


        # Facette 3 (abondances)

            #tab3 <- subset(dgg, panel=="Abondances brutes")

            p.fac3.geom_hline       <- geom_hline(aes(yintercept = z), hline.data3, color="red", alpha=0.5, size=0.5, linetype="dashed")
            p.fac3.line             <- geom_line(data=dggAbon, mapping=aes(color=courbe), size=0.5)
            p.fac3.point1           <- geom_point(data=dggAbon, mapping=aes(color=courbe), size=0.5)
            #p.fac3.point2           <- geom_point(data=dggAbon, mapping=aes(colour=catPoint, alpha=ifelse(!is.na(catPoint),1,0)), size=2)

        p.facettes <- p.facettes + p.fac3.geom_hline + p.fac3.line + p.fac3.point1

        # Nom du fichier à créer
        p.facettes.filename <- paste(dateJour, "facettes.png", sep="-")

        # Création du fichier dans le dossier 'output/img'
        ggsave(filename=file.path("./output/img", p.facettes.filename), p.facettes, width=15, height=20, units="cm")


        # Graphique de tendance sur la période

        p.trend <- ggplot(data=dggVar, mapping=aes(x=annee, y=val), fill=catPoint, colour=catPoint)

            # Composantes

                # Titre axe X
                p.trend.xlab <- xlab("Année")

                # Titre axe Y
                p.trend.ylab <- ylab("")

                # Graduations axe X
                p.trend.scaleXcont <- scale_x_continuous(breaks=min(dgg$annee):max(dgg$annee))

                # Taille de police (étiquettes des axes)
                p.trend.axisTextSize <- theme(axis.text=element_text(size=8))

                # Titre et sous-titre du graphique
                p.trend.labs <- labs(title=titre, subtitle=txtPente1)

                # Légende
                p.trend.legend <- theme(
                    plot.title=element_text(face="bold", size=10),
                    plot.subtitle=element_text(size=7.5),
                    panel.grid.minor=element_blank(),
                    panel.grid.major=element_line(color='grey', size=0.3),
                    panel.background=element_rect(fill='white', color='black'),
                    legend.position=c(0.2, 0.85),
                    legend.title=element_blank(),
                    legend.text=element_text(size=9),
                    legend.key=element_rect(fill='grey', colour='grey'),
                    legend.box.background=element_rect(fill='grey', colour='black')
                )

                # Légende (couleur des points)
                p.trend.legendPointColors <- list(scale_color_manual(values=c('black','white')), scale_fill_manual(values=c('black','white')))

                # Légende (couleur des contours des points)
                p.trend.legendPointStrokeColors <- scale_color_manual(values=c('black','white'))

                # Légende (couleur du remplissage des points)
                p.trend.legendPointFillColors <- scale_fill_manual(values=c('black','white'))

                # Légende sur une ligne
                p.trend.scaleOneLine <- guides(color=guide_legend(nrow=1))

            p.trend <- p.trend + p.trend.xlab + p.trend.ylab + p.trend.scaleXcont + p.trend.axisTextSize + p.trend.labs + p.trend.legend + p.trend.legendPointColors + p.trend.scaleOneLine

            # Graphique

                # Buffer mini-maxi
                p.trend.ribbon <- geom_ribbon(data=dggVar, mapping=aes(y=val, ymin=LL, ymax=UL), color=NA, alpha=0.2)

                # Ligne
                p.trend.line <- geom_line(data=dggVar, stat="identity", size=0.4, color='black')

                # Marqueurs (points)
                p.trend.point <- geom_point(data=dggVar, mapping=aes(color=ifelse(is.na(catPoint),'Non-significatif','Significatif')), size=1.5)

                # Droite de régression (se = buffer ; ici non tracé)
                p.trend.stat_smooth <- stat_smooth(method="lm", formula=y~x, geom="smooth", se=FALSE, size=0.4, linetype='dotted', color='red')

            p.trend <- p.trend + p.trend.ribbon + p.trend.line + p.trend.point + p.trend.stat_smooth

        p.trend

        # Nom du fichier à créer
        p.trend.filename <- paste(dateJour, "tendance.png", sep="-")

        # Création du fichier dans le dossier 'output/img'
        ggsave(filename=file.path("./output/img", p.trend.filename), p.trend, width=14, height=8, units="cm")
}

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

    # Nombre de carrés suivis par année
    nb_carre <- tapply(ifelse(donneesBrutes$passage>0,1,0),donneesBrutes$annee,sum)

    # Nombre de carrés suivis par année (avec présence de pitchou)
    nb_carre_presence <- tapply(ifelse(donneesBrutes$abond>0,1,0),donneesBrutes$annee,sum)

    # Tableau pour le nb. total de carrés suivis et le nb. de carrés avec présence
    tab2 <- data.frame(
        annee=rep(annees,2),
        val=c(nb_carre,nb_carre_presence),
        LL=NA,
        UL=NA,
        catPoint=NA,
        pval=NA,
        courbe=rep(c("carre","presence"),each=length(annees)),   # stocke 'carre' si nb. total carrés et 'presence' si nb. carrés avec présence
        panel=vpan[2])

    # Remplissage colonne 'catPoint' en fonction du seuil d'occurence
    tab2$catPoint <- ifelse(tab2$val == 0,"0",ifelse(tab2$val < seuilOccu,"infSeuil",NA))

    # Abondance brute par année
    abond <- tapply(donneesBrutes$abond,donneesBrutes$annee,sum)

    # Tableau pour les abondances brutes
    tab3 <- data.frame(
        annee=annees,
        val=abond,
        LL=NA,
        UL=NA,
        catPoint=NA,
        pval=NA,
        courbe=vpan[3],
        panel=vpan[3])

    # Remplissage colonne 'catPoint' en fonction du seuil d'abondance
    tab3$catPoint <- ifelse(tab3$val == 0,"0",ifelse(tab3$val < seuilAbond,"infSeuil",NA))

    # GLM Tendances inter-annuelles

        # Définition de la formule avec meilleur modèle trouvé (Romain L.)
        # formule1 <- as.formula("abond ~ passage + annee_txt + (1|site)")
        formule1 <- as.formula("abond ~ passage + annee_txt + (1|site) + (1|observateur)")

        # Exécution et stockage de la formule
        glm1 <- glmmTMB(formule1,data=donneesBrutes,family="poisson")

        # Résumé des résultats de la formule
        sglm1 <- summary(glm1)

        # Ajout de tests de modèle (Romain L.)
        # print(sglm1)
        #simulationOutput <- simulateResiduals(fittedModel=glm1, plot=F)
        #testZeroInflation(simulationOutput)
        #plot(simulationOutput) # Romain L. : c'est OK

        # Stocke les n (='nbans') dernières valeurs renvoyées la fonction 'glm1' (valeurs par années)
        coefan <- setDT(as.data.frame(tail(sglm1$coefficients$cond,pasdetemps)))

        # Vecteur des variations d'abondance par année 'back transformée' (car ce sont des valeurs log)
        # Concatène la valeur 1 (1e année de suivi) en tête de la variable 'coefan' et calcule l'exponentielle des valeurs
        coefannee <- c(1,exp(coefan[[1]]))

        erreuran <- coefan[[2]]

        # Vecteur des erreurs standards 'back transformées' (?)
        erreurannee1 <- c(0,erreuran)*coefannee

        # Intervalle de confiance à 95%
        pval <- c(1,coefan[[4]])

        # Calcul des intervalles de confiance
        glm1.sim <- as.data.frame(confint(glm1))
        glm1.sim <- glm1.sim[sort(grep("annee",row.names(glm1.sim))),]
        setDT(glm1.sim)
        ic_inf_sim <- c(1,exp(glm1.sim[[1]]))
        ic_sup_sim <- c(1,exp(glm1.sim[[2]]))

		# Table pour la réalisation du graphique
        # Concaténation des vecteurs listés en paramètres
        tab1 <- data.frame(
            annee=annees,
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

        # Table de sauvegarde des résultats (variations interannuelles)
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
        # formule2 <- as.formula(paste("abond ~ passage + annee + (1|site)", sep=""))
        formule2 <- as.formula(paste("abond ~ passage + annee + (1|site) + (1|observateur)", sep=""))

        # Exécution et stockage de la formule (Warning message !)
        glm2 <- glmmTMB(formule2,data=donneesBrutes,family="poisson")

        # Résumé des résultats de la formule
        sglm2 <- summary(glm2)

        # Ajout de tests de modèle (Romain L.)
        # print(sglm2)
        #simulationOutput <- simulateResiduals(fittedModel = glm2, plot = F)
        #testZeroInflation(simulationOutput)
        #plot(simulationOutput) # Romain L. : c'est OK

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

        # Calcul des intervalles de confiance
        glm2.sim <- as.data.frame(confint(glm2))
        glm2.sim <- glm2.sim[grep("annee",row.names(glm2.sim)),]
        setDT(glm2.sim)
    ll <- glm2.sim[[1]]
    LL <- round(exp(ll),3)
    ul <- glm2.sim[[2]]
        UL <- round(exp(ul),3)

    pourcent_ll <- round((exp(ll*pasdetemps)-1)*100,2)
pourcent_ul <- round((exp(ul*pasdetemps)-1)*100,2)

        # Table pour la réalisation des figures
        tab1t <- data.frame(
                    Est=trend,
                    LL,
                    UL,
            pourcent = pourcentage,
            pourcent_inf = pourcent_ll,
            pourcent_sup = pourcent_ul,
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
        catEBCC <- affectCatEBCC(trend=trend,pVal=pval,ICinf=LL,ICsup=UL)

        # Table de sauvegarde des résultats (tendance globale)
        tabTendance <- data.frame(
                        nombre_annees = nbans,
                        premiere_annee = anneeDebut,
                        derniere_annee = anneeFin,
                        tendance = as.vector(trend),
                        IC_inferieur = as.vector(LL),
                        IC_superieur = as.vector(UL),
            pourcentage_variation = as.vector(pourcent),
            pourcentage_variation_inferieur = pourcent_ll,
            pourcentage_variation_superieur = pourcent_ul,
                        erreur_standard = as.vector(round(erreurannee2,4)),
                        p_value = round(pval,3),
                        significatif = trendsignif,
                        categorie_tendance_EBCC= catEBCC,
                        mediane_occurrence = median(nb_carre_presence),
                        valide = catIncert,
                        raison_incertitude = raisonIncert)

        # Réalisation des graphiques

        # dgg <- rbind(tab1,tab2,tab3)
        # graphiques(dgg, tab1t, vpan)

    # Résultats de la fonction
    return(list(tabVariations,tabTendance))

}

# Appel de la fonction d'analyse (qui inclut la fonction de création des graphiques) et stockage des résultats
results <- analyse(donneesBrutes)

# Tableau des variations interannuelles
tabVariations <- results[1]

# Tableau de tendance sur la période
tabTendance <- results[2]

# Export des tableaux de résultats
write.csv2(tabVariations,file=paste("./output/",dateJour,"-variations_obs.csv",sep=""),row.names=FALSE)
write.csv2(tabTendance,file=paste("./output/",dateJour,"-tendance_obs.csv",sep=""),row.names=FALSE,fileEncoding="LATIN1")
