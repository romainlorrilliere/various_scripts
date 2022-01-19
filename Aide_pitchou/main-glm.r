### Fonction générale de calcul de la variation temporelle et de la tendance
### La fonction génère aussi les graphiques

main.glm <- function(
                id,                             # ID de la session de calcul (par défaut = date et heure du traitement)
                donneesAll,                     # Fichier de données formaté au début du programme
                listSp=NULL,                    # Liste d'espèces à analyser (par défault, toutes les espèces du fichier de données sont traitées)
                annees=NULL,
                echantillon=1,
                methodeEchantillon=NULL,
                figure=TRUE,                    # Active la réalisation des graphiques
                description=TRUE,
                tendanceSurFigure=TRUE,
                tendanceGroupSpe = FALSE,
                seuilOccu=14,                   # Seuil fixé arbitrairement [MNHN?]
                seuilAbond=NA) {                # Seuil non-fixé ?! [MNHN?]
    
    require(arm)
    require(ggplot2)
	
	# Seuil de significativité
	seuilSignif <- 0.05
	
	# Charge la table de référence des espèces
	tabsp <- read.csv2("Librairie/espece.csv")
	
	# Créé une 1e colonne avec en ligne le contenu de la colonne 'sp' (code espèce)
    rownames(tabsp) <- tabsp$sp
    
    # Vecteur des panels de la figure
    vpan <- c("Variation abondance")
    if(description) vpan <- c(vpan,"Occurrences","Abondances brutes")
    
    # Quelle utilité ? Variable 'nomfile1' reprise nulle part...
    # nomfile1 <- paste("Donnees/carre2001-2014REGION_",id,".csv",sep="")

	# Liste des années présentes dans le fichier à analyser
    annee <- sort(unique(donneesAll$annee))
    
    # Nombre d'années dans le fichier à analyser
    nbans <- length(annee)
    
    # Pas de temps (?)
    pasdetemps <- nbans-1
    
    # Année la plus ancienne
    firstY <- min(annee)
    
    # Année la plus récente
    lastY <- max(annee)

	# Si aucune(s) espèce(s) n'est passée en paramètre --> analyse de toutes les espèces
    if (is.null(listSp)) {
        # Liste des espèces de la table de référence
        spValide <- as.vector(tabsp$sp)
        # Sélection des noms d'espèces du fichier en entrée qui correspondent aux noms d'espèces de la table de référence
        listSp <- colnames(donneesAll)[colnames(donneesAll)%in%spValide]
        # Filtre les lignes de la table de référence des espèces qui sont représentées dans le fichier à analyser
        tabsp2 <- tabsp[listSp,]
        # Liste des espèces à analyser (espèces indicatrices uniquement et triées par ordre alphabétique)
        listSp <- listSp[order(as.numeric(tabsp2$shortlist)*-1,tabsp2$sp)]
    }
    
	# Analyse par espèce
	i <- 0
	nbSp <- length(listSp)
	#browser()
	
    for (sp in listSp) {
    
		i <- i + 1
        
        # Filtre des données pour l'espèce en cours (sp)
        # d <- data.frame(abond=donneesAll[,sp],annee=donneesAll$annee,carre=donneesAll$carre)
        d <- data.frame(abond=donneesAll[,sp],annee=donneesAll$annee,carre=donneesAll$carre,passage=donneesAll$passage) # [SYLUND]
		
		# Sélection du nom de l'espèce
        nomSp <- as.character(tabsp[sp,"nom"])
        
        # Affichage de la ligne de l'espèce en cours (rang + nom d'espèce)
		cat("\n(",i,"/",nbSp,") ",sp," | ", nomSp,"\n",sep="")
		
		flush.console()
		
		# Stocke TRUE/FALSE selon que l'espèce fait partie des espèces indicatrices reconnues à l'échelle nationale
		shortlist <- tabsp[sp,"shortlist"]
		
		# Stocke TRUE/FALSE selon que l'espèce est incluse dans un groupe d'indicateurs (calculs par groupes d'indicateurs)
        indic <- tabsp[sp,"indicateur"]

        # Nombre total de carrés suivis par année
        # Renvoie un tableau avec années en colonnes et nombre de carrés suivis sur la 1e ligne
        # nb_carre <- tapply(rep(1,times=nrow(d)),d$annee,sum)
        nb_carre <- tapply(ifelse(d$passage>0,1,0),d$annee,sum) #[SYLUND]
        
        # Nombre de carrés suivis par année, avec présence
        # Même calcul que pour 'nb_carre' mais pour les carrés avec des individus détectés uniquement
		nb_carre_presence <- tapply(ifelse(d$abond>0,1,0),d$annee,sum)
		
        # Tableau de résultat d'analyses
        # Chaque année est répétée sur 2 lignes avec respectivement le nb. total de carrés suivis et le nb. de carrés avec présence en colonne 'val'
        tab2 <- data.frame(
                        annee=rep(annee,2),
                        val=c(nb_carre,nb_carre_presence),
                        LL=NA,
                        UL=NA,
                        catPoint=NA,
                        pval=NA,
                        courbe=rep(c("carre","presence"),each=length(annee)),   # stocke 'carre' si nb. total carrés et 'presence' si nb. carrés avec présence
                        panel=vpan[2])
        
        # Remplissage colonne 'catPoint' en fonction du seuil d'occurence défini dans main.glm()
        tab2$catPoint <- ifelse(tab2$val == 0,"0",ifelse(tab2$val < seuilOccu,"infSeuil",NA))
        
		# Abondance brute par année
        # Renvoie un tableau avec années en colonnes et abondances brutes sur la 1e ligne
        abond <- tapply(d$abond,d$annee,sum)
        
        # Tableau pour la figure
        tab3 <- data.frame(
                        annee=annee,
                        val=abond,
                        LL=NA,
                        UL=NA,
                        catPoint=NA,
                        pval=NA,
                        courbe=vpan[3],
                        panel=vpan[3])
        
        # Remplissage colonne 'catPoint' en fonction du seuil d'abondance défini dans main.glm()
        tab3$catPoint <- ifelse(tab3$val == 0,"0",ifelse(tab3$val < seuilAbond,"infSeuil",NA))

		# GLM Variation d'abondance
		# formule <- as.formula("abond~as.factor(carre)+as.factor(annee)")
		
		# Définition de la formule
		formule <- as.formula("abond~as.factor(carre)+as.factor(annee)+passage") #[SYLUND] [MNHN?] A confirmer par le MNHN...
        
        # Exécution et stockage de la formule
        glm1 <- glm(formule,data=d,family=quasipoisson)
        
        # Résumé des résultats de la formule
        sglm1 <- summary(glm1)
        
        # Stocke les n (='pasdetemps') dernières valeurs renvoyées la fonction 'glm1' (valeurs par années)
        # coefan <- tail(matrix(coefficients(glm1)),pasdetemps)
        coefan <- tail(matrix(coefficients(glm1)),nbans) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour récupérer les années 2009 à 2020 (avec 'pasdetemps' : on récupère pas 2009...)
        
        # Suppression de la dernière valeur de 'coefan' (passage)
        coefan <- head(coefan, -1) # [SYLUND] [MNHN?]
        
		# Vecteur des variations d'abondance par année 'back transformée' (?)
		# Concatène la valeur 1 (1e année de suivi) en tête de la variable 'coefan' et calcule l'exponentielle des valeurs
		coefannee <- rbind(1,exp(coefan))
        
        # erreuran <- as.data.frame(tail(matrix(summary(glm1)$coefficients[,2]),pasdetemps))
        erreuran <- as.data.frame(tail(matrix(summary(glm1)$coefficients[,2]),nbans)) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour récupérer les années 2009 à 2020 (avec 'pasdetemps' : on récupère pas 2009...)
        
        # Suppression de la dernière valeur de 'erreuran' (passage)
        erreuran <- head(erreuran, -1) # [SYLUND] [MNHN?]
        
		# Vecteur des erreurs standards 'back transformées' (?)
        erreurannee1 <- as.vector(rbind(0,erreuran*coefannee)[,1])
        
        # pval <- c(1,tail(coefficients(sglm1),pasdetemps)[,4])
        pval <- c(1,tail(coefficients(sglm1),nbans)[,4]) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour récupérer les années 2009 à 2020 (avec 'pasdetemps' : on récupère pas 2009...)
        
        # Suppression de la dernière valeur de 'pval' (passage)
        pval <- head(pval, -1) # [SYLUND] [MNHN?]
        
        # Calcul des intervalles de confiance
        glm1.sim <- sim(glm1)
        # ic_inf_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.025),pasdetemps)))
        # ic_sup_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.975),pasdetemps)))
        ic_inf_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.025), nbans))) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour récupérer les années 2009 à 2020 (avec 'pasdetemps' : on récupère pas 2009...)
        ic_sup_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.975), nbans))) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour récupérer les années 2009 à 2020 (avec 'pasdetemps' : on récupère pas 2009...)
		
		# Suppression de la dernière valeur de 'ic_inf_sim' (passage)
		ic_inf_sim <- head(ic_inf_sim, -1) # [SYLUND] [MNHN?]
		
		# Suppression de la dernière valeur de 'ic_sup_sim' (passage)
		ic_sup_sim <- head(ic_sup_sim, -1) # [SYLUND] [MNHN?]
		
		# Table pour la réalisation des figures
		# Concaténation des vecteurs listés en paramètres
        tab1 <- data.frame(
                    annee,
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
        dispAn <- sglm1$deviance/sglm1$null.deviance

		# Table de sauvegarde des résultats
		tabAn <- data.frame(
                    id,
                    code_espece = sp,
                    nom_espece = nomSp,
                    indicateur = indic,
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
        # formule <- as.formula(paste("abond~ as.factor(carre) + annee",sep=""))
        formule <- as.formula(paste("abond~as.factor(carre) + annee + passage",sep="")) #[SYLUND] [MNHN?]
        
        md2 <- glm(formule,data=d,family=quasipoisson)
        
        smd2 <- summary(md2)
        
		# Tendances sur la période
		#coefannee <- tail(matrix(coefficients(md2)),1)
        coefannee <- head(tail(matrix(coefficients(md2)),2),1) #[SYLUND] [MNHN?] Récupération de l'avant-dernière valeur (année)

		trend <- round(exp(coefannee),3)
		
		# Pourcentage de variation sur la période
		pourcentage <- round((exp(coefannee*pasdetemps)-1)*100,2)
        #pval <- tail(matrix(summary(md2)$coefficients[,4]),1)
        pval <- head(tail(matrix(summary(md2)$coefficients[,4]),2),1) #[SYLUND] [MNHN?] Récupération de l'avant-dernière valeur (année)
        
        #erreuran <- as.data.frame(tail(matrix(summary(md2)$coefficients[,2]),1))
        erreuran <- as.data.frame(head(tail(matrix(summary(md2)$coefficients[,2]),2),1)) #[SYLUND] [MNHN?] Récupération de l'avant-dernière valeur (année)
        
		# Erreur standard 
        erreurannee2 <- as.vector(erreuran*exp(coefannee))[,1]
        #pval <- tail(coefficients(smd2),1)[,4]
        pval <- head(tail(coefficients(smd2),2),1)[,4] #[SYLUND] [MNHN?] Récupération de l'avant-dernière valeur (année)
        
		# Calcul des intervalles de confiance
        md2.sim <- sim(md2)
		# LL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.025),1)),3)
		LL <- round(exp(head(tail(apply(coef(md2.sim), 2, quantile,.025),2),1)),3) #[SYLUND] [MNHN?] Récupération de l'avant-dernière valeur (année)
        # UL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.975),1)),3)
		UL <- round(exp(head(tail(apply(coef(md2.sim), 2, quantile,.975),2),1)),3) #[SYLUND] [MNHN?] Récupération de l'avant-dernière valeur (année)
       
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
        
        # Surdispersion
        dispTrend <- smd2$deviance/smd2$null.deviance
        
		# Classement en catégorie 'incertain'
        if(dispTrend > 2 | dispAn > 2 | median(nb_carre_presence)<seuilOccu) catIncert <- "Incertain" else catIncert <-"Bon"
        
        vecLib <-  NULL
        
        if(dispTrend > 2 | dispAn > 2 | median( nb_carre_presence)<seuilOccu) {
             if(median(nb_carre_presence)<seuilOccu) {
                vecLib <- c(vecLib,"Espèce trop rare")
            }
            if(dispTrend > 2 | dispAn > 2) {
                vecLib <- c(vecLib,"Déviance")
            }
        }
        
        raisonIncert <-  paste(vecLib,collapse=" et ")
		
		# Affectation des tendances EBCC
		catEBCC <- affectCatEBCC(trend=as.vector(trend),pVal=pval,ICinf=as.vector(LL),ICsup=as.vector(UL))
        
        # Table complète de résultats
		tabTrend <- data.frame(
                        id,
                        code_espece = sp,
                        nom_espece = nomSp,
                        indicateur = indic,
                        nombre_annees = pasdetemps,
                        premiere_annee = firstY,
                        derniere_annee = lastY,
                        tendance = as.vector(trend),
                        IC_inferieur = as.vector(LL),
                        IC_superieur = as.vector(UL),
                        pourcentage_variation = as.vector(pourcent),
                        erreur_standard = as.vector(round(erreurannee2,4)),
                        p_value = round(pval,3),
                        significatif = trendsignif,
                        categorie_tendance_EBCC = catEBCC,
                        mediane_occurrence = median(nb_carre_presence),
                        valide = catIncert,
                        raison_incertitude = raisonIncert)
        
       
        # Concaténation des lignes 'espèces' dans le cas de plusieurs espèces à analyser
        if(sp==listSp[1]) {
            glmAn <- tabAn
            glmTrend <- tabTrend
        } else  {
            glmAn <- rbind(glmAn,tabAn)
            glmTrend <- rbind(glmTrend,tabTrend)
        }
        
	# Figures
        # Si le paramètre 'figure' = TRUE (fonction générale main.glm())
        if(figure) {
            # Table complète pour la figure en panel par ggplot2 (?)
            # Table pour graph en panel par ggplot2 (?)
           
            # Si le paramètre 'description' = TRUE (fonction générale main.glm())
            if(description)	dgg <- rbind(tab1,tab2,tab3) else dgg <- tab1
     
            ggplot.espece(dgg,tab1t,id,serie=NULL,sp,valide=catIncert,nomSp,description,tendanceSurFigure,seuilOccu=14,vpan=vpan)
                   
        }

    }
    
    filesaveAn <- paste("Résultats/",id,"/variationsAnnuellesEspece_",id,".csv",sep="")
    filesaveTrend <- paste("Resultats/",id,"/tendanceGlobalEspece_",id,".csv",sep="")
    
    write.csv2(glmAn,filesaveAn,row.names=FALSE,quote=FALSE)
    cat("--->",filesaveAn,"\n")
    
    write.csv2(glmTrend,filesaveTrend,row.names=FALSE,quote=FALSE)
    cat("--->",filesaveTrend,"\n")
    
    flush.console()

}
