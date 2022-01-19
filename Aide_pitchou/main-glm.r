### Fonction g�n�rale de calcul de la variation temporelle et de la tendance
### La fonction g�n�re aussi les graphiques

main.glm <- function(
                id,                             # ID de la session de calcul (par d�faut = date et heure du traitement)
                donneesAll,                     # Fichier de donn�es format� au d�but du programme
                listSp=NULL,                    # Liste d'esp�ces � analyser (par d�fault, toutes les esp�ces du fichier de donn�es sont trait�es)
                annees=NULL,
                echantillon=1,
                methodeEchantillon=NULL,
                figure=TRUE,                    # Active la r�alisation des graphiques
                description=TRUE,
                tendanceSurFigure=TRUE,
                tendanceGroupSpe = FALSE,
                seuilOccu=14,                   # Seuil fix� arbitrairement [MNHN?]
                seuilAbond=NA) {                # Seuil non-fix� ?! [MNHN?]
    
    require(arm)
    require(ggplot2)
	
	# Seuil de significativit�
	seuilSignif <- 0.05
	
	# Charge la table de r�f�rence des esp�ces
	tabsp <- read.csv2("Librairie/espece.csv")
	
	# Cr�� une 1e colonne avec en ligne le contenu de la colonne 'sp' (code esp�ce)
    rownames(tabsp) <- tabsp$sp
    
    # Vecteur des panels de la figure
    vpan <- c("Variation abondance")
    if(description) vpan <- c(vpan,"Occurrences","Abondances brutes")
    
    # Quelle utilit� ? Variable 'nomfile1' reprise nulle part...
    # nomfile1 <- paste("Donnees/carre2001-2014REGION_",id,".csv",sep="")

	# Liste des ann�es pr�sentes dans le fichier � analyser
    annee <- sort(unique(donneesAll$annee))
    
    # Nombre d'ann�es dans le fichier � analyser
    nbans <- length(annee)
    
    # Pas de temps (?)
    pasdetemps <- nbans-1
    
    # Ann�e la plus ancienne
    firstY <- min(annee)
    
    # Ann�e la plus r�cente
    lastY <- max(annee)

	# Si aucune(s) esp�ce(s) n'est pass�e en param�tre --> analyse de toutes les esp�ces
    if (is.null(listSp)) {
        # Liste des esp�ces de la table de r�f�rence
        spValide <- as.vector(tabsp$sp)
        # S�lection des noms d'esp�ces du fichier en entr�e qui correspondent aux noms d'esp�ces de la table de r�f�rence
        listSp <- colnames(donneesAll)[colnames(donneesAll)%in%spValide]
        # Filtre les lignes de la table de r�f�rence des esp�ces qui sont repr�sent�es dans le fichier � analyser
        tabsp2 <- tabsp[listSp,]
        # Liste des esp�ces � analyser (esp�ces indicatrices uniquement et tri�es par ordre alphab�tique)
        listSp <- listSp[order(as.numeric(tabsp2$shortlist)*-1,tabsp2$sp)]
    }
    
	# Analyse par esp�ce
	i <- 0
	nbSp <- length(listSp)
	#browser()
	
    for (sp in listSp) {
    
		i <- i + 1
        
        # Filtre des donn�es pour l'esp�ce en cours (sp)
        # d <- data.frame(abond=donneesAll[,sp],annee=donneesAll$annee,carre=donneesAll$carre)
        d <- data.frame(abond=donneesAll[,sp],annee=donneesAll$annee,carre=donneesAll$carre,passage=donneesAll$passage) # [SYLUND]
		
		# S�lection du nom de l'esp�ce
        nomSp <- as.character(tabsp[sp,"nom"])
        
        # Affichage de la ligne de l'esp�ce en cours (rang + nom d'esp�ce)
		cat("\n(",i,"/",nbSp,") ",sp," | ", nomSp,"\n",sep="")
		
		flush.console()
		
		# Stocke TRUE/FALSE selon que l'esp�ce fait partie des esp�ces indicatrices reconnues � l'�chelle nationale
		shortlist <- tabsp[sp,"shortlist"]
		
		# Stocke TRUE/FALSE selon que l'esp�ce est incluse dans un groupe d'indicateurs (calculs par groupes d'indicateurs)
        indic <- tabsp[sp,"indicateur"]

        # Nombre total de carr�s suivis par ann�e
        # Renvoie un tableau avec ann�es en colonnes et nombre de carr�s suivis sur la 1e ligne
        # nb_carre <- tapply(rep(1,times=nrow(d)),d$annee,sum)
        nb_carre <- tapply(ifelse(d$passage>0,1,0),d$annee,sum) #[SYLUND]
        
        # Nombre de carr�s suivis par ann�e, avec pr�sence
        # M�me calcul que pour 'nb_carre' mais pour les carr�s avec des individus d�tect�s uniquement
		nb_carre_presence <- tapply(ifelse(d$abond>0,1,0),d$annee,sum)
		
        # Tableau de r�sultat d'analyses
        # Chaque ann�e est r�p�t�e sur 2 lignes avec respectivement le nb. total de carr�s suivis et le nb. de carr�s avec pr�sence en colonne 'val'
        tab2 <- data.frame(
                        annee=rep(annee,2),
                        val=c(nb_carre,nb_carre_presence),
                        LL=NA,
                        UL=NA,
                        catPoint=NA,
                        pval=NA,
                        courbe=rep(c("carre","presence"),each=length(annee)),   # stocke 'carre' si nb. total carr�s et 'presence' si nb. carr�s avec pr�sence
                        panel=vpan[2])
        
        # Remplissage colonne 'catPoint' en fonction du seuil d'occurence d�fini dans main.glm()
        tab2$catPoint <- ifelse(tab2$val == 0,"0",ifelse(tab2$val < seuilOccu,"infSeuil",NA))
        
		# Abondance brute par ann�e
        # Renvoie un tableau avec ann�es en colonnes et abondances brutes sur la 1e ligne
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
        
        # Remplissage colonne 'catPoint' en fonction du seuil d'abondance d�fini dans main.glm()
        tab3$catPoint <- ifelse(tab3$val == 0,"0",ifelse(tab3$val < seuilAbond,"infSeuil",NA))

		# GLM Variation d'abondance
		# formule <- as.formula("abond~as.factor(carre)+as.factor(annee)")
		
		# D�finition de la formule
		formule <- as.formula("abond~as.factor(carre)+as.factor(annee)+passage") #[SYLUND] [MNHN?] A confirmer par le MNHN...
        
        # Ex�cution et stockage de la formule
        glm1 <- glm(formule,data=d,family=quasipoisson)
        
        # R�sum� des r�sultats de la formule
        sglm1 <- summary(glm1)
        
        # Stocke les n (='pasdetemps') derni�res valeurs renvoy�es la fonction 'glm1' (valeurs par ann�es)
        # coefan <- tail(matrix(coefficients(glm1)),pasdetemps)
        coefan <- tail(matrix(coefficients(glm1)),nbans) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour r�cup�rer les ann�es 2009 � 2020 (avec 'pasdetemps' : on r�cup�re pas 2009...)
        
        # Suppression de la derni�re valeur de 'coefan' (passage)
        coefan <- head(coefan, -1) # [SYLUND] [MNHN?]
        
		# Vecteur des variations d'abondance par ann�e 'back transform�e' (?)
		# Concat�ne la valeur 1 (1e ann�e de suivi) en t�te de la variable 'coefan' et calcule l'exponentielle des valeurs
		coefannee <- rbind(1,exp(coefan))
        
        # erreuran <- as.data.frame(tail(matrix(summary(glm1)$coefficients[,2]),pasdetemps))
        erreuran <- as.data.frame(tail(matrix(summary(glm1)$coefficients[,2]),nbans)) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour r�cup�rer les ann�es 2009 � 2020 (avec 'pasdetemps' : on r�cup�re pas 2009...)
        
        # Suppression de la derni�re valeur de 'erreuran' (passage)
        erreuran <- head(erreuran, -1) # [SYLUND] [MNHN?]
        
		# Vecteur des erreurs standards 'back transform�es' (?)
        erreurannee1 <- as.vector(rbind(0,erreuran*coefannee)[,1])
        
        # pval <- c(1,tail(coefficients(sglm1),pasdetemps)[,4])
        pval <- c(1,tail(coefficients(sglm1),nbans)[,4]) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour r�cup�rer les ann�es 2009 � 2020 (avec 'pasdetemps' : on r�cup�re pas 2009...)
        
        # Suppression de la derni�re valeur de 'pval' (passage)
        pval <- head(pval, -1) # [SYLUND] [MNHN?]
        
        # Calcul des intervalles de confiance
        glm1.sim <- sim(glm1)
        # ic_inf_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.025),pasdetemps)))
        # ic_sup_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.975),pasdetemps)))
        ic_inf_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.025), nbans))) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour r�cup�rer les ann�es 2009 � 2020 (avec 'pasdetemps' : on r�cup�re pas 2009...)
        ic_sup_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.975), nbans))) #[SYLUND] [MNHN?] Remplacement de 'pasdetemps' par 'nbans' pour r�cup�rer les ann�es 2009 � 2020 (avec 'pasdetemps' : on r�cup�re pas 2009...)
		
		# Suppression de la derni�re valeur de 'ic_inf_sim' (passage)
		ic_inf_sim <- head(ic_inf_sim, -1) # [SYLUND] [MNHN?]
		
		# Suppression de la derni�re valeur de 'ic_sup_sim' (passage)
		ic_sup_sim <- head(ic_sup_sim, -1) # [SYLUND] [MNHN?]
		
		# Table pour la r�alisation des figures
		# Concat�nation des vecteurs list�s en param�tres
        tab1 <- data.frame(
                    annee,
                    val=coefannee,
                    LL=ic_inf_sim,
                    UL=ic_sup_sim,
                    catPoint=ifelse(pval<seuilSignif,"significatif",NA),
                    pval,
                    courbe=vpan[1],
                    panel=vpan[1])
	
	    # Nettoyage des intervalles de confiance sup�rieurs tr�s grands				   
        tab1$UL <- ifelse(nb_carre_presence==0,NA,tab1$UL)
        tab1$UL <- ifelse(tab1$UL==Inf,NA,tab1$UL)
        tab1$UL <- ifelse(tab1$UL>1.000000e+20,NA,tab1$UL)
        tab1$UL[1] <- 1
        tab1$val <-  ifelse(tab1$val>1.000000e+20,1.000000e+20,tab1$val)
        
		# Indice de surdispersion
        dispAn <- sglm1$deviance/sglm1$null.deviance

		# Table de sauvegarde des r�sultats
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
        
		# GLM Tendance g�n�rale sur la p�riode
        # formule <- as.formula(paste("abond~ as.factor(carre) + annee",sep=""))
        formule <- as.formula(paste("abond~as.factor(carre) + annee + passage",sep="")) #[SYLUND] [MNHN?]
        
        md2 <- glm(formule,data=d,family=quasipoisson)
        
        smd2 <- summary(md2)
        
		# Tendances sur la p�riode
		#coefannee <- tail(matrix(coefficients(md2)),1)
        coefannee <- head(tail(matrix(coefficients(md2)),2),1) #[SYLUND] [MNHN?] R�cup�ration de l'avant-derni�re valeur (ann�e)

		trend <- round(exp(coefannee),3)
		
		# Pourcentage de variation sur la p�riode
		pourcentage <- round((exp(coefannee*pasdetemps)-1)*100,2)
        #pval <- tail(matrix(summary(md2)$coefficients[,4]),1)
        pval <- head(tail(matrix(summary(md2)$coefficients[,4]),2),1) #[SYLUND] [MNHN?] R�cup�ration de l'avant-derni�re valeur (ann�e)
        
        #erreuran <- as.data.frame(tail(matrix(summary(md2)$coefficients[,2]),1))
        erreuran <- as.data.frame(head(tail(matrix(summary(md2)$coefficients[,2]),2),1)) #[SYLUND] [MNHN?] R�cup�ration de l'avant-derni�re valeur (ann�e)
        
		# Erreur standard 
        erreurannee2 <- as.vector(erreuran*exp(coefannee))[,1]
        #pval <- tail(coefficients(smd2),1)[,4]
        pval <- head(tail(coefficients(smd2),2),1)[,4] #[SYLUND] [MNHN?] R�cup�ration de l'avant-derni�re valeur (ann�e)
        
		# Calcul des intervalles de confiance
        md2.sim <- sim(md2)
		# LL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.025),1)),3)
		LL <- round(exp(head(tail(apply(coef(md2.sim), 2, quantile,.025),2),1)),3) #[SYLUND] [MNHN?] R�cup�ration de l'avant-derni�re valeur (ann�e)
        # UL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.975),1)),3)
		UL <- round(exp(head(tail(apply(coef(md2.sim), 2, quantile,.975),2),1)),3) #[SYLUND] [MNHN?] R�cup�ration de l'avant-derni�re valeur (ann�e)
       
        # Table pour la r�alisation des figures 
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
        
		# Classement en cat�gorie 'incertain'
        if(dispTrend > 2 | dispAn > 2 | median(nb_carre_presence)<seuilOccu) catIncert <- "Incertain" else catIncert <-"Bon"
        
        vecLib <-  NULL
        
        if(dispTrend > 2 | dispAn > 2 | median( nb_carre_presence)<seuilOccu) {
             if(median(nb_carre_presence)<seuilOccu) {
                vecLib <- c(vecLib,"Esp�ce trop rare")
            }
            if(dispTrend > 2 | dispAn > 2) {
                vecLib <- c(vecLib,"D�viance")
            }
        }
        
        raisonIncert <-  paste(vecLib,collapse=" et ")
		
		# Affectation des tendances EBCC
		catEBCC <- affectCatEBCC(trend=as.vector(trend),pVal=pval,ICinf=as.vector(LL),ICsup=as.vector(UL))
        
        # Table compl�te de r�sultats
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
        
       
        # Concat�nation des lignes 'esp�ces' dans le cas de plusieurs esp�ces � analyser
        if(sp==listSp[1]) {
            glmAn <- tabAn
            glmTrend <- tabTrend
        } else  {
            glmAn <- rbind(glmAn,tabAn)
            glmTrend <- rbind(glmTrend,tabTrend)
        }
        
	# Figures
        # Si le param�tre 'figure' = TRUE (fonction g�n�rale main.glm())
        if(figure) {
            # Table compl�te pour la figure en panel par ggplot2 (?)
            # Table pour graph en panel par ggplot2 (?)
           
            # Si le param�tre 'description' = TRUE (fonction g�n�rale main.glm())
            if(description)	dgg <- rbind(tab1,tab2,tab3) else dgg <- tab1
     
            ggplot.espece(dgg,tab1t,id,serie=NULL,sp,valide=catIncert,nomSp,description,tendanceSurFigure,seuilOccu=14,vpan=vpan)
                   
        }

    }
    
    filesaveAn <- paste("R�sultats/",id,"/variationsAnnuellesEspece_",id,".csv",sep="")
    filesaveTrend <- paste("Resultats/",id,"/tendanceGlobalEspece_",id,".csv",sep="")
    
    write.csv2(glmAn,filesaveAn,row.names=FALSE,quote=FALSE)
    cat("--->",filesaveAn,"\n")
    
    write.csv2(glmTrend,filesaveTrend,row.names=FALSE,quote=FALSE)
    cat("--->",filesaveTrend,"\n")
    
    flush.console()

}
