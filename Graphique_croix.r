##################################################################
####   Script generique pour realiser les figures en croix  ######
####       a partir des donnees brut                        ######
##################################################################
### Version V1.1 _ 2017-03-02
################################################################################################

### Les libraries
vecPackage=c("ggplot2","RColorBrewer")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)

library(ggplot2)
library(RColorBrewer)

### Fonction graphe

							col_abscisse = "richesse_tot_coleo";figure_abscisse = "Richesse totale";
                            col_ordonnee = "moy_abond_coleo";figure_ordonnee = "Abondance moyenne";
                            figure_titre = "Referentiel Coléoptères \n";
							nomProtocole = "Coléoptères";
							couleur_groupe = "chocolate1"
							# Coleo "chocolate1"   Abondances moyennes de Coléoptères \n (médianes et quantiles)
							# oisx "dodgerblue3"   Abondances moyennes d'Oiseaux \n (médianes et quantiles)
							
							col_sousGroup = "region_administrative"; # "plante_reference"  "region_administrative" "climat" "paysages"  "tprod" "typo_pays_principale"
							nomGenerique = "Global";
                            val_filtre = NULL;
                            seuilSegment=30;segmentSousSeuil=TRUE
							
							d<-ad_taxons_parcelle # ad_taxons abondance et richesse par taxon et par parcelle
					
							date_export = "_Export_du_01032018"
					
{  # chapeau qui fait les tables de travail
							
library(ggplot2)
                                 # browser()       
    d$sousGroup <- as.character(d[,col_sousGroup]) # liste des regions
    d$abscisse <- d[,col_abscisse] 
    d$ordonnee <- d[,col_ordonnee]
	
    d <- subset(d,!(is.na(sousGroup)) & !(is.na(abscisse)) & !(is.na(ordonnee)) & sousGroup != "")
    if(is.null(val_filtre)) site <- unique(d$sousGroup) else site <- val_filtre
        
    d.autre <- d 
    d.autre$sousGroup <- nomGenerique # la table va servir pour global
	
    for(j in site) { # boucle par region et save

        d.reseau <-  subset(d,sousGroup==j) # table qui va servir pour le regional 
        d.reseau$sousGroup <- j
        ggTable <- rbind(d.autre,d.reseau)

        seuilResum <- nrow(d.reseau) >= seuilSegment
        
        ggTableResum <- aggregate(cbind(ordonnee, abscisse) ~ sousGroup, data = ggTable,quantile, c(.25,.5,.75)) # table pour plot
        ggTableResum <- data.frame(ggTableResum[,1],ggTableResum[,2][,1:3],ggTableResum[,3][,1:3])
        colnames(ggTableResum) <- c("sousGroup","ordonnee.inf","ordonnee.med","ordonnee.sup","abscisse.inf","abscisse.med","abscisse.sup")
        if(ggTableResum$sousGroup[2]==nomGenerique) ggTableResum <- ggTableResum[c(2,1),] # assure l'ordre des lignes Global, puis sousGroup
            
        gg <- ggplot(d,aes(x=abscisse,y=ordonnee,colour=sousGroup,fill=sousGroup))#+ theme_minimal()
        gg <- gg + geom_hline(data=subset(ggTableResum,sousGroup== nomGenerique),aes(yintercept = ordonnee.med,colour=sousGroup),size=.5,linetype="dashed") + geom_vline(data=subset(ggTableResum,sousGroup==nomGenerique),aes(xintercept = abscisse.med,colour=sousGroup),size=.5,linetype="dashed")
        
		if(segmentSousSeuil) {
            gg <- gg + geom_segment(data=ggTableResum,aes(x = abscisse.med, y = ordonnee.inf, xend = abscisse.med, yend = ordonnee.sup),alpha=.8,size=2.5)
            gg <- gg + geom_segment(data=ggTableResum,aes(x = abscisse.inf, y = ordonnee.med, xend = abscisse.sup, yend = ordonnee.med),alpha=.8,size=2.5)			
			if(!(seuilResum)) {
                gg <- gg + geom_segment(data=subset(ggTableResum,sousGroup!=nomGenerique),aes(x = abscisse.med, y = ordonnee.inf, xend = abscisse.med, yend = ordonnee.sup),alpha=.5,size = 1.5,colour="white")
                gg <- gg + geom_segment(data=subset(ggTableResum,sousGroup!=nomGenerique),aes(x = abscisse.inf, y = ordonnee.med, xend = abscisse.sup, yend = ordonnee.med),alpha=.5,size = 1.5,colour="white")
            }
        } else {
            gg <- gg + geom_segment(data=subset(ggTableResum,sousGroup==nomGenerique),aes(x = abscisse.med, y = ordonnee.inf, xend = abscisse.med, yend = ordonnee.sup),alpha=.8,size = 2.5)
            gg <- gg + geom_segment(data=subset(ggTableResum,sousGroup==nomGenerique),aes(x = abscisse.inf, y = ordonnee.med, xend = abscisse.sup, yend = ordonnee.med),alpha=.8,size = 2.5)
        }

        gg <- gg + geom_point(data=d.reseau,size=2)
        gg <- gg + labs(list(title=paste(figure_titre,j),x=figure_abscisse,y=figure_ordonnee))

        gg <- gg + scale_colour_manual(values =setNames(c("grey",couleur_groupe),c("Global",j)),guide=FALSE)
		gg <- gg + scale_fill_manual(values =setNames(c("grey",couleur_groupe),c("Global",j)),guide=FALSE)	
		gg <- gg + theme(plot.title=element_text(size=16),axis.text=element_text(size=12),axis.title=element_text(size=14))
	
	   # browser()
        ggfile <- paste(output_graph_region,"Croix_",nomProtocole,"_",j,date_export,".png",sep="")
        cat("Check",ggfile,":")
        ggsave(ggfile,gg)
        cat("\n")
        flush.console()
    }

	
	}
