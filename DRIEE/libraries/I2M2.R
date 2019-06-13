#########################################
#########	 Fonction R I2M2()		#########
#########################################

###########====================================###########
### Auteurs: Mondy Cédric & Usseglio-Polatera Philippe ###
###########====================================###########

# Création:                 2013/04/17
# Dernières modifications:  2013/07/03
#------------------------------------

###################################################################
###  Chargement des tableaux nécessaires au calcul de l'indice	###
###################################################################
  
  ## Correspondances taxonomiques
  ## ----------------------------
  
  taxo2rcs = read.csv2("../database/taxo2rcs.csv", h = TRUE)
  
  ## Valeurs des modalités de traits et des scores BMWP
  ## --------------------------------------------------
  
  base = read.csv2("../database/base.csv", h = TRUE)
  
  ## Typologie simplifiée des rivières
  ## ---------------------------------
  
  typo_I2M2 = read.csv2("../database/typo_I2M2.csv", h = TRUE)
  
  ## Valeurs de référence des métriques
  ## ----------------------------------
  
  best = read.csv2("../database/best.csv", h = TRUE, row.names = 1)
  colnames(best) = c("Shannon (B1B2)", "ASPT (B2B3)", "Polyvoltinism (B1B2B3)", "Ovoviviparity (B1B2B3)", "Richness (B1B2B3)")
  
  ## Pires valeurs des métriques
  ## ---------------------------
  
  worst = read.csv2("../database/worst.csv", h = TRUE, row.names = 1)
  temp = as.vector(t(worst))
  names(temp) = rownames(worst)
  worst = temp
  
  ## Efficacité de discrimination
  ## ----------------------------
  
  de = read.csv2("../database/de.csv", h = TRUE, row.names = 1)

  #######################
  ###  Fonction I2M2  ###
  #######################

I2M2=function(fau, desc, fau.return = F){

  if ( !require(vegan) )
  {
    install.packages("vegan")
    library(vegan)
  }
  
fau = as.data.frame(fau)
names(fau) = sub(pattern = "X", replacement = "", x=names(fau))

desc = as.data.frame(desc)

test = row.names(fau) %in% row.names(desc)

if(length(test[test == FALSE]) > 0) stop("Les tableaux fau et desc doivent correspondrent aux mêmes échantillons")

desc = desc[row.names(fau),]

test = c("cd_opecont", "typo_nationale", "bocal") %in% names(desc)
names(test) = c("cd_opecont", "typo_nationale", "bocal")

if(length(test[test == FALSE]) > 0) stop("Le tableau desc doit contenir les champs suivants: cd_opecont, typo_nationale et bocal")
  

	#################################################
	###	0-Vérification du nombre de prélèvements	###
	#################################################
  
	temp=table(desc[,c("cd_opecont", "bocal")])
	temp2=paste(temp[,"B1"], temp[,"B2"], temp[,"B3"])
	names(temp2)=row.names(temp)
	out=names(temp2[(temp2%in%c("1 1 1", "4 4 4"))==FALSE])

if(length(out)!=0){
	print("Les évènements d'échantillonnages suivant ne seront pas considérés car le nombre de prélèvement par bocal ou le nombre de bocaux n'est pas respecté")
	print(out)
	}

	fau=fau[(desc$cd_opecont%in%out)==FALSE,]
	desc=desc[(desc$cd_opecont%in%out)==FALSE,]

	if(nrow(desc)==0) stop("Aucun évenement d'échantillonnage ne correspond à un plan d'échantillonnage correct")

###################################
###	1- Verification des taxons	###
###################################

test=names(fau)[is.na(match(names(fau), taxo2rcs[["cd_taxon"]])) == TRUE]

if(length(test)>0){
  print("Le(s) taxon(s) suivant(s) n'existe(nt) pas dans la base de données:")
  print(test)
}

fau = fau[,names(fau) %in% taxo2rcs[["cd_taxon"]]]


###########################################################
###  2- Verification et simplification de la typologie	###
###########################################################
t0 = Sys.time()

print("Simplification de la typologie:")

typo_simple=as.character(replace(desc$typo_nationale, is.na(desc$typo_nationale)==FALSE, NA))
names(typo_simple) = row.names(desc)

for(i in unique(typo_I2M2[["typo_nationale"]])){
  typo_simple[desc[["typo_nationale"]] %in% i] = typo_I2M2[typo_I2M2[["typo_nationale"]] %in% i, "typo_I2M2_v2"]
}

if(length(typo_simple[is.na(typo_simple) | typo_simple %in% c("20", "48", "59")]) > 0) {
  out = unique(desc[["typo_nationale"]][is.na(typo_simple) | typo_simple %in% c("20", "48", "59")])
  print("Les types nationaux suivants ne sont pas encore pris en compte par l'I2M2:")
  print(out)
  
  fau = fau[(desc[["typo_nationale"]] %in% out) == FALSE,]
  desc = desc[row.names(fau),]
  typo_simple = typo_simple[row.names(fau)]
}

desc[["typo_simple"]]=typo_simple

if(nrow(desc)==0) stop("Aucune donnée ne correspond à des types de cours d'eau pour lesquels nous disposons de conditions de référence.")

t1 = Sys.time()

print(paste("ok (", round(t1-t0, 2), "s)", sep=""))

###################################
###	3- Harmonisation des Listes	###
###################################
t0 = Sys.time()

print("Harmonisation des listes faunistiques:")

  #########################################################
  ## Fonction effectuant la redistribution des effectifs ##
  #########################################################
matchup=function(fau, desc){
  
  taxo = taxo2rcs[match(names(fau), taxo2rcs[["cd_taxon"]]),]
  
  # 0 - Cas des taxons dont seule la presence (et non l'abondance) est requise
  
  pres <- c("P")                    # completer par d'autres codes non numériques
                                    # suceptibles d'avoir ete utilises pour la 
                                    # presence des taxons
  
  test <- length(taxo[["Quantification"]][taxo[["Quantification"]] %in% "presence"])
  fau_pres <- colnames(fau)[ taxo[["Quantification"]] %in% "presence"]
  
  if(pres %in% as.matrix(fau[, (colnames(fau) %in% fau_pres) == FALSE])){
    pres2 <- c()
    for(i in 1:length(pres)){
      if(i == 1){
        pres2 = pres[i]
      } else {
        pres2 <- paste(pres2, pres[i], sep=", ")
      }
    }
    stop(c("Un ou plusieurs codes 'presence' (", pres2, ") ont ete utilises pour des taxons ne faisant pas partie de la liste renseignees dans le tableau 'taxo2rcs'"))
  }
  
  if(test > 0){
    for(i in fau_pres){
      if(is.numeric(fau[,i])){
        cond <- fau[,i] > 0
      } else {
        fau[,i] <- as.character(fau[,i])
        
        if(pres %in% fau[,i]){
          cond <- fau[,i] %in% pres
        }
      }
      fau[,i] <- as.numeric(replace(fau[,i], cond, 1))
    }
  }
  
  
  # 1 - Regroupe les taxons determines a un niveau inferieur a celui recommande
 
  name=list(row.names(fau),na.omit(unique(taxo[["cd_taxon_norme"]])))
  fau2=as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))
  
 
  for(i in names(fau2)){
    fau_temp = as.data.frame(fau[,taxo[["cd_taxon_norme"]] %in% i])
    row.names(fau_temp) = row.names(fau)
    
    if(ncol(fau_temp) == 1) {
      fau2[,i] = fau[,taxo[["cd_taxon_norme"]] %in% i]
    } else {
      fau2[,i] = rowSums(fau[,taxo[["cd_taxon_norme"]] %in% i], na.rm = TRUE)
    }
  }
  
  taxo2=taxo[match(names(fau2),taxo[["cd_taxon_norme"]]),]
  
  # 2-Reattribution des effectifs de taxons determines a un niveau superieur a celui recommande en se basant sur la distribution des effectifs des taxons presents dans la même opération de contrôle
  
  temp=as.data.frame(matrix(fau2[,!is.na(taxo2[["Redistribution"]]) & colSums(fau2, na.rm=T) != 0], 
                            nrow = nrow(fau2), 
                            ncol = length(taxo2[["Redistribution"]][!is.na(taxo2[["Redistribution"]]) & colSums(fau2, na.rm=T) != 0]), 
                            byrow = FALSE, 
                            dimnames = list(rownames(fau2), rownames(taxo2)[!is.na(taxo2[["Redistribution"]]) & colSums(fau2, na.rm=T) != 0])))
  
  total = ncol(temp)
  
  for(r in names(temp)){
    for(i in row.names(temp[(temp[,r] %in% c(0, NA)) == FALSE,])){
      
      opecont = as.character(desc[i, "cd_opecont"])
      
      sel = row.names(taxo2[taxo2[["cd_taxon"]] %in% r,])
      level=as.character(taxo2[sel,"Redistribution"])
      group=as.character(taxo2[sel,level])
      
      seltax=names(fau2[,taxo2[,names(taxo2)%in%level]%in%group])
      seltax = seltax[seltax != r]
      
      if(length(seltax) == 0){
        fau2[i, r] = fau2[i,r]
      } else{
        if(length(seltax) == 1){
          total = sum(fau2[desc[["cd_opecont"]] %in% opecont,seltax], na.rm=T)
          
          if(sum(total, na.rm=T) > 0) {
            fau2[i,seltax] = fau2[i,seltax]+fau2[i,r]
            fau2[i, r] = 0
          } else {
            fau2[i, r] = fau2[i,r]
          }
          
        } else {
          total=colSums(fau2[desc[["cd_opecont"]] %in% opecont,seltax], na.rm=T)
          
          if(sum(total, na.rm=T) > 0) {
            prop=total/sum(total, na.rm=T)
            fau2[i,seltax] = fau2[i,seltax]+round(fau[i,r]*prop)
          } 
      }
      }
     
    }
  }
  
  return(fau2[colSums(fau2, na.rm=TRUE) != 0])
}

  ################################################
  ## Utilisation de la fonction d'harmonisation ##
  ################################################

fau=matchup(fau, desc)

t1 = Sys.time()

print(paste("ok (", round(t1-t0, 2), "s)", sep=""))

	#####################################################
	###	4- Creation des listes aux differents niveaux	###
	#####################################################

t0 = Sys.time()

print("Création des listes pour les différents regroupements de bocaux:")

#######################################
## Niveau de l'opération de contrôle ##
#######################################
# Listes faunistiques
name = list(names(table(desc[["cd_opecont"]])[table(desc[["cd_opecont"]])!=0]), names(fau))
fau_opecont = as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

for(i in row.names(fau_opecont)){
  fau_opecont[i,] = colSums(fau[desc[["cd_opecont"]] %in% i,], na.rm = TRUE)
}

# Description des listes
name = list(names(table(desc[["cd_opecont"]])[table(desc[["cd_opecont"]])!=0]),names(desc[,c("cd_opecont", "typo_nationale", "typo_simple")]))
desc_opecont = as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

for(i in row.names(desc_opecont)){
  for(j in names(desc_opecont)){
  if(length(desc[desc[["cd_opecont"]] %in% i, j][is.na(desc[desc[["cd_opecont"]] %in% i, j]) == TRUE]) == length(desc[desc[["cd_opecont"]] == i, j])){
  desc_opecont[i,j] = NA
  }else{
  desc_opecont[i,j] = unique(desc[desc[["cd_opecont"]] %in% i, j])
  }}
  }


desc[["cd_b1b2"]][desc[["bocal"]] %in% c("B1", "B2")] = paste(desc[["cd_opecont"]][desc[["bocal"]] %in% c("B1", "B2")], "B1B2", sep = "-")
desc[["cd_b2b3"]][desc[["bocal"]] %in% c("B2", "B3")] = paste(desc[["cd_opecont"]][desc[["bocal"]] %in% c("B2", "B3")], "B2B3", sep = "-")

######################################
## Niveau du regroupement de bocaux ##
######################################

  ## B1B2
# Listes faunistiques
name = list(names(table(desc[["cd_b1b2"]])[table(desc[["cd_b1b2"]])!=0]), names(fau))
fau_b1b2 = as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

for(i in row.names(fau_b1b2)){
  fau_b1b2[i,] = colSums(fau[desc[["cd_b1b2"]] %in% i,], na.rm = TRUE)
}

# Description des listes
name = list(names(table(desc[["cd_b1b2"]])[table(desc[["cd_b1b2"]])!=0]),names(desc[, c("cd_opecont", "cd_b1b2", "typo_nationale", "typo_simple")]))
desc_b1b2 = as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

for(i in row.names(desc_b1b2)){
  for(j in names(desc_b1b2)){
    if(length(desc[desc[["cd_b1b2"]] %in% i, j][is.na(desc[desc[["cd_b1b2"]] %in% i, j]) == TRUE]) == length(desc[desc[["cd_b1b2"]] == i, j])){
      desc_b1b2[i,j] = NA
    }else{
      desc_b1b2[i,j] = unique(desc[desc[["cd_b1b2"]] %in% i, j])
    }}
}

## B2B3
# Listes faunistiques
name = list(names(table(desc[["cd_b2b3"]])[table(desc[["cd_b2b3"]])!=0]), names(fau))
fau_b2b3 = as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

for(i in row.names(fau_b2b3)){
  fau_b2b3[i,] = colSums(fau[desc[["cd_b2b3"]] %in% i,], na.rm = TRUE)
}

# Description des listes
name = list(names(table(desc[["cd_b2b3"]])[table(desc[["cd_b2b3"]])!=0]),names(desc[, c("cd_opecont", "cd_b2b3", "typo_nationale", "typo_simple")]))
desc_b2b3 = as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

for(i in row.names(desc_b2b3)){
  for(j in names(desc_b2b3)){
    if(length(desc[desc[["cd_b2b3"]] %in% i, j][is.na(desc[desc[["cd_b2b3"]] %in% i, j]) == TRUE]) == length(desc[desc[["cd_b2b3"]] == i, j])){
      desc_b2b3[i,j] = NA
    }else{
      desc_b2b3[i,j] = unique(desc[desc[["cd_b2b3"]] %in% i, j])
    }}
}

t1 = Sys.time()

print(paste("ok (", round(t1-t0, 2), "s)", sep=""))

	#################################
	###	5- Calcul des metriques		###
	#################################
t0 = Sys.time()

print("Calcul des métriques")

###############################################
## Indice de Shannon sur les bocaux B1 et B2 ##
###############################################
met1 = diversity(fau_b1b2, index="shannon", base=2, MARGIN=1)

##################################
## ASPT sur les bocaux B2 et B3 ##
##################################

# Creation de la fonction
	aspt=function(fau){
    require(vegan)
    
	taxo=base[,c("BMWP.taxo","BMWP.Original.Score")]
  row.names(taxo) = base[["cd_taxon"]]

	fau=as.data.frame(fau)
	fau2=fau[,colSums(fau, na.rm=TRUE)!=0]

	taxo2=taxo[match(names(fau2), row.names(taxo)),]

	name=list(names(table(taxo2[["BMWP.taxo"]])[table(taxo2[["BMWP.taxo"]])!=0]), row.names(fau2))
	fau3=as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))
  
	  for(i in row.names(fau3)){
      seltax = names(fau2)[taxo2[["BMWP.taxo"]] %in% i]
      if(length(seltax) == 1){
        fau3[i,] = fau2[,taxo2[["BMWP.taxo"]] %in% i]
      } else {
        fau3[i,] = rowSums(fau2[,taxo2[["BMWP.taxo"]] %in% i], na.rm=TRUE)
      }
	  }

  fau4=replace(fau3,fau3>0,1)
	taxo3=taxo2[match(row.names(fau3), taxo2[["BMWP.taxo"]]),]

	S= specnumber(fau4, MARGIN = 2)

	name=list(names(fau4),c("BMWP","ASPT"))
	temp=as.data.frame(matrix(nrow=length(name[[1]]),ncol=length(name[[2]]),dimnames=name))

	temp[,"BMWP"] = colSums(fau4*taxo3[,"BMWP.Original.Score"], na.rm=TRUE)   
	temp[,"ASPT"]=temp[,"BMWP"]/S
	return(temp)
	}

# Utilisation de la fonction
met2=aspt(fau_b2b3)[,"ASPT"]

################################################
## Polyvoltinisme sur les bocaux B1, B2 et B3 ##
################################################

# Creation de la fonction
	polyvoltin=function(fau){
	fau=as.data.frame(fau)

  fau2=as.data.frame(t(fau[,colSums(fau)!=0]))
  row.names(fau2)=names(fau[,colSums(fau)!=0])
	names(fau2)=row.names(fau)

	modalite=base[match(row.names(fau2), base[["cd_taxon"]]),"polyvoltine"]

	temp=colSums(modalite*log(fau2+1), na.rm = TRUE) / colSums(log(fau2+1), na.rm = TRUE)

	return(temp)
	}

# Utilisation de la fonction
met3=polyvoltin(fau_opecont)

###############################################
## Ovoviviparite sur les bocaux B1, B2 et B3 ##
###############################################

# Creation de la fonction
ovovivipare=function(fau){
  fau=as.data.frame(fau)
  
  fau2=as.data.frame(t(fau[,colSums(fau)!=0]))
  row.names(fau2)=names(fau[,colSums(fau)!=0])
  names(fau2)=row.names(fau)
  
	modalite=base[match(row.names(fau2), base[["cd_taxon"]]),"ovoviviparity"]

  temp=colSums(modalite*log(fau2+1), na.rm = TRUE) / colSums(log(fau2+1), na.rm = TRUE)
	  
	return(temp)
	}

# Utilisation de la fonction
met4=ovovivipare(fau_opecont)

#######################################################
## Richesse taxonomiques sur les bocaux B1, B2 et B3 ##
#######################################################

met5=specnumber(fau_opecont, MARGIN = 1)


################################
## Regroupement des métriques ##
################################

metriques=as.data.frame(cbind(met1[match(desc_opecont[["cd_opecont"]], desc_b1b2[["cd_opecont"]])],met2[match(desc_opecont[["cd_opecont"]], desc_b2b3[["cd_opecont"]])],met3,met4,met5))
names(metriques)=c("Shannon (B1B2)", "ASPT (B2B3)", "Polyvoltinism (B1B2B3)", "Ovoviviparity (B1B2B3)", "Richness (B1B2B3)")
row.names(metriques) = names(met3)

t1 = Sys.time()

print(paste("ok (", round(t1-t0, 2), "s)", sep=""))

	#########################
	###	6- Calcul des EQR	###
	#########################
t0= Sys.time()

print("Calcul des EQR:")

eqr=replace(metriques,is.na(metriques)==FALSE,NA)

for(i in row.names(eqr)){
  if(metriques[i, "Richness (B1B2B3)"] == 0){
    eqr[i,] = 0
  } else {
    eqr[i,] = (metriques[i,] - worst[names(metriques)]) / (best[row.names(best) == unique(desc[desc[["cd_opecont"]] %in% i, "typo_simple"]), names(metriques)] - worst[names(metriques)])
    eqr[i,][eqr[i,] < 0] = 0
    eqr[i,][eqr[i,] >1] = 1
  }
}

t1 = Sys.time()

print(paste("ok (", round(t1-t0, 2), "s)", sep=""))

	###########################
	###	7- Calcul de l'I2M2	###
	###########################

t0 = Sys.time()

print("Calcul de l'I2M2:")

name = list(row.names(eqr), names(de))
i2m2 = as.data.frame(matrix(nrow=length(name[[1]]), ncol=length(name[[2]]), dimnames=name))

for(p in names(i2m2)){
  temp = t(eqr)*de[, p]
  i2m2[[p]] = colSums(temp) / sum(de[[p]])
}

I2M2 = rowMeans(i2m2)

t1 = Sys.time()

print(paste("ok (", round(t1-t0, 2), "s)", sep=""))

	##########################
	### Fichiers de sortie ###
	##########################

temp=list()
temp[["Metriques"]]=metriques
temp[["EQR"]]=eqr
temp[["I2M2"]]=I2M2

if( fau.return == TRUE){
  temp[["fau.return"]] = TRUE
  temp[["fau_prel"]] = fau
  temp[["fau_b1b2"]] = fau_b1b2
  temp[["fau_b2b3"]] = fau_b2b3
  temp[["fau_opecont"]] = fau_opecont
} else {
  temp[["fau.return"]] = FALSE
}

return(temp)

}
