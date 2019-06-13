# programme de mise en forme des table PASSAGE TRONCON_PASSAGE CHIRO
# pour l'utilis� il faut mettre l'ensemble des fichiers au format CSV
# dans le r�pertoire "saisieChiroFileAttenteCSV" .

#____________________________________________________________
# !!!!! ATTENTION le repertoire "saisieChiroFileAttenteCSV"
# ne doit contenir que les fichiers � traiter CSV !!!!!!!

#--------------------------------------------------------------

# Re attention si on traite directement les fichiers dans les case
# il n'y a pas des valeurs mais des formules

#--------------------------------------------------------------

# 		source("cleanchiro.r")

#		fonction � utiliser : 
#				chiro.auto()

#------------------------

# lecture fichier

lecture.csv = function(file.name)
{
	cat(" --> ",file.name,"\n")
	flush.console()
	return(read.csv2(file.name))
}










#------------------------

# fabrique l'enregistrement de la table SESSION du fichier en cour de traitement.
# les champs � r�f�rencer sont :
# "REF_PASSAGE","ID_CIRCUIT","DATE","NUMEROS_PASSAGE","OPERATEUR_MANIP",
# "NOM_FICHIER_XLS","TEMPERATURE","PLUIE","VENT","NUAGE","HEURE","PASSAGE_VIDE",
# "PASSAGE_VALIDE","REMARQUE_PASSAGE"

make.session = function(tab)
{
	new.tab = data.frame(
		gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1], perl=TRUE), # "REF_PASSAGE"
		tab$Num�ros.ciruit[1], # "ID_CIRCUIT"
		as.character(tab$Date[1]), # "DATE"
		tab$N..Passage[1],  # "NUMEROS_PASSAGE"
		tab$Enregistr�.par[1], # "OPERATEUR_MANIP"
		tab$Renomer.le.fichier.Excel[1], # "NOM_FICHIER_XLS"
		NA, #"TEMPERATURE"
		NA, #"PLUIE"
		NA, #"VENT"
		NA, #"NUAGE"
		NA, #"HEURE"
		"Non", #"PASSAGE_VIDE"
		"Oui", #"PASSAGE_VALIDE"
		"" #"REMARQUE_PASSAGE"
	)
	
	colnames(new.tab) = c("REF_SESSION","ID_CIRCUIT","DATE","NUMEROS_PASSAGE",
    "OPERATEUR_MANIP","NOM_FICHIER_XLS","TEMPERATURE","PLUIE","VENT","NUAGE",
    "HEURE","SESSION_VIDE","SESSION_VALIDE","REMARQUE_SESSION")

	return(new.tab)
}







#------------------------

# fabrique les enregistrements de la table TRONCON du fichier en cour de traitement.
# les champs � r�f�rencer sont : 
# "REF_TRONCON","ID_PASSAGE","ID_TRONCON","NUMEROS_TRONCON","TEMPS","VITESSE",
# "NOM_FICHIER_WAV","TRONCON_VIDE","TRONCON_VALIDE","REMARQUE_TRONCON"

make.troncon = function(tab)
{
	# recherche du nombre de troncon
	nbTroncon =  min(which(is.na(tab$Temps.tron�on[-(1:15)])))+10

	new.tab = data.frame(
		paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1],perl=TRUE),
      "-Tron",1:nbTroncon,sep=""), #"REF_TRONCON"
		gsub("-Chiro.xls", "",tab$Renomer.le.fichier.Excel[1], perl=TRUE),#"ID_PASSAGE"
		paste("C",tab$Num�ros.ciruit[1],"T",1:nbTroncon,sep="") , # "ID_TRONCON"
		1:nbTroncon, #NUMEROS_TRONCON
		tab$Temps.tron�on[5:(nbTroncon + 4)],  #TEMPS
		(7200 / tab$Temps.tron�on[5:(nbTroncon + 4 )]), #VITESSE
		paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1], perl=TRUE),
      "-Tron",1:nbTroncon,"-Chiro.wav",sep=""), #NOM_FICHIER_WAV
		troncon.vide(tab,nbTroncon), #"TRONCON_VIDE"
		troncon.valide(tab,nbTroncon) ,#"TRONCON_VALIDE"
		"" #"REMARQUE_TRONCON"
	)  
	colnames(new.tab) = c("REF_TRONCON_SESSION","ID_SESSION","ID_TRONCON",
    "NUMEROS_TRONCON","TEMPS","VITESSE","NOM_FICHIER_WAV","TRONCON_VIDE",
    "TRONCON_VALIDE","REMARQUE_TRONCON")

	return(new.tab)
}







#------------------------

#Calcul du temps du troncon, 
# probl�me il faut d'abord v�rifi� � quelle troncon la donn�e doit �tre attribu�e

troncon.temps = function(tab,nbTroncon)
{
	temps = rep(NA,nbTroncon)
	for (tron in 5:(nbTroncon + 5))
	{
		temps[as.numeric(
      gsub(paste("Cir",tab$Num�ros.ciruit[1],"-2006-Pass",tab$N..Passage[1],"-Tron",sep="")
        ,"",
        gsub("-Chiro.wav", "",tab$Renomer.le.fichier.Excel[tron],
      perl=TRUE),perl=TRUE))] = tab$Temps.tron�on[tron]
	}
	return(temps)
}
	
#------------------------

#Calcul de la vitesse du troncon, 
# probl�me il faut d'abord v�rifi� � quelle troncon la donn�e doit �tre attribu�e

troncon.vitesse = function(tab,nbTroncon)
{
	temps = rep(NA,nbTroncon)
	for (tron in 5:(nbTroncon + 5))
	{
		temps[as.numeric(
        gsub(
          paste("Cir",tab$Num�ros.ciruit[1],"-2006-Pass",tab$N..Passage[1],"-Tron",sep="")
          ,"",
          gsub("-Chiro.wav", "",tab$Renomer.le.fichier.Excel[tron], perl=TRUE),
        perl=TRUE))] = (7200 / tab$Temps.tron�on[tron])
	}
	return(temps)
}



#------------------------
	
#"TRONCON_VIDE" : TRONCON absent | TRONCON pr�sent une fois & (esp�ce = "VIDE" | "")


troncon.vide = function(tab,nbTroncon)
{
	tronconEmpty = rep("Non",nbTroncon)
	for (tron in 1:nbTroncon)
	{
		index.troncon = which(tab$N..du.tron == tron)
		if (length(index.troncon)<2)
  		if (length(index.troncon) == 0)
   			tronconEmpty[tron] = "Oui"
 			else
   				if ((tab$Esp�ce[index.troncon] == "VIDE")|
              (tab$Esp�ce[index.troncon] == "")|
              (tab$Esp�ce[index.troncon] == "PROBLEME"))
      				    tronconEmpty[tron] = "Oui"
	}
	return(tronconEmpty)
}

	
#------------------------
	
#"TRONCON_VALIDE" : esp�ce == "PROBLEME" | "probl�me" |"probleme")
# renvoie un vecteur qui dit si le tron�on est valide
# l'observateur � mis "probl�me" dans la colonne esp�ce

troncon.valide = function(tab,nbTroncon)
{
	tronconValide = rep("Oui",nbTroncon)
	tronconValide[tab$N..du.tron[which((tab$Esp�ce == "PROBLEME") |
      (tab$Esp�ce == "probl�me") |
      (tab$Esp�ce == "probleme"))]] = "Non"
	return(tronconValide)
}


#------------------------

# fabrique les enregistrements de la table CHIRO du fichier en cour de traitement.
# les chams � r�f�rencer sont : 
# "REF_CHIRO","ID_SECTEUR_SESSION","ID_TRONCON_SESSION",
# "OPERATEUR_IDENTIFICATION","TEMPS","ESPECE","EFFECTIF",
# "DECLENCHEMENT","DEGRES_CONFIANCE","REMARQUE"

make.chiro = function(tab,troncon)
{
	new.tab = data.frame(
		paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1], perl=TRUE),
        "_",1:dim(tab)[1],sep=""), # "REF_CHIRO"
		paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1],perl=TRUE),
        "-Tron",tab$N..du.tron�on,"-S",trunc(tab$Temps..s./(troncon$TEMPS[tab$N..du.tron�on]/5))+1,
        sep=""), # "ID_SECTEUR_SESSION"
		paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1],perl=TRUE),
        "-Tron",tab$N..du.tron�on,sep=""), # "ID_TRONCON_SESSION"
		tab$Analys�.par, # "OPERATEUR_IDENTIFICATION"
		tab$Temps..s., #TEMPS"
		tab$Esp�ce, # "ESPECE"
		tab$Effectif, # "EFFECTIF"
		ifelse(tab$D�clench�.par.un.orthopt�re.=="oui","ORTHOPTERE",NA), # "DECLENCHEMENT"
		tab$Degr�.de.confiance, # "DEGRES_CONFIANCE"
		tab$Remarque # "REMARQUE"
	)
	colnames(new.tab) = c("REF_CHIRO","ID_SECTEUR_SESSION","ID_TRONCON_SESSION",
          "OPERATEUR_IDENTIFICATION","TEMPS","ESPECE","EFFECTIF",
          "DECLENCHEMENT","DEGRES_CONFIANCE","REMARQUE")
	new.tab = new.tab[which(new.tab$ESPECE != ""),] # ne concerve que les lignes saisies
	new.tab = new.tab[which(new.tab$ESPECE != "VIDE"),] # ne concerve pas les lignes � probl�me
	return(new.tab)
}







#------------------------	

# fabrique les enregistrements de la table SECTEUR du fichier en cour de traitement.
# Les champs � r�f�rencer sont :
# "REF_SECTEUR","ID_TRONCON","ID_SECTEUR","NUMEROS_SECTEUR","SECTEUR_VIDE"

make.secteur = function(tab,chiro,troncon)
{
	nbTroncon = dim(troncon)[1]
	new.tab = data.frame(
		paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1],perl=TRUE),
      "-Tron",rep(1:nbTroncon,each = 5),"-S",1:5,sep=""), # "REF_SECTEUR"
		paste(paste(gsub("-Chiro.xls", "", tab$Renomer.le.fichier.Excel[1],perl=TRUE),
      "-Tron",rep(1:nbTroncon,each = 5),sep="")), # "ID_TRONCON_SESSION"
		paste("C",tab$Num�ros.ciruit[1],"T",rep(1:nbTroncon,each = 5),"S",1:5,sep=""), # "ID_SECTEUR"
		rep(1:5,nbTroncon), # "NUMEROS_SECTEUR"
		secteur.vide(chiro,troncon) # "SECTEUR_VIDE"
	)
  colnames(new.tab) = c( "REF_SECTEUR_SESSION","ID_TRONCON_SESSION",
    "ID_SECTEUR","NUMEROS_SECTEUR","SECTEUR_VIDE")
	return(new.tab)
}


















#------------------------


secteur.vide = function(chiro,troncon)
{
	secteurEmpty = rep("Non",nbTroncon*5)
	nbTroncon = dim(troncon)[1]
	for(T in 1:nbTroncon)
	{
		if(troncon$TRONCON_VIDE[T] == "Oui")
			secteurEmpty[((T*5)-4):(T*5)] = "Oui"
		else
			for( S in 1:5)
			{
				id.secteur = paste(troncon$REF_PASSAGE[1],"-Tron",T,"-S",S,sep="")
				if (length(which(chiro$ID_SECTEUR == id.secteur)) == 0)
					secteurEmpty[(5*(T-1))+S] = "Oui"
  		}
	}
	return(secteurEmpty)
}


#------------------------
# Enregistre dans un fichier la table correspondante

save.chiro = function(tab,nom.table)
{
	nomFichier = paste("../SaisieChiro_Result/",format(Sys.time(), "%Y_%m_%d"),
    "-",nom.table,".txt",sep="")
	write.table(tab,nomFichier,row.names = FALSE)
	cat("          <-- ",nomFichier,"\n") 
}







#------------------------

chiro.auto = function()
{
	cat("   Traitement des fichiers du r�pertoire \n")
	list.fichier = dir("../saisieChiroFileAttenteCSV",full.names = TRUE)
	i = 1
	tab = lecture.csv(list.fichier[i])
	session = make.session(tab)
	troncon = make.troncon(tab)
	nbTroncon = dim(troncon)[1]
	chiro = make.chiro(tab,troncon)
	secteur = make.secteur(tab,chiro,troncon)
	session.global = session
	troncon.global = troncon
	secteur.global = secteur
	chiro.global = chiro

	if (length(list.fichier) > 1)
	{
		for (i in 2:length(list.fichier))
		{
			tab = lecture.csv(list.fichier[i])
			session = make.session(tab)
			troncon = make.troncon(tab)
			chiro = make.chiro(tab,troncon)
			secteur = make.secteur(tab,chiro,troncon)
			session.global = rbind(session.global,session)
			troncon.global = rbind(troncon.global,troncon)
			secteur.global = rbind(secteur.global,secteur)
			chiro.global = rbind(chiro.global,chiro)
		}
	}
	save.chiro(session.global,"session")
	save.chiro(troncon.global,"troncon")
	save.chiro(secteur.global,"secteur")
	save.chiro(chiro.global,"chiro")
}









