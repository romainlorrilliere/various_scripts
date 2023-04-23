# programme de fabrication de la matrice de presence absence
#				source("presenceAbscence.r")

#       fonction globale : presenceAbscence()

#				ranger les fichiers à traiter dans un repertoir à la racine nome /data/
#				presenceAbscneceLight.txt est le tableau dans lequel tout les lignes qui 
#         qui n'ont que des 0 sont supprimé


# tableau de conversion des tags
lesTags = function(nombrePigeon)
{
  decimal = (1:nombrePigeon)
  hexadecimal = sprintf("%x", decimal) 
  vect=cbind(decimal,hexadecimal)
  vect = as.data.frame(vect)
  return(vect)
}

# fabrication du nom de fichier en fonction de la date
nomFichier = function(dateJour)
{
  return(paste(dateJour,'.csv',sep='') )
}

# lecture du fichier
lectureFichier= function(nom)
{
  nom = paste('data/',nom,sep='')
  tableau = read.csv2(nom)
  tableau = cbind(tableau,as.numeric(paste("0x",as.character(tableau[,3]),sep='')))
  names(tableau)=c("id","date","tagHexa","tagDec")
  return(tableau)
}

# fabrication d'une colonne
presenceAbscenceJour = function (tableauJour,dateJour,vecteurPigeon)
{
  tableauPresenceJour = matrix(0,length(vecteurPigeon))
  tableauPresenceJour = as.data.frame(tableauPresenceJour) 
  names(tableauPresenceJour)=c(dateJour)
  #Recherche de la présence de chaque pigeon dans le tableau boucle avec p pour pigeon
  for (p in 1:length(vecteurPigeon)) 
  {
    tableauPresenceJour[p,1]=as.numeric(length(which(tableauJour[,4]==p))>0)
  }
  return(tableauPresenceJour)
}

# boucle global qui lit les fichiers qui pour toutes les dates fabrique le vecteurs de presence abscence
# et enregistre la sortie dans 2 fichiers : presenceAbscence.txt et  presenceAbscenceLight.txt
presenceAbscence = function()
{
  vecteurPigeon = lesTags(500)[,2]
  tableauPresence = matrix(0,length(vecteurPigeon))
  tableauPresence = as.data.frame(tableauPresence) 
  names(tableauPresence)=c('colonneVide')
  listeFichier = dir('data/')
  for (an in 2006:2008) 
  {
     for (month in 1:12) 
     {
     if (month < 10) month = paste(0,month,sep='')
      for (day in 1:31) 
      {
        if (day < 10) day = paste(0,day,sep='')
        dateJour = paste(an,month,day,sep='-')
        fichier = nomFichier(dateJour)
        
        if (length(which(listeFichier==fichier))>0) 
        {
          tableauJour = lectureFichier(fichier)
          tableauPresence = cbind(tableauPresence,presenceAbscenceJour(tableauJour,dateJour,vecteurPigeon))
        }
      }
     }
  }
  tableauPresence = tableauPresence[,2:dim(tableauPresence)[2]]
  write.table(tableauPresence,"presenceAbscence.txt")
  tableauPresenceLight = tableauPresence[which(apply(tableauPresence,1,sum)>0),]
  write.table(tableauPresenceLight,"presenceAbscenceLight.txt")
}




presenceAbscence()
 


