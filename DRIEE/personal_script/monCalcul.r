
monCalcul <- function(site,  fau.return = TRUE)

    {

if(!("vegan" %in% rownames(installed.packages())))
    install.packages("vegan")


#######################################################
#########  	Utilisation de la fonction I2M2		#########
#######################################################

###########====================================###########
### Auteurs: Mondy Cédric & Usseglio-Polatera Philippe ###
###########====================================###########

# Création:                 2013/06/17
# Dernières modifications:  2013/06/17
#------------------------------------

  #######################################
  ###  Donner un nom à cette session	###
  #######################################

  run = paste("essai", Sys.Date(), sep = "_")

  #######################################################################
  ###  Chargement des listes faunistiques et du descriptif des sites  ###
  #######################################################################
  
    ## Listes faunistiques
    ## -------------------

      # Indiquer le nom du fichier faunistique:

      file_fau = paste("fau_",site,".csv",sep="")
      
      # Import du fichier précédemment renseigné:

      fau = read.csv2(paste("../input/", file_fau, sep=""), h = TRUE, row.names = 1)
    
    ## Description des sites
    ## ---------------------
    
      # Indiquer le nom du fichier faunistique:

      file_desc = paste("desc_",site,".csv",sep="")

      # Import du fichier précédemment renseigné:

      desc = read.csv2(paste("../input/", file_desc, sep=""), h = TRUE, row.names = 1)

  ###################################################################################
  ### Voulez-vous que les listes faunistiques harmonisées (opecont, B1B2 et B2B3) ###
  ### soient retournées (TRUE ou FALSE)                                           ###
  ###################################################################################

  

####=============================####
## A partir d'ici, ne pas modifier ##
####=============================####


  #################################################################
  ###  Chargement de la fonction permettant le calcul de l'I2M2	###
  #################################################################

      source("../libraries/I2M2.R")

  ##########################
  ###  Calcul de l'I2M2  ###
  ##########################

      i2m2_calc = I2M2(fau, desc, fau.return = fau.return)

  ##############################
  ###  Export des résultats  ###
  ##############################

    ## Définition du dossier d'export (nom de session)
    ## -----------------------------------------------

    loc = paste("../output/", run, "/", sep = "")
    
    if(file.exists(substr(x=loc, start=1, stop=nchar(loc)-1)) == FALSE){
      
      dir.create(loc, recursive = TRUE)
    }

    ## Listes faunistiques
    ## -------------------

      if(i2m2_calc$fau.return == TRUE){
        write.csv2(i2m2_calc$fau_prel, paste(loc, "listes harmonisees prelevement.csv", sep = ""))
        write.csv2(i2m2_calc$fau_b1b2, paste(loc, "listes harmonisees bocaux B1 et B2.csv", sep=""))
        write.csv2(i2m2_calc$fau_b2b3, paste(loc, "listes harmonisees bocaux B2 et B3.csv", sep=""))
        write.csv2(i2m2_calc$fau_opecont, paste(loc, "listes harmonisees opecont.csv", sep=""))
      }
    

    ## Métriques brutes
    ## ----------------

      write.csv2(i2m2_calc$Metriques, paste(loc, "Metriques.csv", sep=""))

    ## Métriques en EQR
    ## ----------------
    
      write.csv2(i2m2_calc$EQR, paste(loc, "EQR.csv", sep=""))

    ## I2M2
    ## ----
    
      write.csv2(i2m2_calc$I2M2, paste(loc, "I2M2.csv", sep=""))




}
