##############################################################
## script R de gestion des listes d'espèces dans les znieff ##
##############################################################


## https://github.com/romainlorrilliere/various_scripts/tree/master/ZNIEFF
## Romain Lorrilliere
## romainlorrilliere@gmail.com

#### Libraries ####

## library required
vecPackage=c("dplyr","readODS","reshape")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)


require(readODS)
require(reshape2)
require(dplyr)

##################



#' Mise à jour régional des espèces déterminantes par ZNIEFF
#'
#' @param id_REG identifiant numérique de la région
#' @param fichierZNIEFF.nom nom du fichier de la table des ZNIEFF
#' @param fichierZNIEFF.colnameZNIEFF nom de la colonne des identifiants des ZNIEFF dans la table des ZNIEFF par defaut NM_SFFZN
#' @param fichierZNIEFF.colnameSp nom de colonnes code sp dans le fichiers ZNIEFF par defaut CD_REF
#' @param fichierSp.nom vecteur des fichiers des espèces déterminantes
#' @param fichierSp.colnameSp vecteur des noms des colonnes code sp dans les fichiers espèces
#' @param output pour obtenir un résumé par ZNIEFF cf. return section
#' @param fichierZNIEFF.encoding encodage du fichier ZNIEFF par defaut UTF-8
#' @param fichierZNIEFF.stringsAsFactor valeur du paramètre stringAsFactors du fichier ZNIEFF defaut FALSE
#' @param fichierZNIEFF.decimal valeur du paramètre dec (décimal) du fichier ZNIEFF defaut ,
#' @param fichierSp.encoding vecteur des encodages des fichiers espèces par defaut UTF-8
#' @param fichierSp.stringsAsFactor vecteur des valeurs du paramètre stringAsFactors des fichiers espèces par defaut FALSE
#' @param fichierSp.decimal vecteur des valeurs du paramètre dec (décimal) des fichiers espèces par defaut ,
#'
#' @return if output == TRUE table résumé des ZNIEFF
#' @export sauvegarde de 2 fichiers : mise à jour de la table des ZNIEFF et une table résumé des ZNIEFF régionales
#'
#' @author Romain Lorrilliere
#'
#' @examples regionalisation_espece_znieff(id_REG="52",fichierZNIEFF.nom="BDZNIEFF_PROD_2018-12-13_19-19-29/PROD_ESPECE_2018-12-13.csv",fichierSp.nom=c("Liste_PDL_ 2018_Flore_vf.ods","Liste_PDL_ 2018_Faunev_vf3.ods"),fichierSp.colnameSp=c("CD_NOM (TaxRef12)","CD_NOM (Taxref12)"))

regionalisation_espece_znieff <- function(id_REG,
                                          fichierZNIEFF.nom,
                                          fichierZNIEFF.colnameZNIEFF="NM_SFFZN",
                                          fichierZNIEFF.colnameSp ="CD_REF",
                                          fichierSp.nom,fichierSp.colnameSp,
                                          output=FALSE,
                                          fichierZNIEFF.encoding="UTF-8",
                                          fichierZNIEFF.stringsAsFactor=FALSE,
                                          fichierZNIEFF.decimal=",",
                                          fichierSp.encoding="UTF-8",
                                          fichierSp.stringsAsFactor=FALSE,
                                          fichierSp.decimal=",") {

### set de variable pour debugage

### id_REG="52";fichierZNIEFF.nom="BDZNIEFF_PROD_2018-12-13_19-19-29/PROD_ESPECE_2018-12-13.csv";fichierZNIEFF.encoding="UTF-8"; fichierZNIEFF.stringsAsFactor=FALSE; fichierZNIEFF.decimal=";"; fichierSp.nom=c("Liste_PDL_ 2018_Flore_vf.ods","Liste_PDL_ 2018_Faunev_vf3.ods");fichierSp.colnameSp=c("CD_NOM (TaxRef12)","CD_NOM (Taxref12)");fichierSp.encoding="UTF-8"; fichierSp.stringsAsFactor=FALSE; fichierSp.decimal=";";output=FALSE;fichierZNIEFF.colnameZNIEFF="NM_SFFZN";fichierZNIEFF.colnameSp ="CD_REF"

    library(reshape2)
    library(dplyr)

    if(length(fichierSp.nom)==1) {
        vecsp <- get.listSp(fichierSp.nom,encoding=fichierSp.encoding,dec=fichierSp.decimal,
                            stringsAsFactor=fichierSp.stringsAsFactor,colnameSp=fichierSp.colnameSp)
    }else{
        tFile <- tibble(fichierSp.nom,fichierSp.decimal,
                        fichierSp.stringsAsFactor,fichierSp.encoding,fichierSp.colnameSp)
        vecsp <- NULL
        for(i in 1:nrow(tFile)) {
            vecsp <- c(vecsp, get.listSp(tFile$fichierSp.nom[[i]],
                                         encoding=tFile$fichierSp.encoding,dec=fichierSp.decimal[[i]],
                                         stringsAsFactor=tFile$fichierSp.stringsAsFactor[[i]],
                                         colnameSp=tFile$fichierSp.colnameSp[[i]]))

        }
        vecsp <- unique(vecsp)

    }


    d.sp <-  read.csv.ods(fichierZNIEFF.nom,encoding=fichierZNIEFF.encoding,
                          dec=fichierZNIEFF.decimal,
                          stringsAsFactor=fichierZNIEFF.stringsAsFactor)

    if(fichierZNIEFF.colnameZNIEFF != "NM_SFFZN") colnames(d.sp)[colnames(d.sp)==fichierZNIEFF.colnameZNIEFF] <- "NM_SFFZN"
    if(fichierZNIEFF.colnameSp != "CD_REF") colnames(d.sp)[colnames(d.sp)==fichierZNIEFF.colnameSp] <- "CD_REF"


    colZNIEFF <- colnames(d.sp)
    d.sp$REG <- substr(d.sp$NM_SFFZN,1,2)==id_REG
    d.sp$FG_ESP_REG <- ifelse(substr(d.sp$NM_SFFZN,1,2)==id_REG,
                   ifelse(d.sp$CD_REF %in% vecsp,"D",ifelse(d.sp$FG_ESP == "C","C","A")),
                   NA)

    d.sp$ID_ZNIEFF_STATUT_CHANGE <- d.sp$FG_ESP != d.sp$FG_ESP_REG

    tabZNIEFF <- aggregate(ID_ZNIEFF_STATUT_CHANGE ~ NM_SFFZN , d.sp,subset=REG,any)

    tabZNIEFF_num<- aggregate(as.numeric(ID_ZNIEFF_STATUT_CHANGE) ~ NM_SFFZN , d.sp,
                              subset=REG,sum)
    colnames(tabZNIEFF_num)[2] <- "NB_CHANGE"
    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_num,by=fichierZNIEFF.colnameZNIEFF,all=TRUE)

    tabZNIEFF_FG_ESP_REG <- table(subset(d.sp,REG,select=c("NM_SFFZN","FG_ESP_REG")))
    colnames(tabZNIEFF_FG_ESP_REG) <- paste("REG_",id_REG,"_NB_",colnames(tabZNIEFF_FG_ESP_REG),sep="")
    tabZNIEFF_FG_ESP_REG <- dcast(as.data.frame(tabZNIEFF_FG_ESP_REG),NM_SFFZN ~ FG_ESP_REG)
    tabZNIEFF_FG_ESP <- table(subset(d.sp,REG,select=c("NM_SFFZN","FG_ESP")))
    colnames(tabZNIEFF_FG_ESP) <- paste("NB_",colnames(tabZNIEFF_FG_ESP),sep="")
    tabZNIEFF_FG_ESP <- dcast(as.data.frame(tabZNIEFF_FG_ESP),NM_SFFZN ~ FG_ESP)

    d.sp.change <- subset(d.sp,REG & ID_ZNIEFF_STATUT_CHANGE)
    tabZNIEFF_new <- aggregate(ID_ESPECE ~ NM_SFFZN + FG_ESP_REG, d.sp.change,length)

    tabZNIEFF_new <- dcast(tabZNIEFF_new,NM_SFFZN ~ FG_ESP_REG)
    colnames(tabZNIEFF_new)[2:ncol(tabZNIEFF_new)] <- paste("REG_",id_REG,"_NEW_",colnames(tabZNIEFF_new)[2:ncol(tabZNIEFF_new)],sep="")
    tabZNIEFF_new[is.na(tabZNIEFF_new)] <- 0

    tabZNIEFF_remove <- aggregate(ID_ESPECE ~ NM_SFFZN + FG_ESP, d.sp.change,length)
    tabZNIEFF_remove <- dcast(tabZNIEFF_remove,NM_SFFZN ~ FG_ESP)
    colnames(tabZNIEFF_remove)[2:ncol(tabZNIEFF_remove)] <- paste("REG_",id_REG,"_REMOVE_",colnames(tabZNIEFF_remove)[2:ncol(tabZNIEFF_remove)],sep="")
    tabZNIEFF_remove[is.na(tabZNIEFF_remove)] <- 0

    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_new,by="NM_SFFZN",all=TRUE)
    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_remove,by="NM_SFFZN",all=TRUE)
    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_FG_ESP,by="NM_SFFZN",all=TRUE)
    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_FG_ESP_REG,by="NM_SFFZN",all=TRUE)
    tabZNIEFF[is.na(tabZNIEFF)] <- 0

    newfichierZNIEFFsummary.nom <- paste("ZNIEFF_REG_",id_REG,".csv",sep="")
    cat("\n\n  -->  Sauvegarde table résumé par ZNIEFF:\n", newfichierZNIEFFsummary.nom)
    write.csv2(tabZNIEFF, newfichierZNIEFFsummary.nom,row.names=FALSE)
    cat("\n       DONE !\n")

    newfichierZNIEFF.nom <- paste(substr(fichierZNIEFF.nom,1,nchar(fichierZNIEFF.nom)-4),"_REG_",id_REG,".csv",sep="")

    cat("\n\n  -->  Sauvegarde de la table ZNIEFF mise à jour avec nouvelles colonnes:\n", newfichierZNIEFF.nom)
    write.csv(d.sp, newfichierZNIEFF.nom,row.names=FALSE,fileEncoding=fichierZNIEFF.encoding)
    cat("\n       DONE !\n")

    newfichierZNIEFFlite.nom <- paste(substr(fichierZNIEFF.nom,1,nchar(fichierZNIEFF.nom)-4),"_REG_",id_REG,"_MNHN_lite.csv",sep="")

    d.sp$FG_ESP <- d.sp$FG_ESP_REG
    d.sp <- d.sp[,colZNIEFF]

    cat("\n\n  -->  Sauvegarde de la table ZNIEFF mise à jour sans nouvelles colonnes:\n", newfichierZNIEFF.nom)
    write.csv(d.sp, newfichierZNIEFFlite.nom,row.names=FALSE,fileEncoding=fichierZNIEFF.encoding)
    cat("\n       DONE !\n")




    if(output) return(tabZNIEFF)
}




read.csv.ods <- function(file,sep=c("\t",";",","),
                         dec=",",encoding=encoding,
                         stringsAsFactors) {
#browser()
    fileExtension <- substr(file,(nchar(file)-2),nchar(file))

    cat("importation fichier:",file,"\n")

    if(fileExtension %in% c("csv","txt")) {
        flag <- TRUE
        i <- 0
        vecSep <- sep
        cat("\nfile type: csv ou txt")

        while(flag){
            i <- i+1
            theSeparator <- vecSep[i]
            d <- read.delim(file,sep=theSeparator,dec=dec,
                            header=TRUE,encoding=encoding,stringsAsFactors=stringsAsFactors)
            flag <- ncol(d)<2
        }
        cat("   separateur:",theSeparator,"\n")

    } else {
        if(fileExtension == "ods") {
             cat("\nfile type: ods")
            library(readODS)
            d <- read_ods(file)
        } else {
            stop("Type de fichier '",fileExtension,"' non pris en charge!!!\n les format pris en charge sont : txt, csv et ods\n")
        }

    }
    return(d)
}



get.listSp <- function(file,encoding="UTF-8",dec=",",
                       stringsAsFactor=FALSE,colnameSp) {

    d <- read.csv.ods(file,encoding=encoding,dec=dec,
                      stringsAsFactor=stringsAsFactors)
#browser()
    return(as.vector(d[,colnameSp]))


}
