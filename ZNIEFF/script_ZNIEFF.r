## script R de gestion des listes d'espèces dans les znieff


## the packages


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




#'
#'

#'
#' @examples regionalisation_espece_znieff(id_REG="52",fichierZNIEFF.nom="BDZNIEFF_PROD_2018-12-13_19-19-29/PROD_ESPECE_2018-12-13.csv",fichierSp.nom=c("Liste_PDL_ 2018_Flore_vf.ods","Liste_PDL_ 2018_Faunev_vf3.ods"),fichierSp.colnameSp=c("CD_NOM (TaxRef12)","CD_NOM (Taxref12)"))



regionalisation_espece_znieff <- function(id_REG,
                                          fichierZNIEFF.nom,
                                          fichierZNIEFF.colnameZNIEFF="NM_SFFZN",
                                          fichierSp.nom,fichierSp.colnameSp,
                                          output=FALSE,
                                          fichierZNIEFF.encoding="UTF-8",
                                          fichierZNIEFF.stringsAsFactor=FALSE,
                                          fichierZNIEFF.decimal=",",
                                          fichierSp.encoding="UTF-8",
                                          fichierSp.stringsAsFactor=FALSE,
                                          fichierSp.decimal=",") {


                                        #id_REG="52";fichierZNIEFF.nom="BDZNIEFF_PROD_2018-12-13_19-19-29/PROD_ESPECE_2018-12-13.csv";fichierZNIEFF.encoding="UTF-8"; fichierZNIEFF.stringsAsFactor=FALSE; fichierZNIEFF.decimal=";"; fichierSp.nom=c("Liste_PDL_ 2018_Flore_vf.ods","Liste_PDL_ 2018_Faunev_vf3.ods");fichierSp.colnameSp=c("CD_NOM (TaxRef12)","CD_NOM (Taxref12)");fichierSp.encoding="UTF-8"; fichierSp.stringsAsFactor=FALSE; fichierSp.decimal=";"output=FALSE

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
#browser()
    if(fichierZNIEFF.colnameZNIEFF != "NM_SFFZN") colnames(d.sp)[colnames(d.sp)==fichierZNIEFF.colnameZNIEFF] <- "NM_SFFZN"

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
    colnames(tabZNIEFF_FG_ESP_REG) <- paste("REG_",id_REG,"_NB_",colnames(tabZNIEFF_FG_ESP_REG),
                                            sep="")
    tabZNIEFF_FG_ESP_REG <- dcast(as.data.frame(tabZNIEFF_FG_ESP_REG),NM_SFFZN ~ FG_ESP_REG)
    tabZNIEFF_FG_ESP <- table(subset(d.sp,REG,select=c("NM_SFFZN","FG_ESP")))
    colnames(tabZNIEFF_FG_ESP) <- paste("NB_",colnames(tabZNIEFF_FG_ESP),sep="")
    tabZNIEFF_FG_ESP <- dcast(as.data.frame(tabZNIEFF_FG_ESP),NM_SFFZN ~ FG_ESP)

    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_FG_ESP,by="NM_SFFZN",all=TRUE)
    tabZNIEFF <- merge(tabZNIEFF,tabZNIEFF_FG_ESP_REG,by="NM_SFFZN",all=TRUE)


    newfichierZNIEFFsummary.nom <- paste("ZNIEFF_REG_",id_REG,".csv",sep="")
    cat("\n\n  -->  Sauvegarde table résumé par ZNIEFF:\n", newfichierZNIEFFsummary.nom)
    write.csv(tabZNIEFF, newfichierZNIEFFsummary.nom,row.names=FALSE)
    cat("\n       DONE !\n")

    newfichierZNIEFF.nom <- paste(substr(fichierZNIEFF.nom,1,nchar(fichierZNIEFF.nom)-4),"_REG_",id_REG,".csv",sep="")

    cat("\n\n  -->  Sauvegarde de la table ZNIEFF mise à jour:\n", newfichierZNIEFF.nom)
    write.csv(d.sp, newfichierZNIEFF.nom,row.names=FALSE)
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
