
# source("/personal script/traitement_cemagref.r")




prepareFileCemagref <- function(assembler=FALSE) {
    listeFile <- dir("../input_cemagref/")
    
cat("\n Nombre de fichiers:",length(listeFile),"\n ------------------------\n\n")
    flush.console()
    if(assembler) {
        fau <- oneFile(listeFile,renvoie=TRUE)
        descriptionCoursdEau(fau)
    } else {
        for(f in listeFile) {
            fau <- tableByFile(f,renvoie=TRUE)
            descriptionCoursdEau(fau,f)
        }
    }
        
}

descriptionCoursdEau <- function(fau,f="compilation.csv") {
    desc <- read.table("../libraries/type de cours deau.txt",header=TRUE)
    row.names(desc) <- desc$CDPOINTDECONTROLE
    tdesc <- data.frame(cd_opecont=paste(substr(row.names(fau),1,7),substr(row.names(fau),9,10),sep=""),
                        bocal=substr(row.names(fau),12,13),
                        typo_nationale=desc[substr(row.names(fau),1,7),2])
    row.names(tdesc) <- row.names(fau)
    fichierOutput <-  paste("../input/desc_",f,sep="")
    write.csv2(tdesc,file=fichierOutput,row.names=TRUE)
    cat("  -->",fichierOutput ,"\n")
}



tableByFile <- function(nomFichier,renvoie=FALSE,sauve=TRUE) {
    cat(nomFichier,"\n")
    flush.console()
    fileInput <- paste("../input_cemagref/",nomFichier,sep="")
    d <- read.csv2(fileInput,header=TRUE,skip=86)
    if(ncol(d)==1) d <- read.csv(fileInput,header=TRUE,skip=86)
    limitd <- min(which(is.na(d$CODE.SANDRE)))
    d <- d[1:(limitd-1),]
    codeStation <- d$CODE.STATION[1]
    year <- substr(as.character(d$DATE[1]),nchar(as.character(d$DATE[1]))-1,nchar(as.character(d$DATE[1])))
    nomLignes <- paste(codeStation,year,paste("B",1:3,sep=""),sep="-")
    nomColonnes <-  d$CODE.SANDRE
    tab <- t(as.matrix(d[,c("PHASE.A","PHASE.C","PHASE.C")]))
    rownames(tab) <- nomLignes
    colnames(tab) <- nomColonnes
    tab <- ifelse(is.na(tab),0,as.numeric(tab))
    fichierOutput <-  paste("../input/fau_",nomFichier,sep="")
    if(sauve) {
        write.csv2(tab,file=fichierOutput,row.names=TRUE)
        cat("  -->",fichierOutput ,"\n")
    }
    if(renvoie) return(tab)
}


oneFile <- function(listeFile,renvoie=TRUE) {
    for(f in listeFile) {
        tabf <- tableByFile(f,renvoie=TRUE,sauve=FALSE)
        nomB <- rownames(tabf)
        nomC <- colnames(tabf)
        tabf <- data.frame(t(tabf))
        tabf$code <- as.character(row.names(tabf))
        if(f == listeFile[1]) {
            tab <- tabf
            lesnomB <- nomB
            lesnomC <- unique(nomC)
        } else {
            tab <- merge(tab,tabf,by="code",incomparables=0,all=TRUE)
            lesnomB <- c(lesnomB,nomB)
            lesnomC <- unique(c(lesnomC,nomC)) 
        }
    }
    nomColonnes <- tab$code
    nomLignes <- lesnomB
    tab <- tab[,-1]
    tab <- as.matrix(tab)
    tab <- t(tab)
    tab <- ifelse(is.na(tab),0,tab)
    rownames(tab) <- nomLignes
    colnames(tab) <- nomColonnes
    fichierOutput <-  paste("../input/fau_compilation.csv",sep="")
    write.csv2(tab,file=fichierOutput,row.names=TRUE)
       cat("  -->",fichierOutput ,"\n")
    return(tab)
}
