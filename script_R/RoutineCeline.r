

#"C:\Program Files\Asreml2\bin\asreml.exe" -sSn A1f.as
#"C:\Program Files\Asreml2\bin\asreml.exe" -sSn A1f.pin
#rename A1f.asr replicate1results.txt
#rename A1f.pvc estimate1results.txt


maDOS.Boucle = function()
{
  repertoire = "C:/Nicodemus/PEDANTIX_LD/"
  program = paste(repertoire,"PHENSIM11b",sep="")
  fichier = paste(repertoire, "LDf_Phensim.txt", sep="")
  commande = paste(program,fichier)
  system(commande,show.output.on.console=F)
  
  program = paste(repertoire,"ADVPHENSIM2.exe ",sep="")
  fichier1 = paste(repertoire, "outLDf_Phensim.txt", sep="")
  fichier2 = paste(repertoire, "10R.txt", sep="")
  commande = paste(program,fichier1,fichier2)
  system(commande,show.output.on.console=F)
  
  laTable = lire.tableau()
  laBelleTable = alakeuleuleu(laTable)
  
  colnames(laBelleTable) = c("Band","LD")
  outFile = paste(repertoire,"AdvLDf.asd")
  write.table(laBelleTable,na='NA',outFile,sep="\t",row.names=FALSE,quote=FALSE)
  
  
  setwd(repertoire)
  
  program = "asreml.exe"
  fichier = paste(repertoire, "A1f.as", sep="")
  parametre = "-sSn"
  commande = paste(program,parametre,fichier)
  system(commande,show.output.on.console=F)
  
  
  fichier = paste(repertoire, "A1f.pin", sep="")
  #parametre = "-sSn"
  commande = paste(program,parametre,fichier)
  system(commande,show.output.on.console=F)
  
  conDeFichier = paste(repertoire,"A1f.pvc",sep="")
  tab = read.delim(conDeFichier,colClasses="character")
  for(i in 2:5)
  {
    d = tab[i,1]
    ligneEtHopPasPropre = strsplit(d," ")
    ligneEtHop =  ligneEtHopPasPropre[[1]][which(ligneEtHopPasPropre[[1]]!="")]
    if(i == 2)   tabEtHop = ligneEtHop
    else  tabEtHop = rbind(tabEtHop,ligneEtHop)
    
  }
  etHop = tabEtHop[,c(2,4,5)]
    
  for(i in 6:8)
  {
    d = tab[i,1]
    ligneEtHopPasPropre = strsplit(d," ")
    ligneEtHop =  ligneEtHopPasPropre[[1]][which(ligneEtHopPasPropre[[1]]!="")]
    if(i == 6)   tabEtHop = ligneEtHop
    else  tabEtHop = rbind(tabEtHop,ligneEtHop)
  
  }
  etHop2 =  tabEtHop[,c(1,7,8)]
  
  leTableau = rbind(etHop,etHop2)
  leTableau = as.vector(t(leTableau[,2:3]))
  colonne = c("Vp_estim","Vp_se","Va_estim","Va_se","Ve_estim","Ve_se","Vr_estim","Vr_se","h2_estim","h2_se","e2_estim","e2_se","r2_estim","r2_se")
  
  leTableau = matrix(leTableau,1,length(leTableau))
  colnames(leTableau) = colonne
  
  leTableau = as.data.frame(leTableau)
  
  return(leTableau)
}




#     rangement de tableau de céline



lire.tableau = function()
{
  nom = "C:/Nicodemus/PEDANTIX_LD/AdvLDf.txt"
  tableau = read.table(nom,sep="\t",header=TRUE)
  tableau = tableau[,1:11]
 return(tableau)
}


alakeuleuleu = function(tableau)
{
 # print(dim(tableau))
  j = 2
  sousTableau = cbind(paste(tableau[,1]),tableau[,j])
  tableauSortie = sousTableau
  for(j in 3: ncol(tableau))
  {
   # print(j)
    sousTableau = cbind(paste(tableau[,1]),tableau[,j])
    tableauSortie = rbind(tableauSortie,sousTableau)
  }
#  print(dim(tableauSortie))
  return(tableauSortie)
}




batchDeCeline = function(nbBatch)
{
  repertoire = "C:/Nicodemus/PEDANTIX_LD/"
  fileOUT = paste(repertoire,"tabBatch.csv",sep="")

  for(b in 1:nbBatch)
  {
    cat(b," ")
    flush.console()
    if (b == 1) tabBatch = maDOS.Boucle()
    else  tabBatch = rbind(tabBatch,maDOS.Boucle())
    if(b%%10)
    {
      cat(" [SAVE] \n")
      write.csv(tabBatch,fileOUT)
    }

  }
    
  repertoire = "C:/Nicodemus/PEDANTIX_LD/"
  fileOUT = paste(repertoire,"tabBatch.csv",sep="")
  write.csv(tabBatch,fileOUT)
  
  cat("<-- ",fileOUT,"\n")
  flush.console()
    
  return(tabBatch)
        
}


tab = batchDeCeline(2)