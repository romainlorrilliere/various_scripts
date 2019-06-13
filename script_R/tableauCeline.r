#     rangement de tableau de céline



lire.tableau = function()
{
  nom = "D:/Recherche/R/Céline/AdvLDf.txt"
  tableau = read.table(nom,sep="\t",header=TRUE)
  tableau = tableau[,1:11]
 return(tableau)
}


alakeuleuleu = function(tableau)
{
  print(dim(tableau))
  j = 2
  sousTableau = cbind(paste(tableau[,1]),tableau[,j])
  tableauSortie = sousTableau
  for(j in 3: ncol(tableau))
  {
    print(j)
    sousTableau = cbind(paste(tableau[,1]),tableau[,j])
    tableauSortie = rbind(tableauSortie,sousTableau)
  }
  print(dim(tableauSortie))
  return(tableauSortie)
}


#laTable = lire.tableau()
#laBelleTable = alakeuleuleu(laTable)

#colnames(laBelleTable) = c("id","ld")
#write.table(laBelleTable,na='NA',"D:/Recherche/R/Céline/AdvLDfalakeuleuleu.asd",sep="\t",row.names=FALSE,quote=FALSE)


incrementTableauCeline = function()
{
  nom = "D:/Recherche/R/Céline/LDf-sorted.csv"
  tableau = read.csv(nom)
  incIndiv = rep(0,nrow(tableau))
  incIndiv[1] = 1
  for(i in 2:nrow(tableau))
  {

      if (tableau$Band[i]==tableau$Band[i-1])
        incIndiv[i] = incIndiv[i-1] + 1
      else incIndiv[i] = 1
  }
browser()
  tableauInc = cbind(tableau[,1],cbind(incIndiv,tableau[,2:ncol(tableau)]))
  colnames(tableauInc) = c(colnames(tableauInc)[1],"increment",colnames(tableauInc)[2:ncol(tableau)])
  write.table(tableauInc,na='NA',"D:/Recherche/R/Céline/LDf-sorted-increment.txt",sep="\t",row.names=FALSE,quote=FALSE)
  return("Youpi")
}


incrementTableauCeline()