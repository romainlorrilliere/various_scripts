
listEspeces = c("ALAARV","ALERUF","ANTPRA","BUTBUT","CORFRU","EMBCIR","EMBCIT","FALTIN","LANCOL","LULARB","MILCAL","MOTFLA","PERPER","PHACOL","SAXRUB","SYLCOM","UPUEPO")

tableau = read.table(....,header = TRUE)

tableauSansEsp1 = tableau[,1:4] #on séléctione les première colonnes 
tableauSansEsp2 = tableau[,(ncol(tableauSansEsp1)+length(listEspeces)+1):ncol(tableau)] #on séléctione les dernières colonnes
for(espece in listEspeces)
{
  tableauEspece = cbind(tableauSansEsp1,espece,tableau[,which([colnames(tableau)==espece])],tableauSansEsp2) #on colle en colonne le 1er tableau, puis une colonne du nom de l'espèces puis la colonne qui correspond à l'abondance de l'espece enfin le dernier tableau
  if(espece == listEspece[1]) nouveauTableau = tableauEspece  #on initialise le tableau au premier tour
  else nouveauTableau = rbind(nouveauTableau,tableauEspece) # on met les tableau les un sous les autres
}
colnames(nouveauTableau) = c(colnames(tableauSansEsp1),"NOM_ESPECE","ABONDANCE", colnames(tableauSansEsp1))