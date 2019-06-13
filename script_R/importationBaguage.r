####################################################
#  importation des donnees de baguage dans la base
####################################################

library(RODBC)

repertoire = "D:/Nature/Baguage"
nomFichier = "bordereau2008.xls"
chemin = paste(repertoire,nomFichier,sep="/")
bordereau <- odbcConnectExcel(chemin)

sqlFetch(bordereau, "0") 
data.o = as.data.frame(o)

close(bordereau)





channel <- odbcConnect("test")
sqlSave(channel, USArrests, rownames = "state", addPK=TRUE)
sqlFetch(channel, "USArrests", rownames = "state") # get the lot
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 222
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state", max = 5)
sqlDrop(channel, "USArrests") 
close(channel)
