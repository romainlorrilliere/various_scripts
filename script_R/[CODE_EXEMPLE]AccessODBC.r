############################################################################
# utilisation de R pour attaquer directement une base de donn�es ACCESS
###########################################################################

# note : Access c'est tout de m�me pas terrible ;-)

library(RODBC)

# Access
db <- odbcConnectAccess("test.mdb", uid="ripley")
commande_SQL = paste("SELECT ... WHERE... ",sep="")
cat("sql: ",commande_SQL,"\n")
  flush.console()
#browser()
  tab = sqlQuery(db, commande_SQL)
 
  
  
odbcCloseAll()




