############################################################################
# utilisation de R pour attaquer directement une base de données ACCESS
###########################################################################

# note : Access c'est tout de même pas terrible ;-)

library(RODBC)

# Access
db <- odbcConnectAccess("test.mdb", uid="ripley")
commande_SQL = paste("SELECT ... WHERE... ",sep="")
cat("sql: ",commande_SQL,"\n")
  flush.console()
#browser()
  tab = sqlQuery(db, commande_SQL)
 
  
  
odbcCloseAll()




