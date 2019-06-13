 # arbre regression
local({pkg <- select.list(sort(.packages(all.available = TRUE)))
 if(nchar(pkg)) library(pkg, character.only=TRUE)})

 results = read.table("C:/Documents and Settings/Romain L/Bureau/vincent/multivariate/results.txt",col.names=TRUE,row.names=FALSE)
 attach(results)
 

arbre = rpart(tot1~r1+r2+r3+gain+feff+adap+comp,method="anova")
arbre
summary(arbre)
plot(arbre)
text(arbre,use.n = TRUE,cex=0.6)
hist(tot1)
summary(tot1)
plot(tot1,r1)

 
 



