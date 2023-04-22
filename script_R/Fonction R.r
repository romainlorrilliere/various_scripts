			# Fonction R

			
		# Divers

		
# Quitter
q()

# ou
quit()

# Fonction racine de x
sqrt(x)
Aide

#  Ouverture de l’aide HTML
help.start()

#  Demande d’aide sur function (fct)
?fct

# ou
help(fct)

#_____________________________________

		# Entrée & Sortie & Génération de donnée

# Lire un fichier texte dans un data frame
read.table("../nom_fic.txt", header=T ,row.names=1)

			# forme complete
read.table(file, header = FALSE, sep = "", quote = "\"'", dec = ".", row.names, col.names, as.is = FALSE, na.strings = "NA", colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, fill = !blank.lines.skip strip.white = FALSE, blank.lines.skip = TRUE, comment.char = "#", allowEscapes = FALSE)

# Ecrire un data frame dans un fichier texte
write.table(df,file="../nom_fich.txt",row.names=T,col.names=T)

		# forme complete
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double")) 


# Charger une library
library(lib)

# Lire des données disponible dans les library
data(var)

# Créer un verctor de n valeurs comprisent entre ne et nf
runif(n, min= ne, max= nf)

# Créer un verctor de n valeurs selon une répartition normale de moyenne m et d écart type s
rnorm(n, mean= m, sd= s)

# Créer une suite de n1 à n2 avec un pas de s
seq(n1, n2, s)

# Créer une répétition de n fois de a
rep(a,n)

# Créer un factor balancer (effectifs égaux) de n label d effectif nef
gl(n, nef, labels= c("nom_level1","nom_level2","…"))

# Créer un factor groupe à partir de 2 facteur d un data frame
as.factor(paste(df$1, df$2, rep = ""))

# Créer un vector qui possèdent n valeurs uniques comprises dans l intervalle x
sample(x, n)

#_____________________________________

		# Information sur les données


#  Mode (type de donnée)
mode(var) 

#  Longueur vector
length(vect)

#  Dimension de matrix ou data frame
dim(var)

#  Vector ?
is.vector(var)

#  Factor ?
is.factor (var)

# nombre de facteur
length(levels(varFactor))

#  Matrix ?
is.matrix(var)

#  Data frame ?
is.data.frame(var)

# Position des éléments correspondant à un critère logique x au sein d un vector ou d un data framecrabs
which(x, arr.ind = F)


#_____________________________________

		# Manipulation des données et des objets

		
# Lister les objets actifs
ls()

# ou
objects()

# Effacer des objets actifs de la RAM
rm()

rm(list=ls())

#  Transtypage en vector (par colonne)
as.vector(matvec)

vect<-NULL
for (i in 1:dim(ad)[1]){
	vect<-c(vect,ad[,i])
}

# Transtypage en factor
as.factor(vect)

#  Transtypage en matrix (Attention met un vecteur en une colone)
as.matrix(vect)

# Trandtypage en matrix (sur p colonne et n ligne)
matrix(vect,n,p)

#  ou
dim(vect)<-c(n,p)

#  Transtypage en data frame à partir de vector ou matrix
as.data.frame(mat)

#  Collage en lignes de vector, matrix, data frame (var1 et var2)
rbind(var1,var2)

#  Collage en colonne de vector, matrix, data frame (var1 et var2)
cbind(vat1, var2)

#  Formation d’une list
list(nom.var1 = var1, var2, var3, …)

#  Attach data frame
attach(df)

#  Detach data frame
detach(df)

# Concaténer un vector après l avoir convertit un caractère
paste(vect1, vect2, sep=" ")

# Tri d un vector
sort(vect,decreasing=F,index=F)

# Retrouver les élément d un vector ou d un data frame qui correspond à des conditions x
subset(df, x)

# Normalisation des données par centrage-réduction
as.data.frame(scale(df,center=T,scale=T))

# colle 2 chaînes de caratère
paste(ch1, ch2, sep = '')


# transtypage decimal hexadecimal
hexadecimal = sprintf("%x", decimal) 

# transtypage hexadecimal decimal
decimal = as.numeric(paste("0x",as.character(hexadecimal),sep=''))

#_____________________________________

		# Statistique simple

		
# Calcul dune fonction sur un data frame par rapport à des groupes
by(df, groupe, fct)

# ou
aggregate(df,list(groupe),fct)

#  Moyenne dun vector
mean(vect, na.rm = F)

# Moyenne dun matrix
mean(as.data.frame(mat))

# ou (i = 1 pour calcul en ligne et i = 2 pour calcul en colonne)
apply(mat, i, mean)

# ou
colMeans(mat)

# ou
rowMeans(mat)

# moyenne par groupe
mean.tab = data.frame (meanA = mean(tab[tab$name == 'a' ,4:8]), meanB = mean(tab[tab$name == 'b',4:8]), meanC = mean(tab[tab$name == 'c',4:8]))

# ou
sapply(split(dat[paste("x", 1:5, sep = "")], dat$name), mean)

# ou 
aggregate(data[,c(4:8)],list(data$name),mean)

# Quartile, médiane et moyenne dun vector
summary(vect)

# Ecart type
sd(dfnum)

# Covariance
cov(dfnum)

# Corrélation
cor(dfnum)

# ANOVA sur un vector à partir de groupes à partir de groupes pré-définits
summary(aov(vect~groupe))

# Test de Kruskal-Wallis sur les groupes dun vector
kruskal.test(vect,groupe)

# Comparaisons multiples (bibliothèque ctest; A = bonferroni ou holm ou hochberg 
pairwise.t.test(vect,groupe,p.adj="holm")

#_____________________________________

		# Graphiques simples

		
# ouvrir une nouvelle fenêtre graphique
x11()

# Histogramme
hist(vect)

# "Boîtes à moustaches"
boxplot(vect)

# "Boîtes à moustaches" par groupes
boxplot(vect~groupe)


# Graphiques en points

#pairs
pairs(df)

# Nuage de points
plot(vect1, vect2)

# Nuage de points ammélioré : couleur des groupes et légende
plot(vect1, vect2, col = as.numeric(groupe), pch = 20)
legend(x=15,y=20, legend=unique(as.vector(groupe)), col=as.numeric(unique(groupe)), pch=20,cex=0.8)
title("xxxxx", xlab= null, ylab= null, line= NA,cex.main = 1,   font.main= 3, col.main= "blue")

# Nuage de point avec à la place des points la légende
plot(vect1,vect2,pch=300)
text(vect1,vect2,as.vector(groupe))

# Afficher couleur et forme disponible
plot(1:20,rep(1,20),pch=1:20,col=1:20,cex=3)

# diviser la fenêtre graphique
m = matrix(c(1:3,3),2,2)
layout(m)
layout.show(3)

# plot de type ligne sur une matrice avec les séries en colonnes
matplot(x, frame.plot, type='l',
	col="black", lty = c(1,3,3), lwd = c(3,2,2), 
	xlab= "Years", ylab= "Colonies numbers",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
	main="Number of breeding colonies",font.main = 2, cex.main= 2)


# trace une courbe en gras entouré de 2 courbes en pointillé
plotMeanCI = function(tab,ecriture="")
{	
	x = row.names(tab)
	
	if (ecriture =! "")
	{
		fileNameWrite = ecriture # avec le SD
		cat("  <-- ",fileNameWrite,"\n")
		png(file=fileNameWrite)
		matplot(x, tab, type='l',
			col="black", lty = c(1,3,3), lwd = c(3,2,2), 
			xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
			main="HS weighted mean",font.main = 2, cex.main= 2)
		dev.off()
	}
	
	x11() # avec le SD
	matplot(x, tab, type='l',
		col="black", lty = c(1,3,3), lwd = c(3,2,2), 
		xlab= "Years", ylab= "Weighted mean",font.lab = 3, cex.lab = 1.5, cex.axis= 1.2,
		main="HS weighted mean",font.main = 2, cex.main= 2)
}

# les couleurs (démo)
require(graphics)
# A Color Wheel
pie(rep(1,12), col=rainbow(12))

# Les palettes (démo)
demo.pal <-
  function(n, border = if (n<32) "light gray" else NA,
           main = paste("color palettes;  n=",n),
           ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                      "terrain.colors(n)", "topo.colors(n)", "cm.colors(n)"))
{
    nt <- length(ch.col)
    i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
    plot(i,i+d, type="n", yaxt="n", ylab="", main=main)
    for (k in 1:nt) {
        rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
             col = eval(parse(text=ch.col[k])), border = border)
        text(2*j,  k * j +dy/4, ch.col[k])
    }
}
n <- if(.Device == "postscript") 64 else 16
     # Since for screen, larger n may give color allocation problem
demo.pal(n)


#_____________________________________


		# deboguage

traceback()
browser()




#_____________________________________




		# Petits programmes


makeVectMeanFactor = function(tab)
{
	tabSum = rep(0,length(levels(tab[,1])))
	tabInc = rep(0,length(levels(tab[,1])))
	
	j = 1
	n = 1

	tabSum[n] = tab[1,2]
	tabInc[n] = j

	for (i in c(2:dim(tab)[1]))
	{
		if(tab[i,1] == tab[i-1,1])2
		{
			j = j + 1
			
		}
		else
		{
			n = n + 1
			j = 1
		}

		tabSum[n] = tabSum[n] + tab[i,2]
		tabInc[n] = j
	}
	tabMean = tabSum / tabInc
	return(tabMean)
}






makeTempList = function(tab)
{
	
	j = 1
	tempList = list()
	vecMois = as.vector(tab[1,2])

	for (i in c(2:dim(tab)[1]))
	{
		if(tab[i,1] == tab[i-1,1])
		{
			vecMois = c(vecMois,tab[i,2])			
		}
		else
		{
			#print(vecMois)
			if(length(tempList) == 0){tempList = list(vecMois)}
			else{tempList = c(tempList,list(vecMois))}
			#print(tempList)
			vecMois = as.vector(tab[i,2])
			#print("----------------------------")
			print(tab[i,1])
			flush.console()
		}

	}
	#print(vecMois)
	tempList = c(tempList,list(vecMois))
	#print(tempList)
	#print("##################################################")
	#flush.console()
	listTemp = tempList
	save(listTemp,file="listTemp.Rdata")
	return(tempList)
}





###############################################


# permet de rafraichir la console
flush.console()












