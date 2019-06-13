
####	Packages à installer et à charger	####
library(ade4)
library(pixmap)
library(adehabitat)
library(spatstat)
library(maptools)
library(shapefiles)
library(rgdal)
library(proj4)
library(spdep)
library(MASS)


lezE <- read.table("reproZDEb.txt", head = T, dec =",", na.strings = "na")
lez <- read.table("reproZFD1.txt", head = T, dec =",", na.strings = "na")
lezF <- read.table("reproZFD1.txt", head = T, dec =",")

#pour ZDE
range(lezE[,3])
range(lezE[,4])
grd <- GridTopology(cellcentre.offset=c(183,202), cellsize=c(1,1), cells.dim=c(45,19))

## faire la grille pour ZFD
range(lez[,3])
range(lez[,4])
grd <- GridTopology(cellcentre.offset=c(198,200), cellsize=c(1,1), cells.dim=c(46,30))
sp.grd <- as.SpatialPolygons.GridTopology(grd)
coord <- lez[,2:4]
coord<- apply(coord, 2, trunc)
coord <- apply(coord, 2, as.numeric)
coord.a=split(coord, coord[,1])
coord=coord.a
lez.a <- split(lez, lez[,2])
names(coord) <- c("x", "y")
coord.grd <- coordinates(grd)
coord.grd <- apply(coord.grd, 2, as.numeric)


## mettre en forme la variable fécondation sur l'ensemble des années
df <- matrix(0, nrow = nrow(coord.grd), ncol = length(lez.a))
row.names(df) <- getSpPPolygonsIDSlots(sp.grd)
df <- as.data.frame(df)
names(df) <- names(lez.a)

for(a in 1:16){
tab <- lez.a[[a]]
z <- tab$"F"
z <- as.numeric(z)
print(paste(sum(is.na(z)), "valeurs manquantes sur", length(z), "valeurs"))
index <- is.na(z)
coord <- tab[,3:4]
names(coord) <- c("x", "y")
coord <- apply(coord, 2, trunc)
coord <- apply(coord, 2, as.numeric)

if(sum(index) > 0){
z <- na.omit(z)
coord <- coord[-which(index),]
}


res <- as.list(rep(0, nrow(coord.grd)))

for(i in 1:nrow(coord)){
	xy <- coord[i,]
	mat <- rbind(xy, coord.grd)
	ind <- duplicated(mat)	
	ind <- which(ind == TRUE) - 1
	res[[ind]] <- c(res[[ind]], z[i])
}

index <- lapply(res, function(x) length(x) > 1)
index <- unlist(index)
index <- which(index)
for(i in index)
	res[[i]] <- res[[i]][-1]
z.mean <- lapply(res, mean)
df[,a] <- unlist(z.mean)
print(a)
}

#corrléogramme Pauline 02102008
spdf.grd <- SpatialPolygonsDataFrame(sp.grd, df)
xy <- coordinates(spdf.grd)
nb.grd <- dnearneigh(xy, 0, 2)
 #help(sp.correlogram)
 #sp.correlogram(nb.grd, df[,2], 15, zero.policy = T)
 corr <- sp.correlogram(nb.grd, df[,3], 15, zero.policy = T)
 plot(corr)


# calcul de l'indice de Moran sur la grille
xy <- coordinates(spdf.grd)
nb.grd <- dnearneigh(xy, 0, 2)
nb.grd <- dnearneigh(xy, 0, 5)	# année 1996 particulière?
listw.grd <- nb2listw(nb.grd)
neig.grd <- nb2neig(nb.grd)
mat.grd <- listw2mat(listw.grd)
gearymoran(mat.grd, df, alter = "greater")


# multispati sur la grille
pca.grd <- dudi.pca(df)
ms.grd <- multispati(pca.grd, listw.grd)
z <- ms.grd$li[,1]
res <- df
res <- cbind(res, z)
res <- SpatialPixelsDataFrame(coordinates(sp.grd), res)
image(res, "z", col = grey((1:30)/30))
u <- bbox(res)
rect(u[1,1],u[2,1],u[1,2],u[2,2])
contour(res, "z", add = T, nlevels = 4)

# calcul de l'indice deMoran sur les points
"fun" <- function(tab){
coord <- tab[,c(3:4)]
coord <- tab[,3:4]
names(coord) <- c("x", "y")
coord <- apply(coord, 2, trunc)
coord <- apply(coord, 2, as.numeric)
z <- tab$"F"
z <- as.numeric(z)
print(paste(sum(is.na(z)), "valeurs manquantes sur", length(z), "valeurs"))
index <- is.na(z)
if(sum(index) > 0){
z <- na.omit(z)
coord <- coord[-which(index),]
}
res <- apply(coord, 1, function(xy){
	index <- rbind(xy, coord.grd)
	index <- which(duplicated(index)) - 1
	res <- spsample(sp.grd[index,], 1, "random")
	res <- coordinates(res)
	return(res)
})
res <- t(res)
names(res) <- names(coord)
res <- cbind(res, z)
return(res)
}

res <- lez.a
res <- lapply(res, fun)
lez.pp <- res

for(i in 1:length(lez.pp)){
xy <- res[[i]][,1:2]
nb.grd <- dnearneigh(xy, 0, 2)	# on prend 5 mètre ca c'est le territoire d'un lézard
listw.grd <- nb2listw(nb.grd, style = "W", zero.policy = T)
neig.grd <- nb2neig(nb.grd)
mat.grd <- listw2mat(listw.grd)
print(gearymoran(mat.grd, res[[i]][,3], alter = "greater"))
}
# ----> A la suite de ce test on ne trouve que 2ou 3années sign (1998,1999)...

# ----> forte structuration spatiale de la marque
i <- 10
"fun" <- function(i){
image(spixdf.grd, i, col = grey((30:1)/30))
u <- bbox(spixdf.grd)
rect(u[1,1],u[2,1],u[1,2],u[2,2])
nb.grd <- dnearneigh(res[[i]][,1:2], 0, 10)	# on prend 5 mètre ca c'est le territoire d'un lézard
neig.grd <- nb2neig(nb.grd)
plot(nb.grd, coords = res[[i]][,1:2], add = T, pch = 20)
s.value(res[[i]][,1:2], scalewt(res[[i]][,3]), neig = neig.grd, add.plot = T, cleg = 0)
}
fun(i)
par(mfrow = c(2,2))
fun(2)
fun(10)
fun(14)
fun(16)

# ----> structuration du support mais absence de structure pourla marque
fun(13)	# structuration local : très complexe
i <- 10
xy <- coordinates(spdf.grd)
nb.grd <- dnearneigh(xy, 0.5, 5)	
listw.grd <- nb2listw(nb.grd, zero.policy = TRUE)
neig.grd <- nb2neig(nb.grd)
mat.grd <- listw2mat(listw.grd)
gearymoran(mat.grd, df[sample(1:855, 855),10], alter = "greater")

#travail sur toutes les années ensemble
 res <- apply(res, 2, as.numeric)
 xy <- res[,1:2]
 nb.grd <- dnearneigh(xy, 0, 2)# on prend 5 mètre ca c'est le territoire d'un lézard
 listw.grd <- nb2listw(nb.grd, style = "W", zero.policy = T)
 neig.grd <- nb2neig(nb.grd)
 mat.grd <- listw2mat(listw.grd)
 plot(nb.ord)
 plot(nb.grd, coords = res[[i]][,1:2], add = T, pch = 20)
 plot(nb.grd, coords = res[,1:2], pch = 20)
 s.value(res[[i]][,1:2], scalewt(res[[i]][,3]), neig = neig.grd, add.plot = T, cleg = 0)
 s.value(res[,1:2], scalewt(res[,3]), neig = neig.grd, add.plot = T, cleg = 0, csi = 0.3)
 args(moran.test)
 moran.test(res[,3], listw.grd)
 moran.test(res[,3], listw.grd, zero.policy = T)
 s.value(res[,1:2], scalewt(res[,3]), cleg = 0, csi = 0.3)
 plot(nb.grd, coords = res[,1:2], pch = 20, type="n")
 s.value(res[,1:2], scalewt(res[,3]), neig = neig.grd, add.plot = T, cleg = 0, csi = 0.3)
 plot(nb.grd, coords = res[,1:2], pch = 20, type="n")
 s.value(res[,1:2], scalewt(res[,3]), add.plot = T, cleg = 0, csi = 0.3)
 mean(res[,3])




# travailler que sur les valeurs positives mais sur toutes les années en même temps
coord <- lez[,3:4]
names(coord) <- c("x", "y")
coord <- apply(coord, 2, trunc)
coord <- apply(coord, 2, as.numeric)
coord <- coord[-which(is.na(lez$"F")),]
coord.pp <- unique(coord)

df.pp <- matrix(0, nrow = nrow(coord.pp), ncol = length(lez.a))
row.names(df.pp) <- row.names(coord.pp)
df.pp <- as.data.frame(df.pp)
names(df.pp) <- names(lez.a)


for(a in 1:16){
tab <- lez.a[[a]]
z <- tab$"F"
z <- as.numeric(z)
print(paste(sum(is.na(z)), "valeurs manquantes sur", length(z), "valeurs"))
index <- is.na(z)
coord <- tab[,3:4]
names(coord) <- c("x", "y")
coord <- apply(coord, 2, trunc)
coord <- apply(coord, 2, as.numeric)
if(sum(index) > 0){
z <- na.omit(z)
coord <- coord[-which(index),]
}

res <- as.list(rep(0, nrow(coord.pp)))

for(i in 1:nrow(coord)){
	xy <- coord[i,]
	mat <- rbind(xy, coord.pp)
	index <- duplicated(mat)	
	index <- which(index == TRUE) - 1
	res[[index]] <- c(res[[index]], z[i])
}

index <- lapply(res, function(x) length(x) > 1)
index <- unlist(index)
index <- which(index)
for(i in index)
	res[[i]] <- res[[i]][-1]
z.mean <- lapply(res, mean)
df.pp[,a] <- unlist(z.mean)
print(a)
}

z <- ms.grd$li[,1]
res <- df
res <- cbind(res, z)
res <- SpatialPixelsDataFrame(coordinates(sp.grd), res)
image(res, "z", col = grey((1:30)/30))
u <- bbox(res)
rect(u[1,1],u[2,1],u[1,2],u[2,2])
contour(res, "z", add = T, nlevels = 4)
points(coord.grd, pch = 20, cex = 0.2)
points(coord.pp, pch = 20, col = "red")
index <- apply(df.pp,1, function(x) sum(x == 0)) < 14
points(coord.pp[which(index),], pch = 20, col = "purple")
index <- apply(df.pp,1, function(x) sum(x == 0)) < 13
points(coord.pp[which(index),], pch = 20, col = "yellow")


nb.pp <- dnearneigh(coord.pp, 0.5, 5)	
listw.pp <- nb2listw(nb.pp, zero.policy = TRUE)
neig.pp <- nb2neig(nb.pp)
mat.pp <- listw2mat(listw.pp)
gearymoran(mat.pp, df.pp, alter = "greater")

# multispati sur la grille
pca.pp <- dudi.pca(df.pp)
ms.pp <- multispati(pca.pp, listw.pp)
z <- ms.pp$li[,1]
s.value(coord.pp, z, add.plot = T)	# on se rapproche mais c'est pas terrible


# on peut essayer de regrouper les polygones
fac <- as.factor(cutree(hc, h = 3))
sp2.grd <- unionSpatialPolygons(as(spdf.grd, "SpatialPolygons"), fac)
coord.sp2 <- coordinates(sp2.grd)

# ----> calcul sur la grille
df.gr2 <- matrix(0, nrow = nrow(coord.sp2), ncol = length(lez.a))
row.names(df.gr2) <- getSpPPolygonsIDSlots(sp2.grd)
df.gr2 <- as.data.frame(df.gr2)
names(df.gr2) <- names(lez.a)


for(a in 1:16){
tab <- lez.a[[a]]
z <- tab$"F"
z <- as.numeric(z)
print(paste(sum(is.na(z)), "valeurs manquantes sur", length(z), "valeurs"))
index <- is.na(z)
coord <- tab[,3:4]
names(coord) <- c("x", "y")
coord <- apply(coord, 2, trunc)
coord <- apply(coord, 2, as.numeric)
if(sum(index) > 0){
z <- na.omit(z)
coord <- coord[-which(index),]
}

res <- as.list(rep(0, nrow(coord.sp2)))

for(i in 1:nrow(coord)){
	xy <- coord[i,]
	mat <- rbind(xy, coord.grd)
	index <- duplicated(mat)	
	index <- which(index == TRUE) - 1
	res[[fac[index]]] <- c(res[[fac[index]]], z[i])
}

index <- lapply(res, function(x) length(x) > 1)
index <- unlist(index)
index <- which(index)
for(i in index)
	res[[i]] <- res[[i]][-1]
z.mean <- lapply(res, mean)
df.gr2[,a] <- unlist(z.mean)
print(a)
}


# ----> calcul sur les points



# utiliser l'article de Stéphane Dray pour coupler 2 support différents / généraliser à n sous forme de généralisation du multispati

