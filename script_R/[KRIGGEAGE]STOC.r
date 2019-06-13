####################################################
#   Krigeage STOC
####################################################

library(geoR)
library(gstat)
library(ade4)
library(spatstat)
library(spdep)


repertoire= "D:/Recherche/R/Greg/"
fichier = paste(repertoire,"r_explicatives.txt",sep="")
expli = read.table(fichier,sep=",",header=T)

fichier = paste(repertoire,"r_obs_resid.txt",sep="")
obs = read.table(fichier,sep="\t")
colnames(obs) = c("nom","eff","x","y","carre","anthro","bois","eau","agric","rural","frag")

especes = unique(obs$nom)
sp = especes[1]

obsSp = obs[which(obs[,1]==sp),]
obs.geoR.sp = as.geodata(obsSp,coords.col=4:5,data.col=2,covar.col=6:11)
varioSp = variog(obs.geoR.sp,trend=~obsSp$anthro+ obsSp$bois+obsSp$eau + obsSp$agric+ obsSp$rural + obsSp$frag)
eyefit(varioSp)


## faire la grille pour ZFD
range(obs$x)
range(obs$y)
max(obs$y)
min(obs$y)
edit(obs[which(obs$y == 0),])
grd <- GridTopology(cellcentre.offset=c(198,200), cellsize=c(1,1), cells.dim=c(46,30))
sp.grd <- as.SpatialPolygons.GridTopology(grd)
spdf.grd <- SpatialPolygonsDataFrame(sp.grd, df)
xy <- coordinates(spdf.grd)







nb.grd <- dnearneigh(xy, 0, 2)
 #help(sp.correlogram)
 #sp.correlogram(nb.grd, df[,2], 15, zero.policy = T)
 corr <- sp.correlogram(nb.grd, df[,3], 15, zero.policy = T)
 plot(corr)




