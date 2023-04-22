require(graphics)
par(bg="white")
repertoire = "D:/Recherche/Donges/"
fichier = paste(repertoire,"capturesMonde.csv",sep="")
d = read.csv(fichier)
m = map(xlim=c(-20,30),ylim=c(10,70),fill = TRUE, col = 1:10)
couleur = rep("lightgray",length(m$names))

for(p in 1:nrow(d))
{
  couleur[which(m$names==d$Pays[p])] = heat.colors(max(d$nb))[-1*(d$nb[p])+ max(d$nb)+1]
}
titre = "Distribution Européen des oiseaux \ncontrolés hors du site de Donges"
m = map(xlim=c(-20,30),ylim=c(10,70),fill = TRUE, col = couleur,bg="lightblue")
map.axes()
mtext(titre,side=3,col ="black",las = 1, line = 1, adj = 0.5,cex = 1.2)
mtext("Latitude",side = 2, col ="black",las = 3, line = 2, adj = 0.5)
mtext("Longitude",side = 1, col = "black", las = 1, line = 2, adj = 0.5)

fichierOut = paste(repertoire,"carteEurope")
savePlot(fichierOut,"png")




