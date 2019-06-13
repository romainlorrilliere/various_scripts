#Nul=2394.0
#Residual = 2078.9
#
#(Nul - Residual)/ Nul
#

readHS = function()
{
	hs = read.table(
    "D:/Programmes/VisualWorks_7.6/cormas/Models/Heron/maps/archive/hs.txt")
	hs = as.vector(t(hs))
	return(hs)
}


hsMean = function()
{
 vec = readHS()
 vec = as.numeric(paste(vec))
# print(length(vec))
 #browser()
 vec2 = vec[which(vec>0)]
# print(length(vec2))
 return(mean(vec2))
 
}


hsSummary = function()
{
  vec = readHS()
  vec = as.numeric(paste(vec))
  vec2 = vec[which(vec>0)]
  return(summary(vec2))
}


modelgraph = function(nbCol,hsVal0,hsVal1,nbcol2)
{
  HS0 = hsVal0
  HS1 = hsVal1
#  cat(HS0,' ',HS0	* (0.0418210)+ HS1	* (-0.0370696)," + ")
  NbCol0 = nbCol
  NbCol1 = NbCol2 = NbCol3 = NbCol4 = NbCol5 = NbCol10 = nbcol2
  C8 = HS0
  D8 = HS1
  E8 =  NbCol0
  F8 =  NbCol1
  G8 =  NbCol2
  H8 =  NbCol3
  I8 =  NbCol4
  J8 =  NbCol5
  K8 =  NbCol10
  
  newCol = 0.0418210*C8-
    0.0370696*D8+
    0.8260866*E8+
    0.5738469*F8+
    0.5146434*G8+
    0.4360253*H8+
    0.3960887*(I8)-
    0.0184516*(E8+F8+G8+H8+I8+J8)^2-
    0.0007023*K8^2-
    5.3641104
  return(newCol)
}


graph1 = function()
{
  hsSum = hsSummary()
  nbCol2 = 0
  tabCol = as.data.frame(matrix(NA,51,7))
  tabCol[,1] = 0:50
  for (hs in 1 : 6)
  {
  #  print(hs)
    for (i in 0:50)
    {
  #  print(i)
      tabCol[i+1,hs+1] = modelgraph(i,hsSum[hs],hsSum[3],nbCol2)
    }
  }
  for (j in 2:7) tabCol[,j] = ifelse(tabCol[,j]<0,NA,tabCol[,j])
  nameSum = c("Min","1st quartile","Median","Mean","3rd quartile","Max")
  colnames(tabCol)[2:7] = nameSum
  tab = tabCol
  variable = expression(HS[0])
  axe = TRUE
  require(graphics)
	palette()               # obtain the current palette
  #texteLegende = paste(variable,"=",colnames(tab)[2:7])
  texteLegende = colnames(tab)[2:7]
#browser()
  gris1 = 0
  gris2 = 0.8
  grisCouleur = seq(from = gris1, to = gris2, by= ((gris2 - gris1)/length(texteLegende)))#(0:length(texteLegende)+1)/(length(texteLegende)+1)
  palette(gray(grisCouleur))
  matplot(tab[,1],tab[,2:7],col=1:length(texteLegende),lty = 1,
    ann = FALSE,axes = axe,type='l',xlab="nbCol",ylab="log(newCol)",ylim = c(0,5.5))
	legend("topright", texteLegende, pch=20,col=1:length(texteLegende),bty="n")
	mtext(expression(log(NewC[0])),
    side = 2, las = 3, line = 3, adj = 1.3)
  mtext(expression(C[0]),
    side = 1, las = 1, line = 2, adj = 0.5)
  mtext("(a)",
    side = 3, las = 1, line = 12.2, adj = 0.05)
  mtext(expression(C[i]),
    side = 3, las = 1, line = 12.2, adj = 0.83)
  mtext("=",
    side = 3, las = 1, line = 12.2, adj = 0.86)
  mtext("for i = 1 to 10",
    side = 3, las = 1, line = 11, adj = 0.85)


 # palette("default")

#plot(tabCol[,1],tabCol[,],type='l',xlab="nbCol",ylab="log(newCol)")
#edit(tabCol)
}

graph2 = function()
{
hsSum = hsSummary()
nbCol2 = c(0,0.5,0.75,1,1.25,1.5)
tabCol = as.data.frame(matrix(NA,51,7))
tabCol[,1] = 0:50
for (n in 1 : 6)
{
#  print(n)
  for (i in 0:50)
  {
  #print(i)
    tabCol[i+1,n+1] = modelgraph(i,hsSum[3],hsSum[3],nbCol2[n])
  }
#  print(tabCol[,n+1])
 #   flush.console()
}

for (j in 2:7) tabCol[,j] = ifelse(tabCol[,j]<0,NA,tabCol[,j])
colnames(tabCol)[2:7] = paste(nbCol2)

    tab = tabCol
    variable = expression(C[paste("i in 1 to 10")])
    axe = FALSE
    require(graphics)
		palette()               # obtain the current palette
    #texteLegende = paste(variable,"=",colnames(tab)[2:7])
    texteLegende = colnames(tab)[2:7]
#browser()
  gris1 = 0
  gris2 = 0.8
  grisCouleur = seq(from = gris1, to = gris2, by= ((gris2 - gris1)/length(texteLegende)))#(0:length(texteLegende)+1)/(length(texteLegende)+1)
    palette(gray(grisCouleur))
    matplot(tab[,1],tab[,2:7],col=1:length(texteLegende),lty = 1,
      ann = FALSE,axes = axe,type='l'#,xlab=expression(C[0]),ylab=paste("log(",expression(NewC[0]),")",sep='')
      )
		legend("topright", texteLegende, pch=20,col=1:length(texteLegende),bty="n")
    #palette("default")
    axis(2)
    mtext("(b)", side = 1, las = 1, line = 1, adj = 0.05)
    mtext(expression(HS[0]), 
      side = 1, las = 1, line = 0.5, adj = 0.75)
    mtext("=", side = 1, las = 1, line = 0.5, adj = 0.78)
    #mtext(expression(NewC[0]), side = 2, las = 3, line = 3, adj = 0.5)
    box()



#edit(tabCol)
#plot(tabCol[,1],tabCol[,],type='l',xlab="nbCol",ylab="log(newCol)")
# edit(tabCol)
}


leGraph = function(tab,variable,axe)
{

}


toutou = function()
{
  plot.new()
	m = matrix(c(2,1),2,1)
	layout(m)
	layout.show(2)
  par(mar=c(5,4,0,2))
  graph1()
  par(mar = c(0,4,4,2))
  graph2()
  fichierFigure = "D:/Recherche/Héron/PubliHéron/figures/appendix1" 
  savePlot(fichierFigure,type="png")
  savePlot(fichierFigure,type="eps")
 


}



plot.N_dNdT = function()
{
  tab = read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt")
  #edit(tab)
  
  i = 1 
  new.ligne = c(tab$moyenne[i],(tab$moyenne[i+1]-tab$moyenne[i])/4)
  new.tab = new.ligne
  
  for(i in 2:nrow(tab)-1)
  {
     new.ligne = c(tab$moyenne[i],(tab$moyenne[i+1]-tab$moyenne[i])/4)
     new.tab = rbind(new.tab,new.ligne)
  }
  
  colnames(new.tab) = c("N","dN/dt")
  
  plot(new.tab[,1],new.tab[,2],xlab=colnames(new.tab)[1],ylab = colnames(new.tab)[2])
}

fit.nbCol = function()
{
 tab = read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt")
 temps = 1:nrow(tab) * 4 - 4
 attach(tab)
 e.moy = exp(-temps)
 

}

   #tab = read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt")
#   temps = 1:nrow(tab) * 4 - 4
#   attach(tab)
#   e.moy = exp(-temps)
#   
#  dataCol=read.table("D:/Recherche/Héron/PubliHéron/figures/tabMeanCI.txt",header=TRUE)
#  #colnames(dataCol)[1] = "annee"
#  attach(dataCol)
#  
#  para0.st = getInitial(moyenne ~ SSlogis(annee, alpha, betta, gama),data = dataCol)
#  
#  para1.st <- list(alpha=1483.797010,betta=4.484858,gama=2.284589)
#  para1.st
#  attach(para0.st)
#  fit0 <- nls(moyenne~alpha/(1+exp(betta-gama*annee)),
#    start=para0.st,
#      data = dataCol,
#    trace=T)
#    detach(dataCol)

toutou()