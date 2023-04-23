moy = seq(from=0, to= 1, by = 0.01) 
vari = seq(from=0.01, to= 0.25, by = 0.01) 
i = 1
alpha=moy*((moy*(1-moy)/vari[i])-1)
betta=(1-moy)*((moy*(1-moy)/vari[i])-1)
alpha.table = alpha
betta.table = betta


for (i in 2:length(vari))
{
alpha=moy*((moy*(1-moy)/vari[i])-1)
betta=(1-moy)*((moy*(1-moy)/vari[i])-1)
alpha.table = rbind(alpha.table,alpha)
betta.table = rbind(betta.table,betta)
}

rownames(alpha.table) = paste(vari)
colnames(alpha.table) = paste(moy)

rownames(betta.table) = paste(vari)
colnames(betta.table) = paste(moy)

write.table(alpha.table,"D:/Recherche/Thèse/alpha.txt")
write.table(betta.table,"D:/Recherche/Thèse/beta.txt")

for(i in 1:nrow(alpha.table))
  alpha.table[i,] = ifelse(alpha.table[i,]<=0,NA,alpha.table[i,])
  
for(i in 1:nrow(betta.table))
  betta.table[i,] = ifelse(betta.table[i,]<=0,NA,betta.table[i,])

write.table(alpha.table,"D:/Recherche/Thèse/alphaNA.txt")
write.table(betta.table,"D:/Recherche/Thèse/betaNA.txt")

tablo = alpha.table + betta.table

write.table(tablo,"D:/Recherche/Thèse/alphaBetaValide.txt")

x <-  moy 
y <-  vari
#  plot.new()
#	m = matrix(c(1,2),2,1)
#	layout(m)
#	layout.show(2)
#
require(graphics)
#
#filled.contour(x, y, t(alpha.table), color = heat.colors,
#    plot.title = title(main = "alpha",
#    xlab = "Moyenne", ylab = "Variance"),
#    plot.axes = { axis(1, moy )
#                  axis(2, vari )},
#    key.title = title(main="alpha"),
#    key.axes = axis(4))# maybe also asp=1
#x11()
#filled.contour(x, y, t(betta.table), color = heat.colors,
#    plot.title = title(main = "beta",
#    xlab = "Moyenne", ylab = "Variance"),
#    plot.axes = { axis(1, moy )
#                  axis(2, vari)},
#    key.title = title(main="beta"),
#    key.axes = axis(4))# maybe also asp=1
#    
#x11()
filled.contour(x, y, t(tablo), color = heat.colors,
    plot.title = title(main = "alpha + beta valide",
    xlab = "Moyenne", ylab = "Variance"),
    plot.axes = { axis(1, moy )
                  axis(2, vari)},
    key.title = title(main="alpha+beta"),
    key.axes = axis(4))# maybe also asp=1




