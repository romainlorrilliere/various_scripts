sp1 = 30
sp2 = 20
sp3 = 1.1
r1 = 1.5
r2 = 1.4
r3 = 1.8



surPop = 2
k = 200


pop2 = c(sp2)
pop1 = c(sp1)
pop3 = c(sp3)

for( i in 1:50)
{
  sp1 = sp1*r1
  sp2 = sp2*r2
  sp3 = sp3*r3
  surPop = (sp1 + sp2 + sp3)-k
  cat(i," : ", sp1, " et ",sp2," -> ",surPop,"\n")

  if (surPop>0)
  {
    sp1new = sp1 - (sp1 / (sp2 + sp1 + sp3) * surPop)
    sp2new = sp2 - (sp2 / (sp2 + sp1 + sp3) * surPop)
    sp3new = sp3 - (sp3 / (sp3 + sp1 + sp2) * surPop)
    if(sp2new < 0 ) sp2new = 0
    if(sp1new < 0 ) sp1new = 0
    if(sp3new < 0 ) sp3new = 0
    sp1 = sp1new
    sp2 = sp2new
    sp3 = sp3new
  }

#  cat(i," : ", sp1, " et ",sp2  ,"\n")

  pop2 = c(pop2,sp2)
  pop1 = c(pop1,sp1)
  pop3 = c(pop3,sp3)


}


matplot(data.frame(pop1,pop2,pop3,(pop1+pop2+pop3)),type='l',col=c("red","blue","darkgreen","black"),lty=1,lwd=2)
