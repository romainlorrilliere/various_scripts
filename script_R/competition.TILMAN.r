dR_dt = function(d,S,Ri,Nimpact)
return(d*(S-Ri)-Nimpact)

dN_dt = function(Ni,mu,Ri,m)
return(Ni*(mu*Ri-m))

impactNR = function(cNR,mu,R,N)
return(cNR*mu*R*N)

dR_dt_DC = function(d,S,Ri,Nimpact)
return(d*(S-Ri)-Nimpact)

dN_dt_DC = function(Ni,mu,cNR,Ri,m)
return(Ni*(mu*cNR*Ri-m))

impactNR_DC = function(cNR,R,N)
return(cNR*R*N)

N_MuD = function(S=8, c_nr=1,r_init=5,n_init=4,limTemp = 300)
{
  cat("\n ---  N en fonction de Mu et d  ---\n")
  flush.console()
  vecd = seq(from = 0,to = 1.5, by = 0.025)
  vecMU = seq(from = 0,to = 1, by = 0.025)
  vecM = seq(from = 0,to = 1, by = 0.1)

  
  N.tableMU_d = matrix(NA,length(vecMU),length(vecd))
  for(Mi in 1:length(vecM))
  {
    m = vecM[Mi]
    #cat("** ",Mi," - M: ",m,"\n")
    for(di in 1 : length(vecd))
    { 
      d = vecd[di]
      #cat(di," ")
      #if(di %% 10 == 0) cat("\n")
      #flush.console()
      for(MUi in 1:length(vecMU))
      {
        mu = vecMU[MUi]
        r = c(r_init)
        n = c(n_init)
        for(i in 1:limTemp)
        {
          if(n[i] < 0) n[i] = 0
          r = c(r, 
            r[i] + dR_dt_DC(d,S,r[i],impactNR_DC(c_nr,r[i],n[i])))
          n = c(n, 
            n[i] + dN_dt_DC(n[i],mu,c_nr,r[i],m))
        }  
        if (n[limTemp]> 0)
          N.tableMU_d[MUi,di]= n[limTemp]
      }
    }
    x = vecMU
    y = vecd
    tablo = N.tableMU_d
    repertoire = paste("D:/Recherche/Thèse/Tilman/Tilman_DC/",
      "S",S,"r",r_init,"n",n_init,"/",sep="")
    dir.create(repertoire,showWarnings=FALSE)
    fileNameSufix = paste("Mu_d-M",m*10,"S",S,"r",r_init,"n",n_init,sep="")
    fileName = paste(repertoire,fileNameSufix,".png",sep='')
    sousTitre = paste(
      "t:",limTemp," S: ",S," r0: ",r_init," n0: ",n_init,
      "  || mort:",m)
    png(fileName) 
    par(col = "black",bg="gray")
    filled.contour(x, y, tablo, color = heat.colors,
        plot.title = title(main = "N equilibre",
        xlab = "survie/conversion", ylab = "ressource", 
        sub = sousTitre),
        key.title = title(main="N"),
        key.axes = axis(4))# maybe also asp=1 
    dev.off()
    cat("  <-- PNG: ",fileNameSufix,"\n")
    flush.console()
  }
}


N_MuM = function(S=8, c_nr=1,r_init=5,n_init=4,limTemp = 300)
{
  cat("\n ---  N en fonction de Mu et M  ---\n")
  flush.console()
  vecd = seq(from = 0,to = 1.5, by = 0.1)
  vecMU = seq(from = 0,to = 1, by = 0.025)
  vecM = seq(from = 0,to = 1, by = 0.025)
  
  N.tableMU_M = matrix(NA,length(vecMU),length(vecM))
  for(di in 1:length(vecd))
  {
    d = vecd[di]
    #cat("** ",di," - d: ",d,"\n")
    for(Mi in 1 : length(vecM))
    {
      m = vecM[Mi]
     # cat(Mi," ")
      #if(Mi %% 10 == 0) cat("\n")
      #flush.console()
      for(MUi in 1:length(vecMU))
      {
        mu = vecMU[MUi]
        r = c(r_init)
        n = c(n_init)
        for(i in 1:limTemp)
        {
          if(n[i] < 0) n[i] = 0
          r = c(r,
            r[i] + dR_dt_DC(d,S,r[i],impactNR_DC(c_nr,r[i],n[i])))
          n = c(n,
            n[i] + dN_dt_DC(n[i],mu,c_nr,r[i],m))
        }  
        if (n[limTemp]> 0)
          N.tableMU_M[MUi,Mi]= n[limTemp]
      }
    }
    x = vecMU
    y = vecM
    tablo = N.tableMU_M
    repertoire = paste("D:/Recherche/Thèse/Tilman/Tilman_DC/",
      "S",S,"r",r_init,"n",n_init,"/",sep="")
    dir.create(repertoire,showWarnings=FALSE)
    fileNameSufix = paste("Mu_M-d",d*10,"S",S,"r",r_init,"n",n_init,sep="") 
    fileName = paste(repertoire,fileNameSufix,".png",sep='')
    sousTitre = paste(
      "t:",limTemp,"S:",S,"r0:",r_init,"n0:",n_init,
      "|| ressource:",d)
    png(fileName) 
    par(col = "black",bg="gray")
    filled.contour(x, y, tablo, color = heat.colors,
        plot.title = title(main = "N equilibre",
        xlab = "survie/conversion", ylab = "mortalite", 
        sub = sousTitre),
        key.title = title(main="N"),
        key.axes = axis(4))# maybe also asp=1 
    dev.off()
    cat("  <-- PNG: ",fileNameSufix,"\n")
    flush.console()
  }
}

N_R = function(S=8, c_nr=1,r_init=5,n_init=4,limTemp = 50,
                d=1,mu=0.5,m=0.6)
{ 
  #cat("->  d = ",d," mu = ",mu," m = ",m,"\n")
  r = c(r_init)
  n = c(n_init)

  for(i in 1:limTemp)
  {
    if(n[i] < 0) n[i] = 0
    r = c(r,r[i] + dR_dt_DC(d,S,r[i],impactNR_DC(c_nr,r[i],n[i])))
    n = c(n,n[i] + dN_dt_DC(n[i],mu,c_nr,r[i],m))
  }
  tab = rbind(r,n)
  repertoire = paste("D:/Recherche/Thèse/Tilman/Tilman_DC/",
    "S",S,"r",r_init,"n",n_init,"/",sep="")
  dir.create(repertoire,showWarnings=FALSE)
  fileNameSufix = paste("N-R_",d*10,"-",mu*10,"-",m*10,"S",S,"r",r_init,"n",n_init,sep="")
  fileName = paste(repertoire,fileNameSufix,".png",sep='')
  sousTitre1 = "Noir: ressource Rouge: population"
  sousTitre2 = paste(
    "t:",limTemp,"S:",S,"r0:",r_init,"n0:",n_init,
    "|| ressource:",d,"mu:",mu,"mort:",m) 
  png(fileName) 
  m = matrix(c(1,2),2,1)
  layout(m)
  layout.show(2)
  matplot(t(tab),
    type='l',ylim =c(0,max(tab) ),
    main ="R et N en fonction du temps",sub=sousTitre1,
    xlab="Temps",ylab="abondance R et N")
  plot(r,n,
    type="l",
    main="N en fonction de R", sub= sousTitre2,
    xlab="Ressource", ylab="Population")  
  dev.off()
  cat("  <-- PNG: ",fileNameSufix,"\n")
  flush.console()

}


rVal = 1
sVal = 4
nVal = 2
tempVal = 50

for(sVal in c(8))
{
  cat(":: sVal:",sVal,"\n")
  for(nVal in 1:6)
  {
    cat("  == nVal:",nVal,"\n")
    for(rVal in 1:6)
    {
      cat("   ## rVal:",rVal,"\n")
      flush.console()
      N_MuD(S=sVal, r_init=rVal, n_init=nVal,limTemp=tempVal)
      N_MuM(S=sVal, r_init=rVal, n_init=nVal,limTemp=tempVal)
      MU = seq(from = 0,to = 1, by = 0.1)
      cat("\n ---  N en fonction de R  ---\n")
      for(i in 1:length(MU))
        N_R(mu=MU[i],S=sVal, r_init=rVal, n_init=nVal,limTemp=tempVal)
    }
  }
} 
