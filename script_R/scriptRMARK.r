# Analyse R-MARK
# Données Donges



scriptMark = function(especes,annees)
{  
  for(sp in especes)
  {
    cat("\n == ",sp," ==\n")
    for(an in annees)
    { 
      cat("\n .... ",an," ....\n")
      # Location of Mark.exe file
      
      MarkPath="C:/Program Files/MARK/"
      fichier = paste("C:/DataAggeliki/etude/R-MARK/",sp,"/",an,"/",sp,an,".txt",sep="")
      repOUTcourt =  paste("C:/DataAggeliki/etude/R-MARK/",sp,"/",an,"/",sep="")
      repOUT = paste(repOUTcourt,an,sp,"_",sep="")
      
      
      d = import.chdata(fichier, field.types="f")    
      
      
      ####Models CJS
     
      modCJS.process = process.data(d,model="CJS")
      
      modCJS.ddl = make.design.data(modCJS.process)
      
      # CJS(model 1)
      fichierOUT = paste(repOUT,"CJS",sep="")
      modCJS=mark(modCJS.process,modCJS.ddl,model.parameters=list(Phi=list(formula=~time),p=list(formula=~time)),output=FALSE,filename=fichierOUT)
      
       
      #Constant model (model 2)
      fichierOUT = paste(repOUT,"constant",sep="")
      phi.dot = list(formula=~1)
      p.dot = list(formula=~1)
      mod.constant = mark(modCJS.process,modCJS.ddl,model.parameters=list(Phi=phi.dot,p=p.dot),output=FALSE,filename=fichierOUT)
      
      
      #Constant phi model (model 3)
      fichierOUT = paste(repOUT,"phi_constant",sep="")
      mod.phi_constant = mark(modCJS.process,modCJS.ddl,model.parameters=list(Phi=list(formula=~1),p=list(formula=~time)),output=FALSE,filename=fichierOUT)
      
      
      #Constant p model (model 4)
      fichierOUT = paste(repOUT,"p_consant",sep="")
      mod.p_constant = mark(modCJS.process,modCJS.ddl,model.parameters=list(Phi=list(formula=~time),p=list(formula=~1)),output=FALSE,filename=fichierOUT)
      
      
      #Compare models
      cat("\n *****************\n Compare models CJS \n**********************\n")
      cjs.results = collect.models(type="CJS")
      bachibouzouk = as.data.frame(cjs.results$model.table)
      write.csv(bachibouzouk,paste(repOUT,"summaryCJS.csv"))
 
      cat("\n")
      
      ####Models Pradel
      
     
      fichierOUT = paste(repOUT,"modPradel",sep="")
      modPradel.process = process.data(d,model="Pradel")
      
      # CJS(model 1)
      modPradel.ddl = make.design.data(modPradel.process)
      modPradel = mark(modPradel.process, modPradel.ddl,model.parameters=list(Gamma=list(formula=~time),p=list(formula=~time)),output=FALSE,filename=fichierOUT)
      
            
      #Constant model (model 2)
      fichierOUT = paste(repOUT,"modPr_constant",sep="")
      modPr.constant = mark(modPradel.process, modPradel.ddl, model.parameters=list(Gamma=list(formula=~1),p=list(formula=~1)),output=FALSE,filename=fichierOUT)

      
      
      #Constant gamma model (model 3)
      fichierOUT = paste(repOUT,"modPr_gamma_constant",sep="")
      modPr.gamma_constant = mark(modPradel.process,modPradel.ddl,model.parameters=list(Gamma=list(formula=~1),p=list(formula=~time)),output=FALSE,filename=fichierOUT)

      
      
      #Constant p model (model 4)
      fichierOUT = paste(repOUT,"modPr_p_constant",sep="")
      modPr.p_constant = mark(modPradel.process,modPradel.ddl,model.parameters=list(Gamma=list(formula=~time),p=list(formula=~1)),filename=fichierOUT)

      
      #Compare models

      cat("\n *****************\n Compare models CJS \n**********************\n")
      print(cjs.results)
      
      cat("\n *****************\n Compare models Pradel \n**********************\n")
      pradel.results = collect.models(type="Pradel")
      
      print(pradel.results)
      bachibouzouk = as.data.frame(pradel.results$model.table)
      write.csv(bachibouzouk,paste(repOUT,"summaryPradel.csv"))
      cat("\n")
    }
  }
}


#__________________________________________________________________
# MAIN PROGRAM

library(RMark)
#rm(list=ls(all=TRUE))
  
especes = c("ACRSCH","ACRSCI","LUSSVE","ACROLA")
annees = 2003:2007

scriptMark(especes,annees)
  