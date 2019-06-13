

#Hi Romain! I added some comments and adapted (some) file names to explain this routine to myself - so I can understand it also later
#and use for my analyses. There are a few parts and details where I am not sure what the routines does.
#I indicated (***Q***) where I have questions so they are easier to spot for you.
#Can you look through and add some explanation where you can (where you remember)? Thanks a lot!


#R routine for running PEDANTICS replicates

#"C:\Program Files\Asreml2\bin\asreml.exe" -sSn basename.as
#"C:\Program Files\Asreml2\bin\asreml.exe" -sSn basename.pin
#rename basename.asr as replicate1results.txt
#rename basename.pvc as estimate1results.txt

#creating a routine to run in DOS: 1) run phensim, 2) run adphensim, 3) ? - somehow preparing adphensim_out for analysis,
#4) run ASReml on adphensim_out, 5) rearranging pvc file to produce a nice results table

myDOS.routine = function()
{
   #1) run phensim
   directory = "C:/Nicodemus/PEDANTIX_LD/"               #specifies working directory
   program = paste(directory,"PHENSIM11b",sep="")        #specifies the program to run, paste(): concatenate strings
   file = paste(directory, "Phensim_in.txt", sep="")     #specifies the file to run
   command = paste(program,file)
   system(command,show.output.on.console=F)              #system asks DOS run
  
   #2) run adphensim
   program = paste(directory,"ADVPHENSIM2.exe ",sep="")
   file1 = paste(directory, "Phensim_out.txt", sep="")   #input file 1 (phenotypes simulated by Phensim) ## Just it name
   file2 = paste(directory, "Adphensim_in.txt", sep="")  #input file 2 (specification for the multiple records) ## Just it name
   command = paste(program,file1,file2)
   system(command,show.output.on.console=F)
  
   #3) ***Q*** what is this part doing? I don't quite understand what is happening here and in the  alakeuleuleu function below
   
   ## all right, alakeuleuleu() is not a good function name for a not french speaker ;-)    I propose to replace by  colummnsOneBelowOther()
   ## the aim of this function is to append the columns under each other
  
  
     
   theTable = readTable()                               #***Q*** how does it know which table to read?
                                                              ##  because the readTable() I gave to the function which is developped below
  
   theNiceTable = colummnsOneBelowOther(theTable)
  
   colnames(theNiceTable) = c("ID","laydate")            #specifies column headers for new table
   outFile = paste(directory,"Adphensim_out.asd")        #specifies file to use for the table   ## Just it name
   write.table(theNiceTable,na='NA',outFile,sep="\t",row.names=FALSE,quote=FALSE)        #'na' is specifying missing values=NA
  
   setwd(directory)                                      #set working directory
  
   #4a) run ASReml command file
   program = "asreml.exe"
   file = paste(directory, "basename.as", sep="")        #data file to run needs to be in the same directory as the command file "basename.as"
   parameter = "-sSn"                                    #specification for program (-s: increasing workspace)
   command = paste(program,parameter,file)
   system(command,show.output.on.console=F)
  
   #4b) run ASReml pin file
   file = paste(directory, "basename.pin", sep="")
   parameter = "-sSn"
   command = paste(program,parameter,file)
   system(command,show.output.on.console=F)
  
   #5) rearranging pvc file (Asreml output file) to produce a nice results table
   useFile = paste(directory,"A1f.pvc",sep="")
   tab = read.delim(useFile,colClasses="character")
   for(i in 2:5)         #for rows 2 to 5
   {
     d = tab[i,1]                                        #***Q*** is this finding the tab delimiters in rows i ? but why do you need the '1'?
                                                              ## OK it is a good question, because this method is not the best method so this is not clear. Asreml makes a file very complicated. So we use read.delim() to import without column separator (all the table of Asreml is in the first column). After we create the columns line by line (here for rows 2 to 5), and for that we use the strsplit() function. But this function acccept (in our case) only atom element or not vector. So tab[i,1] is a atom element contrary to tab[i,] that is a vector. 
     notPerfectRow = strsplit(d," ")               #for old row - strsplit(): splits elements of a character vector -> creates separate columns
                                                      ## strsplit don't create a vector but a object of list class
     perfectRow =  notPerfectRow[[1]][which(notPerfectRow[[1]]!="")]         #***Q*** what is happening here?
                                                                          ## which() return the indexes of the elments which are selected by the test (!= "" means different of empty), and we keep just these elements of notPerfectRow to make the perfectRow
     if(i == 2)   notPerfectTable = perfectRow
     else  notPerfectTable = rbind(notPerfectTable,perfectRow)
  
   }
   perfectTable1 = notPerfectTable[,c(2,4,5)]                           #for new table1: specifies columns to use from notPerfectTable
  
   for(i in 6:8)                                         #for rows 6 to 8
   {
     d = tab[i,1]
     notPerfectRow = strsplit(d," ")
     perfectRow =  notPerfectRow[[1]][which(notPerfectRow[[1]]!="")]
     if(i == 6)   notPerfectTable = perfectRow
     else  notPerfectTable = rbind(notPerfectTable,perfectRow)
  
   }
   perfectTable2 =  notPerfectTable[,c(1,7,8)]                         #for new table2
  
   TableOut = rbind(perfectTable1,perfectTable2)
  
  
   #***Q*** Is the following to rearrange TableOut which now has Vp, Vs etc. in rows but you want it columns?
    ## YES, it is. 
   TableOut = as.vector(t(TableOut[,2:3]))               #***Q*** what is this doing?
                                                            ## We select the column of value (2 and 3, the first is the column of the ID), and we transform it to a vector. We want to display all the results in only one row.
   colheaders = c("Vp_estim","Vp_se","Va_estim","Va_se","Ve_estim","Ve_se","Vr_estim","Vr_se","h2_estim","h2_se","e2_estim","e2_se","r2_estim","r2_se")
  
   TableOut = matrix(TableOut,1,length(TableOut))        #***Q*** not sure how this arranges the data
                                                              ## here we make a matrix and below a data.frame with only one row. This is essential to bind the result of several sumulates. 
   colnames(TableOut) = colheaders
  
   TableOut = as.data.frame(TableOut)
  
   return(TableOut)                                      #show out table
}





#***Q*** Should these parts below not come somewhere above?, e.g. alakeuleuleu is specified here but used above
  ## it is not a problem because all the routine is in functions, so R read whole of the file before executing the last line of file : tab = batchPedantics(2)   


#creating a new table using parts of the adphensim_out file
readTable = function()
{
 file = "C:/Nicodemus/PEDANTIX_LD/Adphensim_out.txt"
 table = read.table(file,sep="\t",header=TRUE)
 table = table[,1:11]                                  #use all rows and columns 1 to 11
 return(table)
}

#***Q*** what is this alakeuleuleu part doing? is it transposing the table? (since the output file has 1 row for each individual
#and the repeated records are in the columns - but repeated records should appear as rows)

     ## the main of is function is to append the columns under each other

colummnsOneBelowOther = function(table)
{
 # print(dim(table))
 j = 2
 newTableau = cbind(paste(table[,1]),table[,j])       #***Q*** what is this doing?
                                                          ## We make a new table of 2 columns. The first "paste(table[,id]) is the names (in text format) of id and the second is the value of variable 1 (because j = 2)
 table_out = sousTableau
 for(j in 3: ncol(table))                              #loop for j(=columns) from col 3 to col x (x = number of columns in 'table')
 {
  # print(j)
   newTableau = cbind(paste(table[,1]),table[,j])
   table_out = rbind(table_out,newTableau)                 ## tableauSortie --> table_out
 }
#  print(dim(tableauSortie))
 return(table_out)
}




#specifying the batch by the following function
batchPedantics = function(nbBatch)                      #***Q*** is bnBatch specifying the number of runs? where is bnBatch defined?
                                                            ## Yes, you define this parameter when you run the routine, see below. 
{
 directory = "C:/Nicodemus/PEDANTIX_LD/"
 fileOUT = paste(directory,"tabBatch.csv",sep="")      #***Q***is this just creating an output file?
                                                            ## here we create just the name of the output file.

 for(b in 1:nbBatch)                                   #loop for number of runs
 {
   cat(b," ")                                          #cat(): concatenate and print
   flush.console()                                     #flush.console(): flush output from () to a console
   if (b == 1) tabBatch = myDOS.routine()              #***Q*** is this using the final ouput from myDOS.routine (=TableOut) to transfer it to 'tabBatch'?
                                                          ## TableOut is a local variable of the funtion myDOS.routine(), and we affect the result of this function to tabBatch.
   else  tabBatch = rbind(tabBatch,myDOS.routine())    #for all follwing runs: add ouput from myDOS.routine in tabBatch as new rows
   if(b%%10)                                           #***Q*** what does 'b%%10' mean?
                                                           ## saving during the process, all the 10 step.
   {
     cat(" [SAVE] \n")
     write.csv(tabBatch,fileOUT)                       #write the output file using tabBatch  
                                                            ## this is an update of fileOUT file               
   }

 }
 directory = "C:/Nicodemus/PEDANTIX_LD/"
 fileOUT = paste(directory,"tabBatch.csv",sep="")
 write.csv(tabBatch,fileOUT)

 cat("<-- ",fileOUT,"\n")
 flush.console()
 return(tabBatch)
}

tab = batchPedantics(nbBatch = 2)                                 #***Q*** what is this?
                                                            ## this is the main function of this routine where you must indicate the number of runs (here 2) you want to execute.