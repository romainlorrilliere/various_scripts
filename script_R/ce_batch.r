repertoire = "D:\Programme\"
commande = paste(repertoire,"PHENSIM11b ",repertoire,"LDf_Phensim.txt")
system(commande)

"PHENSIM11b LDf_Phensim.txt"



cd C:\Nicodemus\PEDANTIX LD  #directory
PHENSIM11b LDf_Phensim.txt  #lancement
ADVPHENSIM2.exe outLDf_Phensim.txt 10R.txt #lancement

"C:\Nicodemus\PEDANTIX LD\tableauCeline.r"



cd C:\Nicodemus\PEDANTIX LD\ASR #directory
"C:\Program Files\Asreml2\bin\asreml.exe" -sSn A1f.as #lancement
"C:\Program Files\Asreml2\bin\asreml.exe" -sSn A1f.pin #lancement


rename A1f.asr replicate1results.txt
rename A1f.pvc estimate1results.txt