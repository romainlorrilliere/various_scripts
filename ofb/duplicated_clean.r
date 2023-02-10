library(readxl)
library(data.table)

d <- read_xlsx("data/arretes_par_jour_20230125.xlsx")
setDT(d)

## dim(d)
d[,`:=`(annee = NULL,date = NULL)]
## dim(d)
d <- unique(d)
## dim(d)

head(d)

fwrite(d,"data/arretes_par_jour_20230125_unique.csv",sep="\t")
