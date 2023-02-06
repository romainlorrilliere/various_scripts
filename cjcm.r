library(data.table)

dd <- fread("data/cjcm.csv",encoding="UTF-8")

d <- melt(dd)
colnames(d) <- c("author","review","note")
d <- d[review != "MOYENNE"]

d <- d[,note_sc := scale(note),by=review]

dmean <- d[,.(mean_sc = mean(note_sc),med_sc = median(note_sc)), by = author]
print(dmean)

setorder(dmean,-med_sc)
dmean[,order_med := 1:.N]

setorder(dmean,-mean_sc)
dmean[,order_mean_sc := 1: .N]

print(dmean)


colnames(dd) <- c("author","romain","camille","karl","ludo","alexia","diane","anna","mean")
setorder(dd,-romain)
dd[,order_romain := 1: .N]
setorder(dd,-camille)
dd[,order_camille := 1: .N]
setorder(dd,-karl)
dd[,order_karl := 1: .N]
setorder(dd,-ludo)
dd[,order_ludo := 1: .N]
setorder(dd,-alexia)
dd[,order_alexia := 1: .N]
setorder(dd,-diane)
dd[,order_diane := 1: .N]
setorder(dd,-anna)
dd[,order_anna := 1: .N]


setorder(dd,-mean)
dd[,order_mean := 1: .N]


dd <- merge(dd,dmean,by = "author")

setorder(dd,-mean)


dd[,first_11_mean_sc := order_mean < 12]
dd[,first_11_mean := order_mean < 12]
dd[,first_11_med_sc := order_mean < 12]
print(dd)



dd[,first_11_romain := order_romain < 12]
dd[,first_11_camille := order_camille < 12]
dd[,first_11_karl := order_karl < 12]
dd[,first_11_ludo := order_ludo < 12]
dd[,first_11_alexia := order_alexia < 12]
dd[,first_11_diane := order_diane < 12]
dd[,first_11_anna := order_anna < 12]

print(dd)



dd[,in_mean := ifelse(as.numeric(first_11_mean_sc)+as.numeric(first_11_mean) + as.numeric(first_11_med_sc) == 3, "TRUE",ifelse(as.numeric(first_11_mean_sc)+as.numeric(first_11_mean) + as.numeric(first_11_med_sc) == 0,"FALSE","LITIGE"))]


dd[,in_review := ifelse(as.numeric(first_11_romain)+as.numeric(first_11_camille) + as.numeric(first_11_karl) + as.numeric(first_11_ludo)+as.numeric(first_11_alexia) + as.numeric(first_11_diane)+ as.numeric(first_11_anna) == 7 , "TRUE",ifelse(as.numeric(first_11_romain)+as.numeric(first_11_camille) + as.numeric(first_11_karl) + as.numeric(first_11_ludo)+as.numeric(first_11_alexia) + as.numeric(first_11_diane)+ as.numeric(first_11_anna) == 0,"FALSE","LITIGE"))]

print(dd)


ddd <- dd[,.(author,romain,camille,karl,ludo,alexia,diane,anna,mean,mean_sc,order_romain,order_camille,order_karl,order_ludo,order_alexia,order_diane,order_mean_sc,in_mean,in_review)]

fwrite(ddd,"data/cjcm_classement.csv")

fwrite(dd,"data/cjcm_classement_complet.csv")
