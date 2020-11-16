
vecPackage=c("ggplot2","ggmap","mapproj","maps","mapdata","sf","OpenStreetMap","ggrepel")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
     library(p,character.only = TRUE)

}



geom_point(data = dfirst,mapping=aes(x=x,y=y,colour=time_num_sc))




nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
gg <- ggplot(nc)
gg <- gg + geom_sf()



data <- st_centroid(nc)

coord <- st_coordinates(data)

data2 <- cbind(as.data.frame(data),coord)

veccol <- c(


gg <-  ggplot(data=nc) + geom_sf(aes(fill = SID74, alpha = AREA))
gg <- gg + geom_sf(data=data,aes(colour = as.character(SID79),size=PERIMETER))
##gg <- gg + geom_label(data=data2,mapping=aes(x=X,y=Y,label=NAME))
gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
gg <- gg + scale_fill_gradientn(low = "green",mid="white", high="red" )
gg



gg <- autoplot.OpenStreetMap(mp) + facet_wrap(.~cat_txt_fact)
    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
gg <- gg + scale_colour_gradientn(colours = c("yellow","yellow","orange","red","darkred","darkred"),values=c(0,.1,.3,.6,.9,1))

    gg <- gg + geom_point(data = dfirst,mapping=aes(x=x,y=y,colour=time_num_sc),size=4,shape=15) + geom_point(data = dfirst,mapping=aes(x=x,y=y),colour="white",size=2,shape=15)+ geom_point(data = dlast,mapping=aes(x=x,y=y,colour=time_num_sc),size=4,shape=17) +  geom_point(data = dlast,mapping=aes(x=x,y=y),colour="white",size=2,shape=17)

    gg <- gg + geom_path(data=d,mapping=aes(x=x,y=y,colour=time_num_sc),alpha= 0.45 ,size=1.5)

     gg


gg <- ggplot(data = d, mapping = aes(x= annee, y = abond))
gg  <-  gg + geom_point(colour = "#1224562",size=1.2)+ geom_line(colour="",size=1)
gg <- gg + geom_vline(xintercpt=2000)
gg  <- gg + geom_point(data= dataFrance)
   gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
