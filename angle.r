library(data.table)

x1 <- c(2, 2, 1,2)
y1 <- c(40, 41,40,40)
x2 <- c(2, 2, 2,1)
y2 <- c(45, 39,40,40)

## example of data
d <- data.frame(x_centroid = x1, y_centroid = y1, x_transloc = x2, y_transloc = y2)
setDT(d)

d[,angle:=atan2(x_transloc-x_centroid,y_transloc-y_centroid) * (180 / pi)]


### refence angle talbe
vec_cut <- seq(from = -180, to = 180, 22.5)
all_angle <- -179:180

d_cat <- data.frame(angle_cat = as.vector(unique(cut(all_angle,vec_cut))),dir_cat = c("S",rep(c("SW","W","NW","N","NE","E","SE"),each=2),"S"))


### angle aggreagation in categories
d[,angle_cat := cut(angle,vec_cut)]

d <- merge(d,d_cat,by="angle_cat")

print(d)
