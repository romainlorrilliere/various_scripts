library(data.table)
load("data/datatoaggr.Rdata")
head(x)

library(data.table)
setDT(x)
x[, max:= max(nb_of_points_with_presence),by = species]
x_max <- x[nb_of_points_with_presence == max,]
