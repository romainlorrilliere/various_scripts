######################################
#   Script calcul de distance
#####################################





tablo <- matrix(rnorm(100),10,10)

x <- 1:10
y <- 1:10


filled.contour(x, y, t(tablo), color = heat.colors,
    plot.title = title(main = "...",
    xlab = "...", ylab = "..."),
  ##  plot.axes = { axis(1, moy )
  ##                axis(2, vari)},
    key.title = title(main="..."),
    key.axes = axis(4))


dep <- c(3,5)
arr <- c(8,9)

difX <- arr[1]-dep[1]
difY <- arr[2]-dep[2]




