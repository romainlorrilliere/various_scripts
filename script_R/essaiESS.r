##########################################
# prise en main de ESS
#######################################

nbMax = 1000
a = 1:nbMax
b = a + 3
for(i in a){
  c = a+b+rnorm(nbMax,10,10)
  b = c
  cat(i)
}
plot(a,b,type='l')

a <- 5

if((a>2) & (is.element(codePRA,listPRA))) cat("YES")






