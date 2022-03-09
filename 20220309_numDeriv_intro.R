library(numDeriv)


# Comparison of both
f = function(x){
  x^3
}

df = function(f, x ,del = 10^-4)
{
  
  out = (f(x+del) - f(x-del))/ (2*del)
  return(out)
}

yy = df(f,xx)
xx = seq(-5, 5, by = 0.01)
yy1 = grad(f, xx)
plot(xx, yy1, type='l')


dd = yy - yy1
plot(xx,dd)
lines(xx,dd,col='red')
mean(abs(dd))
max(dd)
min(dd)
#all the differences are positive
