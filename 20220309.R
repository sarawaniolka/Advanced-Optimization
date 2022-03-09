# Numerical derivatives

f = function(x){
  x^3
}

df = function(f, x ,del = 10^-4)
{
  
  out = (f(x+del) - f(x-del))/ (2*del)
  return(out)
}

x = 3
df(f, x)

xx = seq(-5, 5, by = 0.01)
yy = df(f,xx)

plot(xx, yy, type='l')

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


#First order derivative
# f:R^n -> R
#dnf - calculates a numerical derivative of a function f:R^n ->R in a point x.

f = function(x){
  sum(x^2)
}

x = 3
f(x)

x = c(2,3)
f(x)

x = c(1,2,3,4,5)
f(x)

dnf = function(f,x,t = 10^-4){
  g = c()
 n = length(x)
 
 for (i in 1:n){
   ei = rep(0,n)
   ei[i] = 1
   g[i] = (f(x+t*ei) - f(x-t*ei))/(2*t)
}
return(g)
}

x=3
dnf(f,x)

x = c(1,2,3)
dnf(f,x)
grad(f,x)
