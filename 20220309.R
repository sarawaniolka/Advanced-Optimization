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