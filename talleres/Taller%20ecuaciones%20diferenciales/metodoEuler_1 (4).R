#install.packages("phaseR")
install.packages("pracma")
#library(phaseR)
library(pracma)

metodoEuler <- function(f, h, xi, yi, xf)
{
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+(h*f(x[i],y[i]))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}
#install.packages(Deriv)
require(Deriv) # derivadas parciales
#--- Metodo de Taylor, orden 4
mtaylor4= function(f, t0, y0, h, n){
  #Datos igualmente espaciados iniciando en t0 = a, paso h. "n" datos
  t = seq(t0, t0 + (n-1)*h, by = h) # n datos
  y = rep(NA, times=n) # n datos
  y[1] = y0
  # Derivadas parciales con el paquete Deriv. Deriv(f)
  ft=Deriv(f,"t",nderiv = 1); fy=Deriv(f,"y",nderiv = 1)
  f1 = function(t,y)
    ft(t,y)+fy(t,y)*f(t,y)
  f1t=Deriv(f1,"t"); f1y=Deriv(f1,"y")
  f2= function(t,y) f1t(t,y)+f1y(t,y)*f(t,y)
  f2t=Deriv(f2,"t"); f2y=Deriv(f2,"y")
  f3= function(t,y) f2t(t,y)+f2y(t,y)*f(t,y) # orden m = 4
  for(i in 2:n ){
    f0i = f(t[i-1], y[i-1])
    f1i = f1(t[i-1], y[i-1])
    f2i = f2(t[i-1], y[i-1])
    f3i = f2(t[i-1], y[i-1])
    y[i] = y[i-1] + h*(f0i + h/2*f1i + h^2/6*f2i + h^3/24*f3i )
  }
 
  
  return(data.frame(X = t, Y = y))
}

imprimir<-function(e1, col="red"){
  i=1
  while(i<=nrow(e1))
  {
    lines(e1$X, e1$Y, col=col)
    i = i+1
  }
}


#-------- punto 1.
f <- function(x, y) return((-5.6e-8*6*0.5*(y^4-200^4))/(1*100))
e1 = metodoEuler(f, 10, 0, 180, 200)

e1[nrow(e1),]

xx <- c(0, 200); yy <- c(0, 200)
vectorfield(f, xx, yy, scale = 1)

imprimir(e1)

sol <- rk4(f, 0, 200,180, 19)
sol$y
lines(sol$x, sol$y, col="purple")


#------- punto 2.

f = function(t,y) 1-t^2+(t+y)
t0 = 0; y0 = 1; h = 0.1; n=5
s = mtaylor4(f, t0, y0, h, n)

xx <- c(0, 0.5); yy <- c(0, 2.5)
vectorfield(f, xx, yy, scale = 0.02)
imprimir(s)
sol <- rk4(f, 0, 0.4,1, 4)

lines(sol$x, sol$y, col="purple")
errory = abs(sol$y[5]- s[5,2])

errory

#------- punto 3.

f = function(x,y) 1-x^2+(x+y)
t0 = 0; y0 = 1; h = 0.1

xx <- c(0, 3); yy <- c(0, 15)
vectorfield(f, xx, yy, scale = 0.1)
e1 = metodoEuler(f, h, t0, y0, 2)

e1[nrow(e1),]
sol <- rk4(f, 0, 2, 1, 19)
lines(sol$x, sol$y, col="purple")
imprimir(e1)



#----- punto 4.

f = function(x,y) 1-x^2+(x+y)
x0 = 0; y0 = 1; h = 0.1; m=20

g<-function(x0,y0,f ,h, m){
  i = 1
  x = numeric(m+1)
  y = numeric(m+1)
  x[1] = x0
  y[1] = y0
  while(i <= m){
    k1 = h*f(x[i], y[i])
    k2 = h*f(x[i]+h, y[i]+k1)
    
    y[i+1] = y[i] + ((1/2)*(k1+k2))
    x[i+1] = x[i] + h
    i = i+1
  }
  return (data.frame(X = x, Y = y))

}
e1 = g(x0,y0,f,h,m)
e1
e1[nrow(e1),]
xx <- c(0, 3); yy <- c(0, 15)
vectorfield(f, xx, yy, scale = 0.1)
sol <- rk4(f, 0, 2, 1, 19)
lines(sol$x, sol$y, col="purple")
imprimir(e1)


# ---- punto 5.



metodoEulerModificado <- function(f, h, xi, yi, xf)
{
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  y[2] = y[1]+(h*f(x[1],y[1]))
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+((h/2)*(f(x[i],y[i])+f(x[i+1], y[i+1])))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}
f = function(x,y) 1-x^2+(x+y)
t0 = 0; y0 = 1; h = 0.1


e1 = metodoEulerModificado(f, h, t0, y0, 2)
xx <- c(0, 3); yy <- c(0, 15)
vectorfield(f, xx, yy, scale = 0.1)
e2 = metodoEuler(f, h, t0, y0, 2)

imprimir(e1) #eulermodificado
imprimir(e2, col="purple") #euler


#---- punto 7.

rungekutta = function(f,t0,y0,h,n){
  t = seq(t0, t0+n*h, by=h)
  y = rep(NA, times=(n+1))
  
  y[1] = y0
  for(k in 2:(n+1)){
    k1=h/2*f(t[k-1],y[k-1])
    k2=h/2*f(t[k-1]+h/2, y[k-1]+k1)
    k3=h/2*f(t[k-1]+h/2, y[k-1]+k2)
    k4=h/2*f(t[k-1]+h, y[k-1]+2*k3)
    y[k] = y[k-1]+1/3*(k1+2*k2+2*k3+k4)
  }
  return(data.frame(X = t, Y = y))
}

f = function(x,y) 1-x^2+(x+y)
t0 = 0; y0 = 1; h = 0.1


e1 = rungekutta(f, t0,y0,h,10)
xx <- c(0, 1.5); yy <- c(0, 6)
vectorfield(f, xx, yy, scale = 0.1)
e2 = metodoEuler(f, h, t0, y0, 1)

imprimir(e1) #rungekutta
imprimir(e2, col="purple") #euler





