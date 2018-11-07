#100 200 300 400 450 500 600
#-160 -35 -4.2 9 ? 16.9 21.3

#Punto 1

options(digits=3)

require(PolynomF)
x = c( 100,200,300,400,500,600) 
y = c(-160,-35,-4.2,9,16.9,21.3)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y")

DatosX = x[1:5]; DatosY = y[1:5]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio # RESULTADO: -520 + 5.41*x - 0.0217*x^2 + 3.96e-05*x^3 - 2.68e-08*x^4   

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y")
curve(Ajuste_Polinomio,add=T,from =100,to =500)

#Punto 2

#-520 + 5.41*450 - 0.0217*450^2 + 3.96e-05*450^3 - 2.68e-08*450^4 = 29.8

#Punto 3

x = c( 100,200,300,400,450,500,600) 
y = c(-160,-35,-4.2,9,29.8,16.9,21.3)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y")

DatosX = x[2:6]; DatosY = y[2:6]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio # RESULTADO: -2370 + 29.1*x - 0.131*x^2 + 0.000255*x^3 - 1.81e-07*x^4    

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y")
curve(Ajuste_Polinomio,add=T,from =200,to =500)


#Punto 4

lagrange = function(x,y,a){
  n = length(x)
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}
# ----------------------------------------- Prueba---------------------------------------------
x = c( 100,200,300,400,450,500,600) 
y = c(-160,-35,-4.2,9,29.8,16.9,21.3)

Resultados<-c(lagrange(x[2:5],y[2:5],300),lagrange(x[2:5],y[2:5],350),lagrange(x[2:5],y[2:5],400),lagrange(x[2:5],y[2:5],450))
xs<-c(300,350,400,450)
plot(xs, Resultados, type="overplotted",
     pch=1, col="blue", xlab="x",
     ylab="y",
     main="Lagrange",
     ylim=c(400,450))
#-----------------------------------------------------------------------------------------------
plot(x,y,type="l",col="blue",lwd=3, main="funcion", xlab="", ylab="", las=1, col.axis="red")

#Polinomio = -4.20 + 0.44*x + 9.00*x^2 + 29.8*x^3
