specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#f <- function(x) return((1/sqrt(2*pi))*exp(1)^(-(1/2)*x^2))

f <- function(x) return(1/x)
g <- function(x) return(cos(x))
z <- function(x) return(0)

calcularN <- function(m, b, a, er) return((b-a)*sqrt((m*(b-a))/(12*er)))

pr <- function(x) return(sin(x))


sumatoriaAreas <- function(a,b,n,f) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    polygon(0, 10)
    
    rect(aux, 0, aux+h, f(aux+(h/2)))
    suma = suma + (h*(aux+(h/2)))
    aux = aux + h
    
    
    
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}

sumatoriaAreasTrapecio <- function(a,b,n,f) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    
    polygon(c(aux,aux,aux+h,aux+h), c(f(aux),0,0,f(aux+h)), angle = c(90, 90,90,90))
    
    suma = suma + ((h*(f(aux)+f(aux+h))/2))
    aux = aux + h
    
    
    
    
    
    
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}




sumatoriaAreasCurvasTrapecio <- function(a,b,n,f,g) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    
    polygon(c(aux,aux+h), c(f(aux),f(aux+h)), angle = c(90, 90,90,90))
    
    primerAltura = abs(f(aux)-g(aux)) 
    segundaAltura = abs(f(aux+h)-g(aux+h))
    
    suma = suma + ((h*(primerAltura+segundaAltura)/2))
    aux = aux + h
    
    
    
    
    
    
  }
  
  return(specify_decimal(suma,8))
  #return(suma)
}


sumatoriaLongitudCurva <- function(a,b,n,f) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    
    polygon(c(aux,aux+h), c(f(aux),f(aux+h)), angle = c(90, 90,90,90),lwd = 1.5)
    points(c(aux,aux+h), c(f(aux),f(aux+h)), col = "blue")
    
    
    suma = suma + sqrt((aux+h-aux)^2+(f(aux+h)-f(aux))^2)
    aux = aux + h
    
    
    
    
    
    
  }
  
  return(specify_decimal(suma,8))
  #return(suma)
}


a=0.5
b=2
n = 20

#plot(f, from = a, to = b, main= 'Grafica función', xlab = 'X', ylab = 'Y', col="red",xlim=c(-1,5),ylim=c(0,6))
#par(new=TRUE)
plot(f, from = a, to = b, main= 'Grafica función', xlab = 'X', ylab = 'Y', col="red",xlim=c(0,2.0),ylim=c(0,4.5), lwd = 2)






#integrate(pr,0,pi/4)
#integral = 0.5857864





resultado = sumatoriaLongitudCurva(a,b,n,f)

#resultado = sumatoriaAreasCurvasTrapecio(a,b,n,f,z)
#resultado = 2*as.numeric(resultado)
#erro = resultado - integral
#erro





prueba<-function()
{
  while(x<4){
    resultado = sumatoriaAreasTrapecio(a,x,n,f)
    print(x)
    print(resultado)
    x=x+0.01
  }
}
prueba()



#cambio de coordenadas
#hacer integral flor
#hacer doble integrales
#hacer integrales triples
#hacer intregal curva parametrica
