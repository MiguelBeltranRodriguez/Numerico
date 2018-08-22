
options( digits = 13 )
#PUNTO 1.b
n = 4;

z<-matrix(c(2,6,4,7,9,10,12,6,7,3,5,9,-5,6,2,0), ncol=n)


sumarNxN<-function(z,n){
  suma=0
  
  for(x in 1:n)
  {
    for(y in 1:n)
    {
      
      suma = suma +as.numeric(z[x,y]) 
      
    }
    
  }
  return(suma)
}


resultado = sumarNxN(z,n)
print('Matriz:')
z
print('N:')
n
print('Resultado')
resultado


#Tamano entrada n*n
#Operaciones básicas: suma dentro los dos 'for'
#T(n) = n^2
#     = O(n^2)

#Resultados en anexo 1


#PUNTO 2.a





f<-function(x) return(log(x+2)-sin(x))
fm<-function(x)return(log(x+2))
g<-function(x)return(sin(x))


interseccion<-function(f,x, x1, x2)
{
    err = f(x)
    print(x)
    print(err)
    if(abs(err)>=1.e-7)
    {
      aux = x;
      x = x1 -((f(x1)*(x1-x2))/(f(x1)-f(x2)));
      return(interseccion(f,x,aux,x1));
    }
    return(x);
}
#Ver anexo 2 para ver resultado

print('Llamado a la funcion: ')
prueba = interseccion(f,-1.5,-1,-1.6)

print('X donde interceptan: ')
prueba

print('Probar si esa X es casi 0')
j=f(prueba)
j

print('Probar x en la funcion f')
xf = fm(prueba)
xf
print('Probar x en la funcion g')
xg = g(prueba)
xg
print('Los resultados son similares, por lo tanto es un punto de intersección')



#PUNTO 3



# z = x+0.645y

# 50x+31y<=250
# -3x+2y <=4

# y>0
# x>0

library(linprog)
z<-(c(1,0.645))
a<-matrix(c(50,-3,31,2), ncol=2)
b<-(c(250,4))
r<-rep('<=',2)

cifras = 1e-2

solucion<-solveLP(cvec = z,bvec = b,Amat = a, maximum=TRUE, const.dir =r,zero = cifras, tol = 1e-7,verbose = 0)


print('Solución con dos cifras significativas: ')

summary(solucion)

#Es recomendable utilizar la maxima precisión utilizada por el programa

print('Solución con una cifra significativas: ')

cifras = 1
solucion<-solveLP(cvec = z,bvec = b,Amat = a, maximum=TRUE, const.dir =r,zero = cifras, tol = 1e-7,verbose = 0)

summary(solucion)


#Con valores 0, no se maximiza de la forma esperada, pero sigue cumpliendo el sistema de la ecuación
#ver anexo 3 para ver resultados