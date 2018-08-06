#Punto-a


puntofijo <-function(g, x0){
  maxIter=100
  tol=1e-8
  k = 1
  # iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  repeat{
    x1 = g(x0)
    print(x1)
    dx = abs(x1-x0)
    x0 = x1
    #Imprimir estado
    cat("x_", k, "= ", x1, "\n")
    k = k+1
    #until
    if(dx< tol|| k > maxIter) break;
  }
  # Mensaje de salida
  if( dx > tol ){
    cat("No hubo convergencia ")
    #return(NULL)
  } else{
    cat("x* es aproximadamente ", x1, " con error menor que ", tol)
  }
}
g <- function(x) return(5*x-exp(1)^x-1)


puntofijo(g,20)