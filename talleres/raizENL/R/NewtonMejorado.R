#' @title Busca una raíz con el método de newton mejorado
#' @description Por medio del método de newton, busca una raíz aproximada de una ecuación no lineal
#' @param f función de una ecuación no lineal
#' @param xi x inicial
#' @param tol tolerancia (por defecto : 1e-8)
#' @param prec precisión de la respuesta (por defecto: 6)
#' @return la aproximación a la raíz de f, si existe y el error
#' @export newtonMejorado
#' @examples
#'
#' f = function (x) x^2-3
#' xi = 1.6
#' tol = 1e-6
#' prec = 5
#' res = triseccion (f, xi, tol, prec)
#' res
#'
#' res = triseccion (f, xi, tol)
#' res
#'
#' res = triseccion (f, xi)
#' res
#'
#'
#------------------------------------------
newtonMejorado = function(f, xi, tol = 1e-8, prec = 6){

  install.packages(Deriv)
  library(Deriv)
  print("Método newton mejorado")
  cat("Tolerancia:", tol, "\n")
  cat("Precisión:", prec, "\n")
  plot(f, to=xi-1, from=xi+1, type = "l", main = "Gráfica función", xlab = "X", ylab = "Y", col = "red")
  f1 = Deriv(f,"x", nderiv=1)
  f2 = Deriv(f,"x", nderiv=2)
  ea = 1000
  x<-xi
  points(x= x, y= f(x), col = "blue")
  while (ea > tol){
    g = f(x)
    h = f1(x)
    k = f2(x)
    j = x-(g*h)/(h^(2)-(g*k))
    ea=abs(j-x)
    x = j
    points(x= x, y= f(x), col = "blue")
  }

  respuesta<-data.frame(resultado =round(x, prec), error = abs(0-f(x)) )

  return(respuesta)
}
