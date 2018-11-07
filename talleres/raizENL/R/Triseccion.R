#' @import Deriv
#' @title Busca una raíz con el método de trisección
#' @description Por medio de una partición en 3 intervalos de un intervalo a,b, iterativamente el programa busca una raíz para una ecuación
#' no lineal
#' @param a limite menor del intervalo
#' @param b limite mayor del intervalo
#' @param f función de una ecuación no lineal
#' @param tol tolerancia (por defecto : 1e-8)
#' @param prec precisión de la respuesta (por defecto: 6)
#' @return la aproximación a la raíz de f, si existe y el error
#' @export triseccion
#' @examples
#'
#' f = function (x) x^2-3
#' a = 1
#' b = 2
#' tol = 1e-6
#' prec = 5
#' res = triseccion (f, a, b, tol, prec)
#' res
#'
#' res = triseccion (f, a, b, tol)
#' res
#'
#' res = triseccion (f, a, b)
#' res
#'
#'
#------------------------------------------
triseccion = function (f, a, b, tol = 1e-8, prec = 6){
  print("Método de trisección")
  if (f(a)*f(b) > 0){
    print("No se puede aplicar el metodo de la trisección.")
    cat("Razón:","f(a)=", f(a) , "y", "f(b)=", f(b), "tienen el mismo signo.\n")
  }
  else {
    plot(f, to=b, from=a, type = "l", main = "Gráfica función", xlab = "X", ylab = "Y", col = "red")
    cat("Tolerancia:", tol, "\n")
    cat("Precisión:", prec, "\n")
    xr1=((2*a)+b)/3
    xr2=(a+(2*b))/3
    if(f(a)==0){
      xr1 = a
    }
    else if(f(b)==0){
      xr2 = b
    }

    while (abs(f(xr1)) >= tol && abs(f(xr2)) >= tol && !abs(f(xr1))==0 && !abs(f(xr2)) ==0){
      points(x= xr1, y= f(xr1), col = "blue")
      points(x= xr2, y= f(xr2), col = "green")
      if (f(xr1)*f(a)<0)
      {
        b=xr1
      }
      else if (f(xr1)*f(xr2)<0)
      {
        a=xr1
        b=xr2
      }
      else if (f(xr2)*f(b)<0)
      {
        a=xr2
      }

      xr1=((2*a)+b)/3
      xr2=(a+(2*b))/3

    }

    if (abs(f(xr1)) <= tol){
      cat("Error:", abs(0-f(xr1)), "\n")
      respuesta<-data.frame(resultado =round(xr1, prec), error = abs(0-f(xr1)) )
      return(respuesta)
    }
    if (abs(f(xr2)) <= tol){
      cat("Error:", abs(0-f(xr2)), "\n")
      respuesta<-data.frame(resultado =round(xr2, prec), error = abs(0-f(xr2)) )
      return(respuesta)
    }
  }
}

