# Taller # 1
###### Realizado por: Miguel Angel Beltrán Rodriguez y Juan Sebastian Triana Perez
1.
a)  Evaluar el polinomio en cada valor indicado y el número de operaciones mínimo para hacerlo, para los siguientes polinomios junto con sus derivadas
Codigo:

```r
hornet<-function(p,n,x ,iter){ #Funcion hornet p: vector de grados, n: Grado del polinomio, x: Valor inicial, iter: iteraciones
  y = p[c(1)];
  i = 0;
  for(i in 1:n+1){
    y = x*y + p[c(i)];
    
  }
  print(iter+i*2);
  return(y);
}

eval<-function(p, n, x ,iter){
  s = 0;
  i = 0;
  for(i in 0:n){
    s = s +(p[c(i+1)]*(x^(n-i)));
    print(i+1);
  }
  print(iter+i*3);
  return(s);
}

derivar<-function(p, n){  #Funcion que deriva p: Coeficientes, n: Grado del polinomio.
  d=numeric(0);
  iter = 0;
  for(i in 0: n){
    
    d[c(i+1)] = (n-i)*p[c(i+1)];
    iter = iter+1;
  }
  x = hornet(d, n-1, -2, iter); #Llamada a la funcion hornet.
  return(x)
}

p<-c(2,0,-3,3,4)

resultado = derivar(p,4);
resultado
```
Respuesta:

```r
        Con el polinomio: 13
        Con la derivada del polinomio: -49
```

2.
Se necesita un recipiente rectangular, sin tapa, de un litro de capacidad. Para construirlo se debe usar una lamina rectangular de 32 cm de largo y 24 cm de ancho. Eñ procedimiento será recortar un cuadro idéntico en cada una de las cuatro esquinas y doblar los bordes de la lámina para formar el recipiente.
Determine la medida del lado del cuadrado que se debe recortar en cada esquina para que el reccipiente tenga la capacidad requerida.

Utilice dos métodos diferentes para resolver el problema, instruméntelo en el lenguaje adecuado.

1) ¿Cual etapa del proceso ded resolución de un problema numérico requiere mas atención?
2) ¿Qué conocimientos son necesarios para formular un modelo matemático?
3) En el ejemplo de la caja ¿ Cual sería de la desventaja de intentar obtener experimentalmente la solución mediante prueba y error en lugar de analizar el modelo matematico?
4) ¿Que es más critico: el error de truncamiento o el error de redondeo ?
5) ¿Cuál es la ventaja de instrumentar computacionalmente un método numérico?
6) ¿Por que es importante validar los resultados obtenidos?

Codigo: 

```r
    #Metodo #1
    
    volbiseccion<-function(largo,ancho){
      a = 0;
      b = largo;
      x = ((a+b)/2);
      i = 0;
      error = abs(volumen(largo,ancho,x)-1000);
      while(error>1.E-8){
        print(x);
        i = i +1;
        if(volumen(largo,ancho,x)<1000)
        {
          b=x;
        }
        else
        {
          a=x;
        }
        x <- ((a+b)/2);
        error = abs(volumen(largo,ancho,x)-1000);
      }
      return (x);
    }
    volumen<-function(largo, ancho, x){
      return((largo-2*x)*(ancho-2*x)*x);
    }
    cubica<-function(largo, ancho){
      return()
    }
    y = volbiseccion(32,24);
    y
    
    #Metodo #2
    Fx <- function(x, largo, ancho) {return ((largo-2*x)*(ancho-2*x)*x)}
    F1x <- function(x, largo, ancho) {return (12*x^2-4*ancho*x-4*largo*x+largo*ancho)}
    
    newton <- function(x, largo, ancho) {
      for(i in 1:60) {
        x<-x-Fx(x, largo, ancho)/F1x(x, largo, ancho)
        if (Fx(x, largo, ancho) == 1000) break
        error<-abs(Fx(x, largo, ancho)/F1x(x, largo, ancho))
        cat("X=",x,"\t","E=",error,"\n")
      }
    }
    newton(10, 32, 24)
```
Respuesta metodo # 1

```r
    16
    8
    12
    10
    9
    8.5
    8.25
    8.125
    8.0625
    8.09375
    8.078125
    8.085938
    8.089844
    8.091797
    8.092773
    8.093262
    8.093018
    8.09314
    8.093201
    8.093231
    8.093216
    8.093224
    8.09322
    8.093218
    8.093219
    8.093219
    8.09322
    8.09322
    8.09322
    8.09322
    8.09322
    8.09322
    8.09322
    8.09322
    8.09322
    y = 8.09322
```

Respuesta metodo #2

```r
    X= 11.76471 	 E= 0.2272153
    X= 11.99192 	 E= 0.008067941
    X= 11.99999 	 E= 1.082665e-05
    X= 12 	 E= 1.953637e-11
    X= 12 	 E= 0
    X= 12 	 E= 0
```

Respuestas a preguntas:

1) La etapa de verificación
2) Cálculo básico y pensamiento algoritmico, ademas de conocimientos basicos en el area al que pertenece el modelo matematico.
3) La eficiencia es el mayor problema, ademas el margen de error puede ser mayor
4) El error de truncamiento puede llegar a ser mucho mas objetivo que el de redondeo
5) La velocidad y la eficacia
6) Para comprobar si el algoritmo es eficaz.


3.
Implemente en R el siguiente algoritmo que sirve para calcular la raíz cuadrada. Aplíquelo para evaluar la raíz cuadrada de 7, analice su precisión, convergencia y validez.

Codigo:
```r
    tercerpunto<-function(n,e,x){
      
      y<- 1/2 * (x+n/x);
      print("Convergencia:");
      
      erroanter=abs(x-y);
      errornuevo=0;
      contador=0;
      while (abs(x-y) > e) {
        
        errornuevo = abs(x-y);
        
        print(errornuevo/erroanter);
        x<-y;
        y<- 1/2 *(x+(n/x));
        erroanter = errornuevo;
        contador= contador+1;
        text(contador,erroanter,".",cex=1.0, col="red" );
      }
      return(y);
    } 
    
    raiz=tercerpunto(7,1.E-8,12);
    
    
    validez = raiz*raiz;
    
    print("Resultado:");
    format(round(raiz, 8), nsmall=8);
    print("Validez:");
    validez
```

Respuesta:

```r
    "Convergencia:"
         1
        0.4536424
        0.3497373
        0.1619293
        0.02767229
        0.0007669302
        
        validez = raiz*raiz;
        Resultado:
        2.64575131
        Validez:
        7
```
Gráfica:

![Grafica](https://github.com/SebastianTrianaP/Analisis-Numerico-1826-Juan-Sebastian-Triana-Perez/blob/master/Taller-1/graficap3.JPG)

Gráfica de error por numero de iteraciones.

6.
Eficiencia de un algoritmo esta denotada por T(n)
a) Recorra el algoritmo con n=73
b)  Suponga que T(n) representa la cantidad de operaciones aritméticas de división que se realizan para resolver el problema de tamaño n. Encuentre T(n) y exprésela con la notación O( ) Para obtener T(n) observe el hecho de que cada ciclo el valor de n se recude aproximadamente a la mitad.

Codigo:

```r
    sextopunto<-function(n){
      while (n>0){
        d<- n%%2;
        n<- trunc(n/2);
        print(d);
        #print(n);
      }
    }
    sextopunto(5);
    plot(funcionT, from=0, to=100);
    
    funcionT<-function(n){
      
      return(log2(n)+1);
    }
```

Explicación:

```r
     T(n)= T(n/2)+1
         =(T(n/4)+1)+1
         = T(n/4)+2
         =(T(n/8)+1)+2
         = T(n/8)+3
         ...
         = T(n/(2^i))+i
         ...
         =T(n/(2^(log(n))))+log 2(n)
         =T(1)+log2(n)
    Por tanto, T(n) es O(log n)
```

Gráfica:

![Grafica](https://github.com/SebastianTrianaP/Analisis-Numerico-1826-Juan-Sebastian-Triana-Perez/blob/master/Taller-1/Grafica1.JPG)

Gráfica generada en el programa, función f(x)=log2(t)+1.

7.
Utilice el método de Newton para resolver el problema, muestre gráficamente cómo se comporta la convergencia a la solución.
Una particula se mueve en el espacio con el vector de posición R(t) = (2cos(t)), sen(t), 0). Se requiere conocer el tiempo en el que el objeto se encuentra más cerca del punto P(2,1,0). Utilice el método de Newton con cuatro decimales de precisión.

Codigo:

```r
    distancia=expression (sqrt ((2*cos(x)-2)^2+(sin*(x)-1)^2))
    f=D(distancia,"x")
    df<-expression(3*sin(x)*cos(x)-4*sin(x)+cos(x))
    df1=D(df,"x")
    Fx<-function(x) eval.parent(df,x)
    F1x<-function(x) eval.parent(df1,x)
    newton <- function(x) {
       for(i in 1:5) {
         x<-x-Fx(x)/F1x(x)
         if (Fx(x) == 0) break
         error<-abs(Fx(x)/F1x(x))
         cat("X=",round(x,4),"\t","E=",error,"\n")
       }
     }
    newton(0)
```

13.
Encuentre una fórmula iterativa de convergencia cuadrática y defina un intervalo de convergenica apropiado para calcular para calcular la raiz real n-ésima de un número real. El algoritmo solamente debe incluir operaciones aritmética elementales.

Codigo:

```r
    Fx <- function(x, b) {return (x*x-b)}
    F1x<- function(x, b){return(2*x)}
      
    
    newton <- function(x , b) {
        
      for(i in 1:10) {
        x<-x-Fx(x, b)/F1x(x, b)
        if (Fx(x, b) == sqrt(b)) break
        error<-abs(Fx(x, b)/F1x(x, b))
        cat("X=",x,"\t","E=",error,"\n")
      }
    }
    newton(2, 3)
```

Repuesta:

```r
    X= 1.75 	 E= 0.01785714 
    X= 1.732143 	 E= 9.204713e-05 
    X= 1.732051 	 E= 2.44585e-09 
    ...
```

15.
Se propone resolver la ecuación ... con el método del punto fijo.

a) Obtenga la ecuacón f(x)=0 resolviendo la integral

b) Mediante un gráfico aproximado, o evaluando directamenta, localice la raíces reales.

c) Proponga una ecuacion equivalente x= g(x) y determine el intervalo de convergencia para calcular una de las dos raices.

d) Del intervalo anterior, elija un valor inicial y realice 5 iteraciones. En cada ineración verifique que se cumple la condición de convergencia del punto fijo y estime el error de truncamiento con el último resultado.

Codigo:

```r
#Punto-a
f <- function(x) return(5*x-(exp(1)^x)-1)


#Punto-b

plot(f,from=-3,to=3)
abline(h=0,col="blue");
#raices: 0.5448804 y 2.396139


#Punto-c
g <- function(x) return((1+(exp(1)^x))/5)
#intervalo de [0,2] y [2,3]


#Punto-d

puntofijo <-function(g, x0){
  maxIter=100
  tol=1e-8
  k = 1
  repeat{
    x1 = g(x0)
    
    dx = abs(x1-x0)
    print(dx)
    x0 = x1
    cat("x_", k, "= ", x1, "\n")
    k = k+1
    #until
    if(dx< tol|| k > maxIter) break;
  }
  
  if( dx > tol ){
    cat("No hubo convergencia ")
  } else{
    cat("x* es aproximadamente ", x1, " con error menor que ", tol, "\n")
  }
  cat("Error de truncamiento", abs(dx-tol))
}

puntofijo(g,0)
puntofijo(g,2.5)
#Solo converge con la raiz de 0.5448804
```

Respuesta:

```r
> puntofijo(g,0)
[1] 0.4
x_ 1 =  0.4 
[1] 0.09836494
x_ 2 =  0.4983649 
[1] 0.0308406
x_ 3 =  0.5292055 
[1] 0.01031108
x_ 4 =  0.5395166 
[1] 0.003518894
x_ 5 =  0.5430355 
[1] 0.001209232
x_ 6 =  0.5442447 
[1] 0.0004165235
x_ 7 =  0.5446613 
[1] 0.0001435894
x_ 8 =  0.5448049 
[1] 4.951389e-05
x_ 9 =  0.5448544 
[1] 1.70755e-05
x_ 10 =  0.5448715 
[1] 5.888904e-06
x_ 11 =  0.5448773 
[1] 2.030955e-06
x_ 12 =  0.5448794 
[1] 7.004354e-07
x_ 13 =  0.5448801 
[1] 2.415663e-07
x_ 14 =  0.5448803 
[1] 8.331146e-08
x_ 15 =  0.5448804 
[1] 2.873249e-08
x_ 16 =  0.5448804 
[1] 9.909274e-09
x_ 17 =  0.5448804 
x* es aproximadamente  0.5448804  con error menor que  1e-08 
Error de truncamiento 9.072582e-11> puntofijo(g,2.5)
[1] 0.1364988
x_ 1 =  2.636499 
[1] 0.3563465
x_ 2 =  2.992845 
[1] 1.195623
x_ 3 =  4.188469 
[1] 9.195883
x_ 4 =  13.38435 
[1] 129938
x_ 5 =  129951.3 
[1] Inf
x_ 6 =  Inf 
[1] NaN
x_ 7 =  Inf 
Error in if (dx < tol || k > maxIter) break : 
  missing value where TRUE/FALSE needed
```
Gráfica: 

![Grafica](https://github.com/SebastianTrianaP/Analisis-Numerico-1826-Juan-Sebastian-Triana-Perez/blob/master/Taller-1/Graficap15.JPG)

Gráfica dgenerado por el programa, función f(x)=5*x-(exp(1)^x)-1.
