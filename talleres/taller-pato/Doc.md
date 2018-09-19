# Pato volando

###### Realizado por : Miguel Beltran y Sebastian Triana

Instalar el paquete pracma;
Se usan las funciones: 
- pchip(xi, yi, x)
- pchipfun(xi, yi)

[Documentación](https://cran.r-project.org/web/packages/pracma/pracma.pdf "Documentación"), pagina 246.

## Codigo
```r
#Pato volando

x = c(0.9, 1.3, 1.9, 2.1, 2.6, 3.0, 3.9, 4.4, 4.7, 5, 6.0, 7.0, 8.0, 9.2, 10.5, 11.3, 11.6, 12.0, 12.6, 13.0, 13.3)
y = c(1.3, 1.5, 1.85, 2.1, 2.6, 2.7, 2.4, 2.15, 2.05, 2.1, 2.25, 2.3, 2.25, 1.95, 1.4, 0.9, 0.7, 0.6, 0.5, 0.4, 0.25)


pchip(x, y, seq(1, 5, by = 1))
fp <- pchipfun(x, y)





x2 = c(0.817, 0.897, 1.022, 1.191, 1.510, 1.834, 2.264, 2.962, 3.624, 4.202, 4.499, 4.779, 5.109, 5.527)
y2 = c(1.180, 1.065, 1.023, 1.010, 1.032, 1.085, 1.192, 1.115, 1.087, 1.100, 0.830, 0.608, 0.350, 0.106)

pchip(x2, y2, seq(1, 5, by = 1))
fp2 <- pchipfun(x2, y2)

  


x3 = c(4.659, 4.865, 5.085, 5.261, 5.387, 5.478, 5.527)
y3 = c(-5.161, -4.741, -3.933, -2.951, -1.970, -0.981, 0.106)
pchip(x3, y3, seq(1, 5, by = 1))
fp3 <- pchipfun(x3, y3)




x4 = c(4.659, 4.750, 4.990, 5.289, 5.560, 5.839, 6.113, 6.606, 6.916, 7.305, 7.563, 7.802, 7.983)
y4 = c(-5.161, -5.259, -5.284, -5.268, -5.161, -4.982, -4.769, -4.286, -3.911, -3.213, -2.670, -2.176, -1.655)
pchip(x4, y4, seq(1, 5, by = 1))
fp4 <- pchipfun(x4, y4)


x5 = c(8.141, 8.473, 8.832, 9.337, 9.887, 10.572, 10.995, 11.501, 11.923, 12.364, 12.763, 13.300)
y5 = c(-1.138, -0.434, -0.514, -0.494, -0.382, -0.005, -0.090, -0.085, -0.030, 0.093, 0.120, 0.250)
pchip(x5, y5, seq(1, 5, by = 1))
fp5 <- pchipfun(x5, y5)




x6 = c(2.663, 2.700, 2.805, 2.886)
y6 = c(2.202, 2.279, 2.293, 2.222)
pchip(x6, y6, seq(1, 5, by = 1))
fp6 <- pchipfun(x6, y6)



x7 = c(2.663, 2.720, 2.826, 2.886)
y7 = c(2.202, 2.130, 2.143, 2.222)
pchip(x7, y7, seq(1, 5, by = 1))
fp7 <- pchipfun(x7, y7)

plot(fp,from=0.817, to = 13.3,xlim=c(0,13.3),ylim=c(-5.161,2.6), col="red")
par(new=TRUE)
plot(fp3,from=4.659,to=5.527,xlim=c(0,13.3),ylim=c(-5.161,2.6), col="red")
par(new=TRUE)
plot(fp4,from=4.659,to=7.983,xlim=c(0,13.3),ylim=c(-5.161,2.6), col="red")
par(new=TRUE)
plot(fp5,from=7.983,to=13.3,xlim=c(0,13.3),ylim=c(-5.161,2.6), col="red")
par(new=TRUE)
plot(fp6,from=2.663,to=2.886,xlim=c(0,13.3),ylim=c(-5.161,2.6), col="red")
par(new=TRUE)
plot(fp7,from=2.663,to=2.886,xlim=c(0,13.3),ylim=c(-5.161,2.6), col="red")
par(new=TRUE)
plot(fp2,from=0.817, to= 5.527,xlim=c(0,13.3), ylim=c(-5.161,2.6),col="red",  axes=TRUE) 
title(main="Pato volando")
abline(a=0, b=0)

```

## Imagen

![Grafica](https://github.com/SebastianTrianaP/Analisis-Numerico-1826-Juan-Sebastian-Triana-Perez/blob/master/Pato%20volando/Pato.JPG)

