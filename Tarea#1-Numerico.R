#funcion

f<-function(x){
  return(((9.8*68.1)/x)*(1-(exp(1)^(-(x/68.1)*10)))-40);
}
#biseccion
biseccion<-function(a,b){
  k = ((a+b)/2);
  plot(f,from=a,to=b)
  abline(h=0,col="blue");
  i = 0;
  while(abs(f(k))>=(1.e-8)){
    
    i = i +1;
    if(f(a)*f(k)<0)
    {
      b=k;
    }
    else
    {
      a=k;
    }
    text(k,0,i,cex=0.8,col="red")
    
    k <- ((a+b)/2);
    cat("i=",i,"\tX=",k,"\n");
    i=i+1;
  }
  return (k);
  
}
#triseccion

triseccion<-function(a,b,c){
  
  k = ((a+b+c)/3);
  plot(f,from=a,to=c)
  abline(h=0,col="blue");
  i = 0;
  while(abs(f(k))>=(1.e-8)){
    
    i = i +1;
    if(f(a)*f(k)<0)
    {
        if(f(b)*f(k)<0)
        {
          a=b;
          b=(b+k)/2;
          c=k;
        }
      else
      {
        b=(a+k)/2;
        c=k;
      }
    }
    else
    {
      if(f(k)*f(b)<0)
      {
        a=k;
        c=b;
        b=(b+k)/2;
        
      }
      else
      {
        a=k;
        b=(c+k)/2;
      }
    }
    Sys.sleep(0.5)
    text(k,0,i,cex=0.8,col="red")
    
      k <- ((a+b+c)/3);
      cat("i=",i,"\tX=",k,"\n");
      i=i+1;
  }
  return (k);
}

#tetra

tetraseccion<-function(a,b,c,d){

  k = ((a+b+c+d)/4);
  plot(f,from=a,to=d)
  abline(h=0,col="blue");
  i = 0;
  
  k = biseccion(a,b);
  if(abs(f(k))>=(1.e-8)){
    k = biseccion(b,c);
      if(abs(f(k))>=(1.e-8)){
        k = biseccion(c,d);
      }
    }
    
    
  text(k,0,i,cex=0.8,col="red")
  cat("i=",i,"\tX=",k,"\n");
   
  
  return (k);
}

#pruebas
coeficiente = biseccion(1,100)
coeficiente
coeficiente = triseccion(1,50,100)
coeficiente
f(raiz)
