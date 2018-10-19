f<-expression(1)

x0=1
y0=1

picard<-function(x0,y0,f,n){
  require(Ryacas) #para integrales no definidas
  t<-Sym("t")
  
  c=0
  
  xi = expression(y0)
  while(c<n){
    t<-Sym("t")
    fi = Integrate(f,t)
    print(fi)
    t<-xi
    g = Eval(fi)
    
    xi = Sym(xi+fi-g)
    
    f = xi
    c= c+1;
  }
  
  return(xi) 
  
}


g <- picard(x0,y0,f,3)
g
