#f'(a) x a +p
# avec p = f(a) -f'(a) x a
#equation y=f′(a)(x−a)+f(a)
#y=f′(a)(x−a)+f(a)=0
#x−a=−f(a)/f′(a)
#x=a−f(a)/f′(a)
#f(x)=x^2-r
#f'(x)=2x
#x=a−f(a)/f′(a)
#x=a−(x^2-r)/(2x)





derivee<-function(f,a,h=1/2**10){
  (f(a+h)-f(a))/h
}
fonction<-function(x){return(x^2-2)}
Newtown <- function(f,a,h){
  d<-a
  while (d>h || d==a){
    cat(sprintf("a=%.15f \n",a))
    x<- a-f(a)/derivee(f,a)
    d<-x-a
    a<-d

  }
}

Newtown(fonction,2,1/2**5)


