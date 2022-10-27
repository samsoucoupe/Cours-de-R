#input

entre<- c(18,10,0.00000000,0.50000000,1.00000000,2048.00000000,0.00781250,0.50781250,1.00781250,2048.00781250,2147483647.00000000,-0.00000000,-0.50000000,-1.00000000,-2048.00000000,-0.00781250,-0.50781250,-1.00781250,-2048.00781250,-2147483647.00000000)
#output

sortie<-c(0,5,1,2,7,5,1,2,2,0,5,1,2,7,5,1,2,2)

#code

premierelt<-function(val){
  if (typeof(val)!="string"){val<-toString(val,0)}
  return(substr(val,1,1))
}

f<-function(n,base){
  if  (n==0) return(0)
  n<-abs(n)
  while (!(n<=base && n>=1)){

    if (n<1) n<-n*base
    if (n>base) n<-n/base

  }
  n<-as.integer(n)
  return(n)

}
#stopifnot(f(0,2)==0)
#stopifnot(f(0.2,10)==2)
#f(10,2)
#f(16,16)
#f(1600,16)

f(0.5,10)




test<-function(){
  vect_entree<-entre     #all entree withoaut the 2 first
  cat("vect_entree=",vect_entree,"\n")
}

test()

