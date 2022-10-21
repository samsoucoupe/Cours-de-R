## TableMult(0) ## error !
## TableMult(1.5) ## error !
#TableMult(4)
#4 x  1 =   4
#4 x  2 =   8
#4 x  3 =  12
#4 x  4 =  16
#4 x  5 =  20
#4 x  6 =  24
#4 x  7 =  28
#4 x  8 =  32
#4 x  9 =  36
#4 x 10 =  40

TableMult<-function(n){
  if (n<1 || n%%1!=0){
    stop("n doit être un entier positif")
  }
  if (n>9){
    stop("n doit être inférieur ou égal à 9")
  }
  cat(paste(n," x ",1:10," = ",n*(1:10)),sep='\n')
}
TableMult(4)
TableMult(10)