#n <- 3456
#nbin <- IntToBin(3456)
#cat(n, "->", nbin,  "->", strtoi(nbin, base = 2), '\n')
#3456 -> 110110000000 -> 3456

IntToBin<-function (n){
  if (n<0){
    stop("n doit être positif")
  }
  if (n==0){
    return('0')
  }
  if (n==1){
    return('1')
  }
  if (n%%2==0){
    return(paste(IntToBin(n/2),'0',sep=''))
  }
  if (n%%2==1){
    return(paste(IntToBin((n-1)/2),'1',sep=''))
  }
}

n <- 3456
nbin <- IntToBin(3456)
cat(n, "->", nbin,  "->", strtoi(nbin, base = 2), '\n')




#n <- 3456
#nbin <- IntToBin(3456)
### On utilise maintenant la fonction inverse
#cat(n, "->", nbin,  "->", BinToInt(nbin), '\n')
#3456 -> 110110000000 -> 3456

BinToInt<-function (nbin){
  

  if (nbin=='0'){
    return(0)
  }
  if (nbin=='1'){
    return(1)
  }
  if (substr(nbin,nchar(nbin),nchar(nbin))=='0'){
    return(BinToInt(substr(nbin,1,nchar(nbin)-1))*2)
  }
  if (substr(nbin,nchar(nbin),nchar(nbin))=='1'){
    return(BinToInt(substr(nbin,1,nchar(nbin)-1))*2+1)
  }
}

n <- 3456
nbin <- IntToBin(3456)
## On utilise maintenant la fonction inverse
cat(n, "->", nbin,  "->", BinToInt(nbin), '\n')