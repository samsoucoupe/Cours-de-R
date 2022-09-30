pgcdREC<-function(a,b){
  if (b==0){
    return(a)
  }
  else{
    return(pgcdREC(b,a%%b))
  }
}
pgcdREC(12,18)
pgcdITER<-function(a,b){
  while (b!=0){
    c<-a%%b
    a<-b
    b<-c
  }
  return (a)
}

pgcdITER(12,18)