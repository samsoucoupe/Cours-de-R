serie<-function(r1,r2){
  return(r1+r2)
}

parallele<-function(r1,r2){
  return((r1*r2)/(r1+r2))
}

Circuit1<-function(r1,r2,r3){
  return(serie(r1,parallele(r2,r3)))
}
Circuit1(5,100,25)

Circuit2<-function(r1,r2,r3){
  return(serie(parallele(r1,parallele(r2,r3)),parallele(r2,r3)))
}

Circuit2(5,100,25)