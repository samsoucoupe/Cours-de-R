SomCh<-function(n){
  acc=0
  while (n>0){
    acc<-acc+n%%10
    n<-n%/%10
  }
  return (acc)
}
SomCh(3456)

SomBin<-function(n){
  acc=0
  while (n>0){
    acc<-acc+n%%2
    n<-n%/%2
  }
  return (acc)
}
SomBin(3456)

SomCH<-function(n,base){
  acc=0
  while (n>0){
    acc<-acc+n%%base
    n<-n%/%base
  }
  return (acc)
}
SomCH(3456,2)
SomCH(3456,10)
as.hexmode(3456)
SomCH(3456, base = 16)


Renverser<-function(n){
  acc=0
  while (n>0){
    acc<-acc*10+n%%10
    n<-n%/%10
  }
  return (acc)
}
Renverser(3456)
Renverser(34560)


RenverserBase<-function(n,base){
  acc=0
  while (n>0){
    acc<-acc*base+n%%base
    n<-n%/%base
  }
  return (acc)
}

RenverserBase(3456,2)

RenverserBase(as.hexmode("ABC"), base = 16)