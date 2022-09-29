
facRec<-function(n){
  if(n==0){
    return(1)
  }else{
    return(n*facRec(n-1))
  }
}
facRec(5)

facIter<-function(n){
  res<-1
  for(i in 1:n){
    res<-res*i
  }
  return(res)
}
facIter(5)

facRec(1000)


system.time(replicate(1000,facRec(100)))
system.time(replicate(1000,facIter(100)))



afficheFac<-function(n){
  for(i in 0:n){
    cat(i,"!=",facRec(i),"\n")
  }
}
#11 operation
afficheFac(10)

afficheFac2<-function(n){
  acc=1
  cat(0,"!=",acc,"\n")
  for(i in 1:n){
    acc<-acc*i
    cat(i,"!=",acc,"\n")
  }
}

#12 operation
afficheFac2(10)