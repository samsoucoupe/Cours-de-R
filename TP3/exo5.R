pgcdREC<-function(a,b){
  if (b==0){
    return(a)
  }
  else{
    return(pgcdREC(b,a%%b))
  }
}

pgcdITER<-function(a,b){
  if (a ==0 || b==0){
    return(a+b)
  }
  else{

  while (b!=0){
    c<-a%%b
    a<-b
    b<-c
  }
  return (a)
  }
}

pgcdSubRec<-function(a,b){
  if (a==b ||a ==0 || b==0){
    return(a)
  }
  else if (a>b){
    return(pgcdSubRec(a-b,b))
  }
  else{
    return(pgcdSubRec(a,b-a))
  }
}

pgcdSubIter<-function(a,b){
  if (a ==0 || b==0){
    return(a+b)
  }
  else{
    while (a!=b){
    if (a>b){
      a<-a-b
    }
    else{
      b<-b-a
    }
    }
  }
  return(a)
}

TestPGCD <- function(pgcd) {
  system.time(
    stopifnot(
      pgcd(12,8) == 4,
      pgcd(8,12) == 4,
      pgcd(87,116) == 29,
      pgcd(2,0) == 2
      ## Ajouter des tests
      ## ...
    ))
}

#print("Test pgcdREC")
#TestPGCD(pgcdREC)
#print("Test pgcdITER")
#TestPGCD(pgcdITER)
#print("Test pgcdSubRec")
#TestPGCD(pgcdSubRec)
#print("Test pgcdSubIter")
#TestPGCD(pgcdSubIter)


PerfPGCD <- function(pgcd) {
  n <- 2**seq(1, 31, 3)
  n <- c(n-1, n, 2**32-1)
  df <- expand.grid(a = n, b = n)
  print(df) ## afficher les cas de tests
  system.time(apply(df, 1, function(x) pgcd(x[1], x[2])))
}
print("Perf pgcdREC")
#PerfPGCD(pgcdREC)
print("Perf pgcdITER")
#PerfPGCD(pgcdITER)
print("Perf pgcdSubRec")
#PerfPGCD(pgcdSubRec)
print("Perf pgcdSubIter")
#PerfPGCD(pgcdSubIter)

afficherfraction<-function(a,b){
  if (a==0 || b==0){
    cat(0)
  }
  if( pgcdITER(a,b)==1){
    cat(a,"/",b)
  }
  else{
    cat(a,"/",b," --> ",a/pgcdITER(a,b),"/",b/pgcdITER(a,b),"\n")
  }
}

afficherfraction(12,8)
afficherfraction(8,12)
afficherfraction(87,116)
afficherfraction(2,0)
afficherfraction(51,85)