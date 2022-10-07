pgcdITER<-function(a,b){
  if (a ==0 || b==0){
    return(a+b)
  }
  while (b!=0){
      c<-a%%b
      a<-b
      b<-c
    }
    return (a)
}


testpgcd<-function(pgcd){
  system.time(
    stopifnot(
      pgcd(1,1)==1,
      pgcd(8,1)==1,
      pgcd(12,1)==1,
      pgcd(87 ,1)==1,
      pgcd(116 ,1)==1,
      pgcd(1, 8)==1,
      pgcd(8 ,8)==8,
      pgcd(12 ,8)==4,
      pgcd(87 ,8)==1,
      pgcd(116 ,8)==4,
      pgcd(1 ,12)==1,
      pgcd(8 ,12)==4,
      pgcd(12 ,12)==12,
      pgcd(87 ,12)==3,
      pgcd(116 ,12)==4,
      pgcd(1 ,87)==1,
      pgcd(8 ,87)==1,
      pgcd(12 ,87)==3,
      pgcd(87 ,87)==87,
      pgcd(116 ,87)==29,
      pgcd(1 ,116)==1,
      pgcd(8 ,116)==4,
      pgcd(12 ,116)==4,
      pgcd(87 ,116)==29,
      pgcd(116 ,116)==116
    ))
}

testpgcd(pgcdITER)