JeuHazard<-function (){
  haz<-(sample(1:6,3))
  haz<-sum(haz)
  if (haz==18){
    return(50)
  }
  else if (10<haz && 17>haz){
    return(5)
  }
  else{
    return(0)
  }
}
table(replicate(216,JeuHazard()))
i<-0
while (JeuHazard()!=50){
  i<-i+1
}
print(i)

#proba win 50€ 1/6*1/6*1/6=1/216