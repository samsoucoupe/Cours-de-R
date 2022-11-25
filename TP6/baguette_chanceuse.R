argent <- c(10, 10, 20, 50, 50, 50, 100, 200, 200)
paye<- function(take, money, amount) {
  sum(sample(money, take, replace=FALSE)) >= amount
}

n<-10**6
proba<-mean(replicate(n,paye(2,money=argent,amount=100)))
cat("proba=",round(proba,3),"\n")


