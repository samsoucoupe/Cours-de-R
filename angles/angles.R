
angles <- function(a,b,c,d) {
  if (a==c & b==d) return(4)
  if (a^2+b^2==c^2+d^2 | b^2+c^2==d^2+a^2) return(2)
  rots <- matrix(c(a,b,c,d,b,c,d,a,c,d,a,b,d,a,b,c), ncol=4, byrow=TRUE)
  if (any((rots[,1]-rots[,3])^2+rots[,2]^2==rots[,4]^2)) return(2)
  if (any(rots[,1]>rots[,2]+rots[,3]+rots[,4])) return(-1)
  if (any((rots[,3]-rots[,4])^2<=rots[,1]^2+rots[,2]^2 & rots[,1]^2+rots[,2]^2<=(rots[,3]+rots[,4])^2)) return(1)
  return(0)
}


entree<- readLines("angles/angles.txt")
n <- as.numeric(entree[1])+1
for (i in 2:n) {
  a<-as.numeric(strsplit(entree[i], " ")[[1]][1])
  b<-as.numeric(strsplit(entree[i], " ")[[1]][2])
  c<-as.numeric(strsplit(entree[i], " ")[[1]][3])
  d<-as.numeric(strsplit(entree[i], " ")[[1]][4])

  cat(sprintf("%d\n", angles(a,b,c,d)))
}





