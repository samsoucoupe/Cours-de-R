# def des plus petites fonctions unique pour chacun des cas

angles <- function(a,b,c,d) {
  if (a==c & b==d) return(4)
  if (a^2+b^2==c^2+d^2 | b^2+c^2==d^2+a^2) return(2)
  rots <- matrix(c(a,b,c,d,b,c,d,a,c,d,a,b,d,a,b,c), ncol=4, byrow=TRUE)
  if (any((rots[,1]-rots[,3])^2+rots[,2]^2==rots[,4]^2)) return(2)
  if (any(rots[,1]>rots[,2]+rots[,3]+rots[,4])) return(-1)
  if (any((rots[,3]-rots[,4])^2<=rots[,1]^2+rots[,2]^2 & rots[,1]^2+rots[,2]^2<=(rots[,3]+rots[,4])^2)) return(1)
  return(0)
}

CarreouRectangle<- function(a,b,c,d) { return(a==b & c==d)}
RectangleouLosange<- function(a,b,c,d) { return(a^2+b^2==c^2+d^2 | b^2+c^2==d^2+a^2)}
Trapeze<- function(a,b,c,d) { return((a-b)^2+c^2==d^2)}

anglessimplifie <- function(a,b,c,d) {
  if (CarreouRectangle(a,b,c,d)) { return(4) }
  if (RectangleouLosange(a,b,c,d)) { return(2) }
  rots <- matrix(c(a,b,c,d,b,c,d,a,c,d,a,b,d,a,b,c), ncol=4, byrow=TRUE)
  if (any(Trapeze(rots[,1],rots[,2],rots[,3],rots[,4]))) { return(2) }
}

entree<- readLines("angles/angles.txt")
resultat<- readLines("angles/resultat.txt")
n <- as.numeric(entree[1])+1
test<-function (ang){
  for (i in 2:n) {
    a<-as.numeric(strsplit(entree[i], " ")[[1]][1])
    b<-as.numeric(strsplit(entree[i], " ")[[1]][2])
    c<-as.numeric(strsplit(entree[i], " ")[[1]][3])
    d<-as.numeric(strsplit(entree[i], " ")[[1]][4])
    res<-as.numeric(strsplit(resultat[i-1], " ")[[1]][1])
    if (ang(a,b,c,d)==res) {
      cat("OK \n")
    }
    else {
      cat("KO",a,b,c,d,ang(a,b,c,d),res," \n")
    }
    }

  }
cat("angles \n")
test(angles)
cat("angles simplifie \n")
test(anglessimplifie)





