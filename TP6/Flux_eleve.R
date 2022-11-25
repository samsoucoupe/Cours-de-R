FluxEleves <- function(n, nA, nB, probAB, probBA) {
  na <- numeric(n)
  na[1] <- nA
  nab <- nA + nB
  for(i in seq(2,n)) {
    da <- round( probAB * na[i-1] )
    db <- round( probBA*(nab-na[i-1]))
    na[i] <- na[i-1] - da + db
    if(na[i] == na[i-1]) {
      na <- head(na, i)
      break
    }
  }
  return(na)
}

nA <- 2000
nB <- 8000
na <- FluxEleves(50, nA, nB, 0.1, 0.15)
nb <- nA + nB - na
print(na)
plot(na, type = 'b', xlab = "Année", ylab = "Effectif", lty = 1, pch = 1)
lines(nb, type = 'b', lty = 2, pch = 2)