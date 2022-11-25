PlusPetitDiv <- function(n) {
  ## le plus petit diviseur de n dans [2,n-1], avec n >= 2
  if (n %% 2 == 0) return(2)
  d <- 3
  while(d*d <= n) {
    if (n %% d == 0) return(d)
    d <- d + 2
  }
  return(n)
}

PlusPetitDiv(35)

EstPremier <- function(n) {
  ## TRUE si n est premier, FALSE sinon
  if (n<2) return(FALSE)
  return (PlusPetitDiv(n)==n)
}

EstPremier(35)
EstPremier(29)

#100 premiers nombres premiers
cat("nombres premiers compris entre 2 et 100 :\n")
n <- 3
cat(2,"")
while (n <= 100) {
  if (EstPremier(n)) {
    cat(n,"")
  }
  n <- n + 2
}
cat("\n")