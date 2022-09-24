hypotenus <- function(a, b) {
  return(sqrt(a**2 + b**2))
}

hypotenus(3, 4)

demandehypo <- function() {
  a <- as.numeric(readline("a="))
  b <- as.numeric(readline("b="))
  return(hypotenus(a, b))
}

affichehypo <- function() {
  print(demandehypo())
  return
}

affichehypo()