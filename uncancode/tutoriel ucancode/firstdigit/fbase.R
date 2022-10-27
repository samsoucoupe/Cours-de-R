####################################################
## ÉCRIRE VOTRE CODE ICI
#####################################################




FirstDigit <- function(n,base){
  if  (n==0) return(0)
  n<-abs(n)
  while (!(n<=base && n>=1)){

    if (n<1) n<-n*base
    if (n>base) n<-n/base

  }
  n<-as.integer(n)
  return(n)

}

####################################################
## SCRIPT PRINCIPAL À NE PAS MODIFIER
####################################################

## Lire un vecteur (par défaut d'entiers) sur l'entrée standard.
## Capturer le flux d'entrée standard.
stream <- file("stdin","r")
## Lire le flux jusqu'à la fin dans un vecteur.
x <- scan(file=stream, what= numeric(), quiet=TRUE)
## Ferme proprement le flux (pour éviter un warning).
close(stream)

## On boucle sur les cas de test
for(n in tail(x, -2)) {
  cat(FirstDigit(n, x[2]), "\n", sep = "")
}

