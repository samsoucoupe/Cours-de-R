#Utilisez la fonction sample pour créer une liste de 100 entiers compris entre 1 et 100 tirés au hasard avec remise.

x<-sample(1:100,100,replace=TRUE)

#Calculer le nombre d’éléments compris entre 1 et 100 qui n’appartiennent pas à cette liste.


absents<-function (n){
  x <- sample.int(n, n, replace=TRUE)
  missing <- !(1:n %in% x)

  return(sum(missing))
}

absents(100)

#Recommencer cette expérience un grand nombre de fois et calculer la moyenne du nombre d’absents.

abs <- 0
xperienc <- 1000
for(i in 1:xperienc){
  abs <- abs + absents(100)
}
abs <- abs/xperienc
cat("La moyenne du nombre d'absents est de ",abs,"\n")

