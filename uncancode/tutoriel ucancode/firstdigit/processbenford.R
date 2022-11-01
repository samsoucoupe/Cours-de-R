source('fbonus.R')



#Écrire une fonction ProcessBenford qui prend en argument un vecteur numérique x et une base base et effectue les étapes suivantes.
#Elle vérifie la validité de ses arguments.
#Elle calcule le vecteur des premiers chiffres des nombres de x écrit en base base.
#Elle renvoie un histogramme.
testvalidite<-function(x,base){
    if (base<2){ cat("La base doit être supérieure ou égale à 2 \n")
      return(FALSE)}
    if (length(x)==0) stop("x doit être non vide")
    if (any(x<0)) stop("x doit être positif")
    if (any(x!=as.integer(x))) stop("x doit être un vecteur d'entiers")
    if (any(x==0)) stop("x doit être non nul")
    return(TRUE)
}
#Écrire une fonction ProcessBenford qui prend en argument un vecteur numérique x et une base base et effectue les étapes suivantes.
ProcessBenford<-function(x,base){
  #Elle vérifie la validité de ses arguments.
  if (testvalidite(x,base)==FALSE) return(NA)
  #Elle calcule le vecteur des premiers chiffres des nombres de x écrit en base base.
  premiers_chiffres<-FirstDigit_vec(c(length(x)-2,base,x))
  #Elle renvoie un histogramme.
  Hist(premiers_chiffres,base)
}

ProcessBenford(1:9,10)
ProcessBenford(c(1, 1, 1, 1, 2, 2, 2, 3, 3), 4)