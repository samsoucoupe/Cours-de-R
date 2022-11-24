#-------------------------------------------------------------#
# Description: Estimation de pi par la méthode de Monte Carlo
#-------------------------------------------------------------#

#-------------------------------------------------------------#
#Parti booléen pour les partie

section_pi<-FALSE


#Comment tirer uniformément au hasard n points dans un rectangle ? Indication: utiliser la primitive runif, qui permet de tirer uniformément au hasard un ou plusieurs points dans un intervalle.

#runif(n, min = 0, max = 1)

#Comment faire pour savoir si un point de coordonnées (x,y) appartient ou non au cercle de centre (0,0) et de rayon R ?
#(x,y)€ cercle => x²+y²<=R²


#Définir une fonction mc.pi qui prend en argument un entier n, et renvoie une valeur approchée de π, obtenue à l’aide de la méthode de Monte Carlo, et avec n points tirés uniformément au hasard.

mc.pi<-function(n){
  #point alea
  x<-runif(n, min = 0, max = 1)
  y<-runif(n, min = 0, max = 1)
  R<-1 #rayon du cercle
  N<-0 #nombre de points dans le cercle
  for (i in 1:n){
    if (x[i]^2+y[i]^2<=R^2){N<-N+1} #si le point est dans le cercle
  }
  return(4*N/n)
}

#function test grandeur de mc.pi
testmc.pi<-function(n){
  s<-1:n
  s<-10**s
  res<-sapply(s,mc.pi)
  return(res)
}

#testmc.pi(8)

#Estimations
#Dans cette section, vous allez définir une matrice PIE de taille t*p (avec t=50 et p=7), contenant des estimations de π. Plus précisément, la coordonnée (i,j) de PIE (avec 1<=i<=t et 1<=j<=p) doit contenir une estimation de π effectuée avec n=10**j points. Ainsi, la j-ième colonne de PIE contient t estimations de π, toutes effectuées avec n=10**j points. Toutes les estimations seront faites de façon indépendante les unes des autres.
if(section_pi){ t <- 50
  p <- 7
  PIE <- matrix(0, t, p)
  for (i in 1:t) {
    for (j in 1:p) {
      PIE[i, j] <- mc.pi(10**j)
    }
  }
  #Dans cette section, vous allez définir un vecteur tE de taille p contenant le temps moyen mis pour obtenir de telles estimations. Plus précisément, la j-ième coordonnée du vecteur tE (avec 1<=j<=p) doit contenir le temps moyen mis pour effectuer une estimation de π, chacune de ces estimations étant effectuée avec n=10**j points.
  #Indication: utiliser la primitive system.time, qui renvoie le temps mis pour évaluer un expression donnée et la primitive replicate qui répète l’évaluation d’une expression.

  tE <- system.time(replicate(p, mc.pi(10**j)))
  tE


  #Erreur relative
  #Quelle est la formule pour l’erreur relative entre une valeur et son estimation ? Définir une matrice ERR de taille t*p dont la coordonnée (i,j) est l’erreur relative entre π et son estimation PIE[i,j].

  #la valeur absolue
  ERR <- abs(PIE / pi - 1)

  par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) + 0.1)
  boxplot(ERR, main = 'Erreur relative sur PI', log = 'y', xlab = '#points', ylab = 'Rel. Error')
  plot(10^(1:p), tE, type = 'b', main = 'Temps moyen d\'une simulation', log = 'x', xlab = '#points', ylab = 'Time') }





## Definition d'une fonction très utile
creer_polygone <- function (x,y) {
  matrix(c(x, x[1], y, y[1]), ncol=2,dimnames=list(c(), c("x","y")))
}

carre <- creer_polygone(c(10,10,90,90), c(30, 70, 70, 30))
## Une permutation cyclique des points donne le même polygone
carre <- creer_polygone(c(10,90,90,10), c(70, 70, 30, 30))
## En revanche, le code suivant ne définit pas un rectangle,
## mais un polygone dont les arêtes se croisent.
papillon <- creer_polygone(c(10,90,10,90), c(30,70,70,30))
## pour finir, voici un losange.
losange <- creer_polygone(c(50,10,50,90),c(30,50,70,50))

print(carre)
#affiche un plot du carre
#+BEGIN_SRC R :exports both :file act07/dessin_poly.jpg :width 300 :height 300 :session poly plot(carre, type=’l’) lines(papillon -1, type=’b’, col=’firebrick’) lines(losange, type=’l’, col=’darkblue’) #+END_SRC R

dessin_polynome<-function(polynome){
    plot(polynome, type="l")
    lines(polynome -1, type="b", col="firebrick")
    lines(polynome, type="l", col="darkblue")
}

dessin_polynome(carre)

#Définir une fonction reg_poly <- function(n, r=1) { ... } qui prend en argument un entier n, un réel strictement positif r (de valeur 1 par défaut), et qui renvoie un polygone p vérifiant:
#le polygone p a n côtés,
#il est inscrit dans un cercle de centre (0,0) et de rayon r.

reg_poly<-function(n, r=1){
  #Tous les sommets d’un polygone régulier peuvent être engendrés à partir d’un seul sommet et les images successives d’une rotation: quel est l’angle de cette rotation ? Donnez-le en radians et en fonction du nombre n de côtés du polygone.
  #Quelles sont les coordonnées cartésiennes (x,y) d’un point dont les coordonnées polaires sont (r,theta)?
  #Tester votre fonction reg_poly en dessinant un exemple de polygone régulier (vous pouvez utiliser la fonction dessin_polygone définie précédemment).
  theta<-2*pi/n
  x<-r*cos(theta)
  y<-r*sin(theta)
  return(creer_polygone(x,y))

}

dessin_polynome(reg_poly(5, 1))