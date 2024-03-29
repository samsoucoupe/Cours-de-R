library('ggplot2')

#Écrire une fonction Cor qui donne le coefficient de corrélation avec la loi de Benford en base b pour un pour un vecteur x de chiffres de poids fort.

#import du module firstdigit
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


FirstDigit_vec<-function(vect_entree){

  base<-vect_entree[2]
  vect_entree<-vect_entree[3:length(vect_entree)]
  return(sapply(vect_entree,FirstDigit,base))
}


#import de la loi de benford

BenfordLaw <- function(x, base = 10) {
  return((log(x+1, base) - log(x, base)))}

#input
#Cor(x = 1:9, base = 10)
#Cor(x = c(1, 1, 1, 1, 2, 2, 2, 3, 3), base = 4)
#output
#NA
#Message d'avis :
#Dans cor(y, z) : l'écart type est nul
#0.9719616
#calcul de la corrélation



convert_comptage<-function(vect_entree,base){
  res<-rep(0,(base-1))
  for (i in vect_entree){
    res[i]<-res[i]+1
  }
  res<-res/length(vect_entree)
    return(res)
}
cat(convert_comptage(c(1,1,1,1,3,3),4),'\n')

cat(convert_comptage(c(1,1,1,1,2,2,2,3,3),4),'\n')
cat(convert_comptage(1:9,10),'\n')
#cat("La loi de Benford en base 10 est: \n")
#BenfordLaw(c(1,2,3,4,5,6,7,8,9),10)
#cat("La loi de Benford en base 4 est: \n")
#BenfordLaw(convert_comptage(c(1,3,3,3,3),4),4)
#cat("fin\n")


Cor<-function(x,base){
    #calcul de la loi de benford

    x<-convert_comptage(x,base)
    #cat(x,'\n')

    #calcul de la loi de benford pour les chiffres de poids fort


    loi_benford_poids_fort<-BenfordLaw(1:(base-1),base)
    #cat(loi_benford_poids_fort,"\n")
    #calcul de la corrélation
    r<-cor(loi_benford_poids_fort,x)

    return(r)

}

testCor<-function(){
  #cat("Test de la fonction FirstDigit_vec avec l'entrée ",entree," et la base ",entree[2],"\n")
  stopifnot(
    Cor(x = 1:9, base = 10)==NA
    #Cor(x = c(1, 1, 1, 1, 2, 2, 2, 3, 3), base = 4)==0.9719616
  )
}

#Cor(x = c(1, 1, 1, 1, 2, 2, 2, 3, 3), base = 4)
#Cor(FirstDigit_vec(volcano),10)
#cat(1:9,'\n')
#Cor(x = 1:9, base = 10)
#testCor()

Hist <- function(x, base) {
  ## Build text elements
  title <- paste0(
    "Premiers chiffres en base ", base,
    "\nCorrélation avec Benford : ",  Cor(x, base)
  )
  df <- data.frame(digit = x)
  ggplot2::ggplot(df,  ggplot2::aes(x=digit)) +
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), color="black", fill="white", binwidth = 1) +
    ggplot2::stat_function(fun=BenfordLaw, args = list(base = base), ggplot2::aes(color="Benford Law")) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}
#Hist(x = c(1, 1, 1, 1, 2, 2, 2, 3, 3),4)

#x<-c(1, 1, 1, 1, 2, 2, 2, 3)
#table(x)

#cat(AirPassengers,'\n')

cat("volcano\n")
ProcessBenford(volcano, base = 10)
cat("AirPassengers\n")
ProcessBenford(AirPassengers, base = 10)
cat("sunspot.month\n")
ProcessBenford(sunspot.month, base = 10)
cat("rivers\n")
ProcessBenford(rivers, base = 8)