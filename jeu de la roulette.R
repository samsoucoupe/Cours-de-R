print("jeu de la roulette bonus")

LireType <-function () {
  ## Le return est implicite !
  menu ( ## choisir une option dans l'interpréteur
    title = " Veuillez saisir le type de la mise :",
    choices = c ( " Plein " , " Transversale " , " Colonne ","Douzaine " , " Pair - Impair " , " Manque - Passe " ) )
}

LireNumero <-function () {
  cat ( " Veuillez saisir un numéro de la mise : \ n " )
  numero <- scan ( n = 1 , quiet = TRUE )
  stopifnot ( numero >= 0 , numero <= 36 )
  return ( numero )
}

LireMontant <- function () {
  cat ( " Veuillez saisir le montant de la mise : \ n " )
  montant <- scan ( n = 1 , quiet = TRUE )
  stopifnot ( montant >= 0 )
  return ( montant )
}

TirerNumeroGagnant <- function () {
  return ( sample ( 0:36 , size = 1 ) )
}

GainPlein <- function (numero,gagnant){
  if (numero == gagnant) return (35)
  else return (0)
}

GainTransversale <- function (numero,gagnant){
  if ((numero-1)%/%3 == (gagnant-1)%/%3) return (11)
  else return (0)
}

GainColonne <- function (numero,gagnant){
  if ((numero-1)%%3 == (gagnant-1)%%3) return (2)
  else return (0)
}

GainDouzaine <- function (numero,gagnant){
  if ((numero-1)%/%12 == (gagnant-1)%/%12) return (2)
  else return (0)
}

GainPairImpair <- function (numero,gagnant){
  if (numero%%2 == gagnant%%2) return (1)
  else return (0)
}

GainManquePasse <- function (numero,gagnant){
  if (numero <= 18 && gagnant <= 18) return (1)
  else if (numero > 18 && gagnant > 18) return (1)
  else return (0)
}

GainRoulette <- function (type,numero,gagnant){
  if(type == 1) GainPlein(numero,gagnant)
  else if (gagnant ==0) return (0)
  else if (type == 2) GainTransversale(numero,gagnant)
  else if (type == 3) GainColonne(numero,gagnant)
  else if (type == 4) GainDouzaine(numero,gagnant)
  else if (type == 5) GainPairImpair(numero,gagnant)
  else if (type == 6) GainManquePasse(numero,gagnant)
  else return(0)
}



JouerRoulette<-function () {
  ## L'utilisateur saisit sa mise
  type <- LireType ()
  numero <- LireNumero ()
  montant <- LireMontant ()
  ## Le joueur peut miser sur 0 uniquement pour Plein .
  stopifnot ( type < 1 || numero > 0 )
  ## Le croupier tire le numéro gagnant
  gagnant <- TirerNumeroGagnant ()
  cat ( " Le numéro gagnant est le " , gagnant , " .\n " , sep="")
        ## Le croupier détermine le montant du gain .
        gain <-montant*GainRoulette(type,numero,gagnant)
        ## On affiche le montant du gain
        cat ( " Vous avez gagné " , gain , " .\n " , sep="")
        ## On renvoie le montant du gain
        return (gain)
}

GainTransversale(3,1)
GainTransversale(3,6)
GainColonne(3,1)
GainColonne(3,6)
GainDouzaine(1,11)
GainDouzaine(1,16)
GainManquePasse(1,19)
GainManquePasse(21,19)
GainPairImpair(1,11)
GainPairImpair(1,16)




#unacode
GainPlein <- function (numero,gagnant){
  if (numero == gagnant) return (35)
  else return (0)
}

GainTransversale <- function (numero,gagnant){
  if ((numero-1)%/%3 == (gagnant-1)%/%3) return (11)
  else return (0)
}

GainColonne <- function (numero,gagnant){
  if ((numero-1)%%3 == (gagnant-1)%%3) return (2)
  else return (0)
}

GainDouzaine <- function (numero,gagnant){
  if ((numero-1)%/%12 == (gagnant-1)%/%12) return (2)
  else return (0)
}

GainPairImpair <- function (numero,gagnant){
  if (numero%%2 == gagnant%%2) return (1)
  else return (0)
}

GainManquePasse <- function (numero,gagnant){
  if (numero <= 18 && gagnant <= 18) return (1)
  else if (numero > 18 && gagnant > 18) return (1)
  else return (0)
}

GainPlein <- function (numero,gagnant){return (35*(numero == gagnant))}

GainTransversale <- function (numero,gagnant){return (11*((numero-1)%/%3 == (gagnant-1)%/%3))}

GainColonne <- function (numero,gagnant){return (2*((numero-1)%%3 == (gagnant-1)%%3))}

GainDouzaine <- function (numero,gagnant){return (2*((numero-1)%/%12 == (gagnant-1)%/%12))}

GainPairImpair <- function (numero,gagnant){return ((numero%%2 == gagnant%%2))}

GainManquePasse <- function (numero,gagnant){return ((numero <= 18 & gagnant <= 18)||(numero > 18 & gagnant > 18))}

GainRoulette <- function (type,numero,gagnant){

  if(type == 1) GainPlein(numero,gagnant)
  else if (gagnant ==0) return (0)
  switch(type,
         GainPlein(numero,gagnant),
         GainTransversale(numero,gagnant),
         GainColonne(numero,gagnant),
         GainDouzaine(numero,gagnant),
         GainPairImpair(numero,gagnant),
         GainManquePasse(numero,gagnant),
         0)
}


MontantGainRoulette <- function (type, numero, montant, gagnant) {
  ## La fonction principale renvoyant le montant du gain
  ## et prenant en arguments une mise et un numéro gagnant.
  if(numero==0 & type>1){ return(NA)}
  else if(type <= 0 || type > 6){ return(NA)}
  else if(numero < 0 || numero > 36) {return(NA)}
  else if(montant < 0) {return(NA)}
  else if(gagnant < 0 || gagnant >36) {return(NA)}
  else { return(montant * GainRoulette(type, numero, gagnant)) }
}

MontantGainRoulette(1,1,1,1)
MontantGainRoulette(1,1,0,1)
MontantGainRoulette(1,1,1,2)
MontantGainRoulette(2,1,1,3)
MontantGainRoulette(2,1,1,4)
MontantGainRoulette(0,1,1,1)
MontantGainRoulette(1,-1,1,1)
MontantGainRoulette(1,1,-1,1)
MontantGainRoulette(1, 1, 1, 37)



