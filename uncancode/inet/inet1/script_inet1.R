#Input
#Une adresse internet en base 16 et la base (10 ou 16) de sa représentation pointée séparés par un espace. Les chiffres hexadécimaux sont donnés en majuscules (pour les lettres).

#Output
#La représentation pointée en base 10 ou 16 de cette adresse. En base 16, les 0 ne sont pas omis pour garder la représentation binaire sous jacente.

entree <- readLines("./uncancode/inet/inet1/entree.txt")
sortie <- readLines("./uncancode/inet/inet1/sortie.txt")

address<-c("C0290614","C0290614","0000000000","0000000000")
base<-c(16,10,16,10)
sortie<-c("C0.29.06.14","192.41.6.20","00.00.00.00","00.00.00.00")







#seprarer la chaines tous les 2 caractères
separateur<-function (chaine,n=2){
  taille<-nchar(chaine)
  ch<-c()
    if(taille%%n==0){
      #separer la chaine en n caractères
        for(i in seq(1,taille,n)){
            ch[i]<-substr(chaine,i,i+1)
        }
      #del les NA
      ch<-ch[!is.na(ch)]
    }
    else{
        ch<-NULL
    }
    return(ch)
}

#convert 16 to base 10
Convert16ToBase10<-function(val){
  alpha<-c("A","B","C","D","E","F")
    val<-toupper(val)
    if(val %in% alpha){
            val<-as.numeric(match(val,alpha))+9
        }
        else{
            val<-as.numeric(val)
        }
    return(val)
}

#convertir en base base
Convert<-function(addr, base){
  #seprarer la chaines tous les 2 caractères
  addr<-separateur(addr)
  #convertir en base base
  if (base==16){
    return(paste(addr,collapse="."))
  }
  else{
    #convertir en base 10
    for(i in 1:length(addr)){
      donne1<-substr(addr[i],1,1)
      donne2<-substr(addr[i],2,2)

      donne1<-Convert16ToBase10(donne1)
      donne2<-Convert16ToBase10(donne2)
      donne1<-donne1*16
      donne<-donne1+donne2

        addr[i]<-donne

    }

    return(paste(addr,collapse="."))
  }

}

for (i in 1:length(address)){

  Addresse<-Convert(address[i],base[i])
    cat(Addresse,"\n")

}