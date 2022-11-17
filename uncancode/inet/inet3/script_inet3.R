

address<-c("C0290614","863B83AC","C0A80001","F0040506")

sortie<-c("C02906 14","863B 83AC","C0A800 01","F0040506 F0040506")

#

#Nom de Classe	Valeurs de Bits de classe
#A	0
#B	10
#C	110
#D	1110
#E	1111

# si premier bit = 0 alors A
# si premier bit = 1 et deuxieme bit = 0 alors B
# si premier bit = 1 et deuxieme bit = 1 et troisieme bit = 0 alors C
# si premier bit = 1 et deuxieme bit = 1 et troisieme bit = 1 et quatrieme bit = 0 alors D
# si premier bit = 1 et deuxieme bit = 1 et troisieme bit = 1 et quatrieme bit = 1 alors E

#convert base 16 to base 2
Convert16to2<-function (val){
  if (typeof(val)!= "character") {
    val<-as.character(val)
  }
  if (val=="0") return("0000")
    if (val=="1") return("0001")
    if (val=="2") return("0010")
    if (val=="3") return("0011")
    if (val=="4") return("0100")
    if (val=="5") return("0101")
    if (val=="6") return("0110")
    if (val=="7") return("0111")
    if (val=="8") return("1000")
    if (val=="9") return("1001")
    if (val=="A") return("1010")
    if (val=="B") return("1011")
    if (val=="C") return("1100")
    if (val=="D") return("1101")
    if (val=="E") return("1110")
    if (val=="F") return("1111")
}



Classe<-function(address){
  #scrap du permier bit
  donne<-substr(address,1,1)
  #convertion en base 2
  donne<-Convert16to2(donne)
  #scrap du premier bit
  bit1<-substr(donne,1,1)
  #scrap du deuxieme bit
  bit2<-substr(donne,2,2)
  #scrap du 3eme bit
  bit3<-substr(donne,3,3)
  #scrap du dernier bit
  bit4<-substr(donne,4,4)
  if(bit1=="0") return("A")
  if (bit2=="0") return("B")
  if (bit3=="0") return("C")
  if (bit4=="0") return("D")
  if (bit4=="1") return("E")
  else{
    return("Erreur")
  }
}

# donne Le netid et le hostid séparés par un espace.

# classe A : netid = 8 bits, hostid = 24 bits
# classe B : netid = 16 bits, hostid = 16 bits
# classe C : netid = 24 bits, hostid = 8 bits
# classe D : netid = 32 bits, hostid = 32 bits
# classe E : netid = 32 bits, hostid = 32 bits

Get_netid<-function(adresse,classes){
  if (classes=="A"){
    return(substr(adresse,1,2))
  }
    if (classes=="B"){
        return(substr(adresse,1,4))
    }
    if (classes=="C"){
        return(substr(adresse,1,6))
    }
    if (classes=="D" || classes=="E"){
        return(substr(adresse,1,8))
    }
}

Get_hostid<-function(adresse,classes){
  if (classes=="A"){
    return(substr(adresse,3,8))
  }
    if (classes=="B"){
        return(substr(adresse,5,8))
    }
    if (classes=="C"){
        return(substr(adresse,7,8))
    }
    if (classes=="D" || classes=="E"){
        return(substr(adresse,1,8))
    }

}


Formatage<-function(address){
  classes<-Classe(address)
  #recuperation du netid
  netid<-Get_netid(address,classes)
  #recuperation du hostid
  hostid<-Get_hostid(address,classes)
  return(paste(netid,hostid,sep=" "))
}

for ( i in 1:length(address)){
  if (Formatage(address[i])!=sortie[i]){
    print(paste("Erreur",address[i],Formatage(address[i]),sortie[i]))
  }
  else{
    print(paste("OK"))
  }

}




