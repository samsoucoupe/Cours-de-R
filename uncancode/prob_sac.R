#probleme du sac
#entre
#poids max 15kg
#12kg 7€
#9kg 10€
#7kg 3€
#5kg 2€
#2kg 1€

#resulat
#natif
#9kg 10€
#2kg 1€
#==================
#11kg 11€

#attendu
#12€ 14kg



trie<- function(l1,l2){
  swap<-function(i,j,l){
    tmp<-1[i]
    l[i]<-l[j]
    l[j]<-tmp
  }
  l1_s<-sort(l1)


  for (i in 1:length(l1)){
    j<-i
    while (l1_s[i]!=l1[i] && j<=length(l1)){

        j<-j+1

      }
    swap(i,j,l1)
    swap(i,j,l2)
    }
  return (l2)
}



sac<-function(quantité,poids,valeur){
  if (length(poids)!=length(valeur) || length(poids)==0 || length(valeur)==0 || quantité<0){
    return ("erreur")
  }
  if (0 %in% poids && valeur[which(poids==0)]>0){
    return ("erreur")
  }
  if (0 %in% valeur && poids[which(valeur==0)]>0){
    return ("erreur")
  }
    if (quantité==0){
        return (0)
    }
  if (!(0 %in% poids)){
    poids<-c(0, poids)
    valeur<-c(0, valeur)
  }


  dic_poids<-rep(0,quantité+1)

  for (i in 1:length(poids)){
    dic_poids[poids[i]]<-valeur[i]
  }

  for (i in 1:quantité){
    for (j in 1:length(poids)){
      if (i+1>poids[j]){
        dic_poids[i+1]<-max(dic_poids[i+1],dic_poids[i-poids[j]]+valeur[j])
      }
    }
  }

  return (dic_poids[quantité+1])
}

#l1<-c(1,3,2)
#l2<-c(4,5,6)
#trie(l1,l2)
#cat(l1,'\n',l2,'\n')

#sac(15,c(0,2,5,7,12,9),c(0,1,2,3,7,10))

affichematrice<-function(matrice){
    for (i in 1:nrow(matrice)){
        for (j in 1:ncol(matrice)){
        cat(matrice[i,j],'\t')
        }
        cat('\n')
}
}

sac_mat<-function(quantité,poids,valeur){
  poids<-c(0,poids)
  valeur<-c(0,valeur)
  trie(poids,valeur)
  cat(valeur,'\n')
  dic_poids<-matrix(0,length(poids)+1,quantité+3)
  dic_test<-matrix(0,length(poids)+1,quantité+3)
  #affichematrice(dic_poids)
  for (i in 0:quantité+3){
    dic_test[1,i]<-i-3


  }
  for (i in 1:length(poids)+1){
    dic_test[i,1]<-poids[i-1]
    dic_test[i,2]<-valeur[i-1]
  }
  dic_test[1,1]<-'p'
  dic_test[1,2]<-'v'



  cat('====================\n')
  affichematrice(dic_test)
  for (np in 2:length(poids)+1){
    for (i in 1:quantité+3){

      if (i<poids[np-1]){
        dic_test[np,i]<-dic_test[np-1,i]
      }
      else{
        case_dessus<-as.integer(dic_test[np-1,i])
        case_gauche<-as.integer(dic_test[np-1,i-poids[np-1]])+valeur[np-2]

        cat(np,i,case_dessus,case_gauche,valeur[np-2],'\t',max(case_dessus,case_gauche),'\n')#case
        dic_test[np,i]<-max(case_dessus,case_gauche)
      }


    }
  }
  cat('====================\n')
  affichematrice(dic_test)
  for (np in 2:length(poids)+1){
    for (i in 1:quantité+3){
      dic_test[np,i]<-paste(np,i)



      }
    }



  cat('====================\n')

  affichematrice(dic_test)
}

#sac_mat(5,c(2,5,7,12,9,15),c(1,2,3,7,10,15))
#sac_mat(15,c(2,5,7,12,9),c(1,2,3,7,10))

affichesac_mat<-function(quantité,poids,valeur){
  if (length(poids)!=length(valeur) || length(poids)==0 || length(valeur)==0 || quantité<0){
    return ("erreur")
  }
  if(0 %in% poids || 0 %in% valeur){
    return ("erreur")
  }
  poids<-c(0,poids)
  valeur<-c(0,valeur)
  trie(poids,valeur)

  dic_poids<-matrix(0,length(poids)+1,quantité+3)
  dic_test<-matrix(0,length(poids)+1,quantité+3)
  #affichematrice(dic_poids)
  for (i in 0:quantité+3){
    dic_test[1,i]<-i-3
  }
  for (i in 1:length(poids)+1){
    dic_test[i,1]<-poids[i-1]
    dic_test[i,2]<-valeur[i-1]
  }
    dic_test[1,1]<-'p'
    dic_test[1,2]<-'v'
    cat('====================\n')
    affichematrice(dic_test)
  for (np in 2:length(poids)+1){
    for (i in 1:quantité+3){
      dic_test[np,i]<-sac(i-3,poids[1:np-1],valeur[1:np-1])
      }
    }

  cat('====================\n')
  affichematrice(dic_test)}


Testsac<-function(fonction_sac) {
  system.time(
    stopifnot(
      fonction_sac(15,c(0,2,5,7,12,9),c(0,1,2,3,7,10)) == 12,
      fonction_sac(15,c(2,5,7,12,9),c(1,2,3,7,10)) == 12,
      fonction_sac(2,c(0,2,5,7,12,9),c(0,1,2,3,7,10))==1,
      fonction_sac(0,c(0,2,5,7,12,9),c(0,1,2,3,7,10))==0,
      fonction_sac(-1,c(0,2,5,7,12,9),c(0,1,2,3,7,10))=='erreur',
      fonction_sac(15,c(0,2,5,7,12,9),c(0,1,2,3,7))=='erreur',
      fonction_sac(15,c(0,2,5,7,12,9),c(0,1,2,3,7,10,11))=='erreur',
      fonction_sac(15,c(),c(0,1,2,3,7,10,11))=='erreur',
      fonction_sac(15,c(0,2,5,7,12,9),c())=='erreur'

  ))
}



Testsac(sac)
affichesac_mat(15,c(2,5,7,12,9),c(1,2,3,7,10))
#Testsac(sac_mat)


