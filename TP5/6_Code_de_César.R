# import 2_Alphabet.R as A
Alphabet<-function(upper = TRUE){
  if (upper){return (intToUtf8(as.character(utf8ToInt('A'):utf8ToInt('Z'))))}
  else {return (intToUtf8(as.character(utf8ToInt('a'):utf8ToInt('z'))))}
}


CodeCesar_2<-function(msg,k){
  msg<-utf8ToInt(msg)
    msg<-msg+k
  return(intToUtf8(msg))
}
#CodeCesar_2('ABC',1)

CodeCesar_1<-function(msg,k){
  msg<-utf8ToInt(msg)

  for (i in 1:length(msg)){

  if( msg[i]>=utf8ToInt('A') && msg[i]<=utf8ToInt('Z') || msg[i]>=utf8ToInt('a') && msg[i]<=utf8ToInt('z')){
    msg[i]<-msg[i]+k
  }
  else if (msg[i]>utf8ToInt('Z')){
    msg[i]<-msg[i]-26
  }
  else if (msg[i]>utf8ToInt('z')){
    msg[i]<-msg[i]-26
  }
  }

  return(intToUtf8(msg))

}

CodeCesar_MAJ<-function(msg,k){
  msg<-utf8ToInt(msg)
  if (k>0){k<-k%%26} else {k<-k%%-26}

  for (i in 1:length(msg)){

    if (msg[i]>=utf8ToInt('A') && msg[i]<=utf8ToInt('Z')){
      msg[i]<-msg[i]+k
      if (msg[i]<utf8ToInt('A')){
        msg[i]<-msg[i]+26
      }
      if (msg[i]>utf8ToInt('Z')){
        msg[i]<-msg[i]-26
      }
    }

  }

  return(intToUtf8(msg))

}

CodeCesar_tout<-function(msg,k){
  endode<-function(char,borneInf,borneSup){
    char<-char
    if (char<utf8ToInt(borneInf)){
      char<-char+26
    }
    if (char>utf8ToInt(borneSup)){
      char<-char-26
    }
    return(char)
  }

  msg<-utf8ToInt(msg)
  if (k>0){k<-k%%26} else {k<-k%%-26}

  for (i in 1:length(msg)){

    if (msg[i]>=utf8ToInt('A') && msg[i]<=utf8ToInt('Z')){
      msg[i]<-msg[i]+k
      msg[i]<-endode(msg[i],'A','Z')
    }
    else if (msg[i]>=utf8ToInt('a') && msg[i]<=utf8ToInt('z')){
      msg[i]<-msg[i]+k
      msg[i]<-endode(msg[i],'a','z')
    }

  }

  return(intToUtf8(msg))

}



CodeCesar<-function(msg,k){
  msgc<-" "
  maj<-(utf8ToInt('A'):utf8ToInt('Z'))
  min<-(utf8ToInt('a'):utf8ToInt('z'))
  for ( i in 1:nchar(msg)){
    l<-substr(msg,i,i)
    l<-utf8ToInt(l)
    if (l %in% maj){
      l<-l+k
      if (l>utf8ToInt('Z')){l<-l-26}
      if (l<utf8ToInt('A')){l<-l+26}
    }
    if (l %in% min){
      l<-l+k
      if (l>utf8ToInt('z')){l<-l-26}
      if (l<utf8ToInt('a')){l<-l+26}
    }
    msgc<-paste(msgc,intToUtf8(l),sep="")

  }
    return (msgc)
}
#CodeCesar("Bonjour ",3)

DecodeCesar<-function(cod,k){
  return(CodeCesar_1(cod,-k))
}

#DecodeCesar(CodeCesar_1("Bonjour",3),3)

TestCesar <- function(msg, k) {
  cod <- CodeCesar_tout(msg,k)
  dcod <- DecodeCesar(cod,k)
  cat('k=', k, ":" , msg, '-->', cod,'-->', dcod, '\n')
}
DecodeCesar_sans_Cle<-function(cod){
  for (i in 1:26){
    msgc<-DecodeCesar(cod,i)
    cat(i,':',msgc,'\n')
  }
}
#DecodeCesar_sans_Cle('JLGVI XRJFZC')# 17 :  SUPER GASOIL
#
#
TestCesar('envoyez 36 hommes !', 3)
TestCesar('ENVOYEZ 36 HOMMES !', -23)
TestCesar('ENVOyez 36 homMES !', 5)


TestcodeCesar<-function(Function_code){
  #cas limite
  #cas general
  # #cas d'erreur
  #cas avec K > 26
  stopifnot(
    #cas majuscule
    #cas limite
    Function_code('A',1)=='B',
    Function_code('Z',1)=='A',
    Function_code('Z',2)=='B',
    Function_code('A',-1)=='Z',
    Function_code('A',-2)=='Y',
    Function_code('Z',-1)=='Y',
    #cas general
    Function_code('ABC',1)=='BCD',
    Function_code('ABC',2)=='CDE',
    Function_code('ABC',-1)=='ZAB',
    Function_code('ABC',-2)=='YZA',
    #cas avec des caractères spéciaux
    Function_code('ABC !',1)=='BCD !',
    Function_code('ABC !',2)=='CDE !',
    Function_code('ABC !',-1)=='ZAB !',
    Function_code('ABC !',-2)=='YZA !',
    Function_code('ABC 152',1)=='BCD 152',
    Function_code('ABC 152',2)=='CDE 152',
    Function_code('ABC 152',-1)=='ZAB 152',
    Function_code('ABC 152',-2)=='YZA 152',
    #cas avec K > 26
    Function_code('ABC',27)=='BCD',
    Function_code('ABC',-27)=='ZAB'



  )

}

TestcodeCesar(CodeCesar_MAJ)
TestcodeCesar(CodeCesar_tout)