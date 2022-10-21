# import 2_Alphabet.R as A
Alphabet<-function(upper = TRUE){
  if (upper){return (intToUtf8(as.character(utf8ToInt('A'):utf8ToInt('Z'))))}
  else {return (intToUtf8(as.character(utf8ToInt('a'):utf8ToInt('z'))))}
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
  sgc<-" "
  maj<-(utf8ToInt('A'):utf8ToInt('Z'))
  min<-(utf8ToInt('a'):utf8ToInt('z'))
    for ( i in 1:nchar(cod)){
        l<-substr(cod,i,i)
        l<-utf8ToInt(l)
        if (l %in% maj){
        l<-l-k
        if (l>utf8ToInt('Z')){l<-l-26}
        if (l<utf8ToInt('A')){l<-l+26}
        }
        if (l %in% min){
        l<-l-k
        if (l>utf8ToInt('z')){l<-l-26}
        if (l<utf8ToInt('a')){l<-l+26}
        }
        sgc<-paste(sgc,intToUtf8(l),sep="")


    }
    return (sgc)
}

#DecodeCesar(CodeCesar("Bonjour",3),3)

TestCesar <- function(msg, k) {
  cod <- CodeCesar(msg,k)
  dcod <- DecodeCesar(cod,k)
  cat('k=', k, ":" , msg, '-->', cod,'-->', dcod, '\n')
}
DecodeCesar_sans_Cle<-function(cod){
  for (i in 1:26){
    msgc<-DecodeCesar(cod,i)
    cat(i,':',msgc,'\n')
  }
}
DecodeCesar_sans_Cle('JLGVI XRJFZC')# 17 :  SUPER GASOIL


TestCesar('envoyez 36 hommes !', 3)
TestCesar('ENVOYEZ 36 HOMMES !', -23)
TestCesar('ENVOyez 36 homMES !', 5)