Ascii<-function (){
  min<-32
  max<-126
  r<-min:max
  t<-intToUtf8(r, multiple = TRUE)
  cat(t,'\n')

  resultat<-c(paste((r),t,sep=':'))

  return (resultat)
}

Ascii()