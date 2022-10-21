Alphabet<-function(upper = TRUE){
  if (upper){return (intToUtf8(as.character(utf8ToInt('A'):utf8ToInt('Z'))))}
  else {return (intToUtf8(as.character(utf8ToInt('a'):utf8ToInt('z'))))}
}

Alphabet()
Alphabet(FALSE)


