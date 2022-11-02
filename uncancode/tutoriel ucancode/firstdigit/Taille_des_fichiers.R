#Écrire une fonction FileSize qui renvoie le vecteur des tailles des fichiers dans le répertoire path visité récursivement. Indices : file.size et list.files.
#Analyser la taille des fichiers dans différents répertoires.


FileSize<-function(path){
  #Écrire une fonction FileSize qui renvoie le vecteur des tailles des fichiers dans le répertoire path visité récursivement. Indices : file.size et list.files.
  return(file.size(list.files(path,recursive=TRUE)))
}

#Analyser la taille des fichiers dans différents répertoires.

x<-FileSize("uncancode/tutoriel ucancode/firstdigit/cor.R")
cat(x)