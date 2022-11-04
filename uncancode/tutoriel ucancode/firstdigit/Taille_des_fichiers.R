#Écrire une fonction FileSize qui renvoie le vecteur des tailles des fichiers dans le répertoire path visité récursivement. Indices : file.size et list.files.
#Analyser la taille des fichiers dans différents répertoires.


FileSize<-function(path){
  cat("Taille des fichiers dans le répertoire ",path,":\n")
  cat("Taille totale des fichiers : ",sum(file.size(list.files(path,recursive=TRUE))),"\n")


  #Écrire une fonction FileSize qui renvoie le vecteur des tailles des fichiers dans le répertoire path visité récursivement. Indices : file.size et list.files.
  return(file.size(list.files(path,recursive=TRUE)))
}

#Analyser la taille des fichiers dans différents répertoires.

FileSize(".")
list.files(".",recursive=TRUE)

cat()