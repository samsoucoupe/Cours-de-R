## Lire 4 nombres sur l'entrée standard.
## Capturer le flux d'entrée standard.
stream <- file("stdin","r")
c1 <- scan(file=stream, what= integer(), quiet=TRUE, n = 1)
c2 <- scan(file=stream, what= integer(), quiet=TRUE, n = 1)
c <- scan(file=stream, what= integer(), quiet=TRUE, n = 1)
v <- scan(file=stream, what= integer(), quiet=TRUE, n = 1)
## Ferme proprement le flux (pour éviter un warning).
close(stream)

## Écrire votre code ci-dessous

c1<-c1/1000
r<-(c*v-(c2*v))/(c1-c2)

## Vous devez calculer le volume en microlitre de la solution 1 dans le mélange
r<-r*1000
r<-round(r)
## et le mémoriser dans la variable v1.
v1<-r

## Afficher le volume (ne pas modifier le code ci-dessous)
cat(v1, '\n', sep = '')