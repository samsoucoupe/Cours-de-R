exo_2<-function(){
    Somme2Max<-function(x,y,z){
        val<-x+y+z
        if(x<y){
            if(x<z){
                min<-x
            }
            else{
                min<-z
            }
        }
        else{
            if(y<x){
                if(y<z){
                    min<-y
                }
                else{
                    min<-z
                }

                }
        }

        return(val-min)
        }
    Somme2Max(1, 2, 3)
    Somme2Max(1, 3, 2)
    Somme2Max(2, 1, 3)
    Somme2Max(2, 3, 1)
    Somme2Max(3, 1, 2)
    Somme2Max(3, 2, 1)

    Somme2Maxf<-function(x,y,z){
        return(sum(x,y,z)-min(x,y,z))}

    Somme2Maxf(1, 2, 3)
    Somme2Maxf(1, 3, 2)
    Somme2Maxf(2, 1, 3)
    Somme2Maxf(2, 3, 1)
    Somme2Maxf(3, 1, 2)
    Somme2Maxf(3, 2, 1)


    printsomme2max<-function(x,y,z){
        cat("Le résultat de l'appel de fonction Somme2Max(",x,",",y,",",z,") est ",Somme2Max(x,y,z),"\n")

    }

    printsomme2max(3,5,7)
}

exo_3<-function(){
    print(sample(6, size=1))
    print(sample(1:6, size=1))
    print(sample(c(1, 2, 3, 4, 5, 6), size=1))

    print(sample(100:200,size=1))

    RandPair<- function(n) {
        return(2*sample(n%/%2, size = 1))
    }

    RandPair_corrige<-function(n){
        return(2*sample(n/2,size=1))
    }

    print(RandPair(10))
    print(RandPair_corrige(10))

    MonteCarlo<-function(){
        return(sample(c(2,3,5),size=1))
    }

    print(MonteCarlo())

    LasVegas<-function(){
        return(sample(c(2,3,3,5,5,5),size=1))
    }


    print(table(replicate(1000, LasVegas())))

    RandChiffres2<- function(n) {
        return(sample(0:9, size = n))
    }
    print(RandChiffres2(5))

    RandChiffres<- function(n) {
        return(sample((1*(10**n-1)):(1*10**(n)-1), size = 1))
    }

    print(RandChiffres(2))
}

exo_4<-function (){
serie<-function(r1,r2){
        return(r1+r2)
    }

    parallele<-function(r1,r2){
        return((r1*r2)/(r1+r2))
    }

    Circuit1<-function(r1,r2,r3){
        return(serie(r1,parallele(r2,r3)))
    }
    print(Circuit1(5,100,25))

    Circuit2<-function(r1,r2,r3){
        return(serie(parallele(r1,parallele(r2,r3)),parallele(r2,r3)))
    }

    print(Circuit2(5,100,25))
}

#probleme avec circuit
exo_5<-function(){
    hconv<-function(n){
        seconde<-n%%60
        minute<-n%/%60
        heure<-minute%/%60
        minute<-minute%%60
        sprintf("%02d:%02d:%02d",heure,minute,seconde)
        #return
    }
    hconv(4567)
    hconv(3601)
    hconv(123456789)
}

exo_6<-function(){

    Tranche <- function(s, b, h, p) {
        if (s < b) {
            return(0)
        }
        else {
            if (s < h) {
                return((s - b) * (p / 100))
            }
            return((h - b) * (p / 100))
        }
    }

    arrondi <- function(x) {
        return(x - x %% 1)
    }

    Impot <- function(s) {
        hh <- Tranche(s, 8000, 25000, 10) #au dessus de huit mille
        diffhvc <- s - 25000   #audessus de 25000
        hh <- hh + (diffhvc * (20 / 100)) #au dessus de vingt cinq mille
        return(arrondi(hh))
    }

    Tranche(1500, 2000, 3000, 10)
    Tranche(2500, 2000, 3000, 10)
    Tranche(4000, 2000, 3000, 10)
    Impot(40001)
    #probleme avec impot
}

exo_7<-function(){

    hypotenus <- function(a, b) {
        return(sqrt(a**2 + b**2))
    }

    hypotenus(3, 4)

    demandehypo <- function() {
        a <- as.numeric(readline("a="))
        b <- as.numeric(readline("b="))
        return(hypotenus(a, b))
    }

    affichehypo <- function() {
        print(demandehypo())
        return
    }

    affichehypo()
}

exo_9<-function (){
    f<-function(x){
        sin(x)/sqrt(x**4+1)
    }
    derivéedef<-function(x){
        return((f(x+0.0001)-f(x))/0.0001)
    }
    derivésecondedef<-function(x){
        return((derivéedef(x+0.0001)-derivéedef(x))/0.0001)
    }

    derivésecondedef(sqrt(2))
}

#exo_2()
#exo_3()
exo_4()
#exo_5()
#exo_6()
#exo_7()
#exo_9()

