#fonction max
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
    sprintf("Le résultat de l'appel de fonction Somme2Max(%d,%d,%d) est %d",x,y,z,Somme2Max(x,y,z))
}

printsomme2max(3,5,7)