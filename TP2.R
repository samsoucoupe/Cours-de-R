#fonction max
Somme2Max<-function(x,y,z){
    if(x>y){
        if(x>z){
        return(x)
        }else{
        return(z)
        }
    }else{
        if(y>z){
        return(y)
        }else{
        return(z)
        }
    }
}

Somme2Max<-function(x,y,z){return(max(x,y,z))}

Somme2Max(1, 2, 3)
Somme2Max(1, 3, 2)
Somme2Max(2, 1, 3)
Somme2Max(2, 3, 1)
Somme2Max(3, 1, 2)
Somme2Max(3, 2, 1)