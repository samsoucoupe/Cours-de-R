Verhulst<-function(nT, r, k){
    nT1<-nT+r*nT*(1-nT/k)
    return(nT1)
}
Verhulst(100, 0.1, 1000)

EvolutionVerhulst<-function(n, n0, r, k){
    cat(sprintf('Population à la génération %d : %f\n', 0, n0))
    for (i in 1:n){
        n0<-Verhulst(n0, r, k)
        cat(sprintf('Population à la génération %d : %f\n', i, n0))
    }
    return(n0)
}

EvolutionVerhulst(5, 100, -0.1, 1000)
EvolutionVerhulst(100, 100, -0.1, 1000)
EvolutionVerhulst(100, 100, 0.1, 1000)
EvolutionVerhulst(100, 100, 2.5, 1000) #cycle