Malthus<-function(nT, r){
  nT1<-nT+r*nT
  return(nT1)
}
Malthus(100, 0.1)


EvolutionMalthus<-function(n, n0, r,verbose=FALSE){
  if (verbose){cat(sprintf('Population à la génération %d : %f\n', 0, n0))}
  for (i in 1:n){
    n0<-Malthus(n0, r)
    if (verbose){cat(sprintf('Population à la génération %d : %f\n', i, n0))}

  }
  return(n0)
}

EvolutionMalthus(5, 100, 0.1,TRUE)
EvolutionMalthus(100, 100, 0.1,FALSE)