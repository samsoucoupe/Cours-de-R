ProchaineFrequence<-function (p,wA,wB){
  q<-1-p
  num<-p*wA
  denum<-num+q*wB
  r<-num/denum
  return(r)
}

ProchaineFrequence(0.1, 1, 0.9)

EvolutionFrequence1<-function(n, p, wA, wB){
  for (i in 0:n){
    cat(sprintf('Fŕequence à la génération %d : %f\n', i, p))
    p<-ProchaineFrequence(p, wA, wB)
  }
}

#EvolutionFrequence1(3, 0.1, 1, 0.9) # avec 3 générations


EvolutionFrequence2<-function(n, p, wA, wB){

  i<-0
  h<-1/2**20
  cat(sprintf('Fŕequence à la génération %d : %f\n', i, p))

  while (i<=n && (1-p)>h){
    p<-ProchaineFrequence(p, wA, wB)
    i<-i+1
    cat(sprintf('Fŕequence à la génération %d : %f\n', i, p))
  }

  if ((1-p)<h && i<n){
    p<-ProchaineFrequence(p, wA, wB)
    cat(sprintf('Fŕequence à la génération %d : %f\n', i+1, p))
    cat("Fixation : arrêt de la simulation.\n")}
}
EvolutionFrequence2(1000, 0.1, 1, 0.9)
#EvolutionFrequence2(3, 1, 1, 0.9)
#EvolutionFrequence2(100, 0.1, 1, 1 - 10**(-12))

