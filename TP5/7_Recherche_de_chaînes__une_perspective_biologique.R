

CountSubstringMatch<-function(s1,s2){
    # s1: string to search in
    # s2: string to search for
    # return: number of matches
    #use of gregexpr
  #CountSubstringMatch_2('atatata','ata')=3
  #CountSubstringMatch_2('atgacatgcacaagtatgcat','atgc')=2
  #CountSubstringMatch_2('atatata','atc')=0
  n<-0
  i<-1
    while (i<=nchar(s1)){
        if (substr(s1,i,i)==substr(s2,1,1)){
        if (substr(s1,i,i+nchar(s2)-1)==s2){
            n<-n+1
            i<-i+nchar(s2)
        }
        else{i<-i+1}
        }
        else{i<-i+1}
    }
  return(n)


}

CountSubstringMatch('atatata','ata')
CountSubstringMatch('atgacatgcacaagtatgcat','atgc')
CountSubstringMatch('atatata','atc')