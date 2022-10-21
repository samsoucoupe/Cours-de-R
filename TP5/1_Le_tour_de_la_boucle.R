Foo <- function(x,a,b) {
  for (i in a:b) {
    x = x + i
  }
  return(x)
}
Foo(10,15,20)==115
Foo(10,20,15)==115

Foowhile<-function (x,a,b){
  i<-a
  while (i<=b){
    x<-x+i
    i<-i+1
  }
  return(x)
}

FooSansBoucle<-function(x,a,b){
  somme_a_b <- ((a+b)*(b-a+1))/2
  return(x+somme_a_b)
}

testFoo<-function (FOO_t){

  stopifnot(
    FOO_t(10,15,20)==115
  )

}

testFoo(Foowhile)
testFoo(FooSansBoucle)