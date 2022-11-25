Compacter <- function(x) {
  i <- 1
  cx <- c()
  while (i <= length(x) ) {
    y <- x[i]
    i <- i + 1
    cx <- append(cx, y)
    ## ou
    ## cx[length(cx)+1] <- x
    while (i <= length(x) && x[i]==y) {
      i <- i + 1
    }
  }
  return(cx)
}

#test
x <- c(3,3,3,8,5,5,5,7,7)
x2<-c(1,2,3,4,5,6,7,8,0,0,9,10)
cat(Compacter(x),"\n")
stopifnot(Compacter(x) == c(3,8,5,7))
stopifnot(Compacter(c(1,x,9))==c(1,3,8,5,7,9))
stopifnot(Compacter(c(x,x))==c(3,8,5,7,3,8,5,7))
stopifnot(Compacter(c(x,x,x))==c(3,8,5,7,3,8,5,7,3,8,5,7))
stopifnot(Compacter(x2)==c(1,2,3,4,5,6,7,8,0,9,10))

Compacter <- function (x) x[c(TRUE, as.logical(diff(x)))]
Compacter(x)
Compacter(c(1,x,9))


