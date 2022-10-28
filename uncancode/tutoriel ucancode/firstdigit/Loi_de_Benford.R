library(ggplot2)
#input
#BenfordLaw(x = 1, base = 2)
#BenfordLaw(x = 1, base = 8)
#BenfordLaw(x = 1:9, base = 10)
#BenfordLaw()

#output
#"[1] 1
#[1] 0.3333333
#[1] 0.30103000 0.17609126 0.12493874 0.09691001 0.07918125 0.06694679 0.05799195
#[8] 0.05115252 0.04575749
#[1] 0.30103000 0.17609126 0.12493874 0.09691001 0.07918125 0.06694679 0.05799195
#[8] 0.05115252 0.04575749


BenfordLaw <- function(x, base = 10) {
  if (base < 2) {
    stop("base must be >= 2")
  }
    if (any(x < 1 | x > 9)) {
    stop("x must be between 1 and 9")
    }
    if (base == 10) {
    return(log10(1 + 1/x))
    }
    else {
      return(log((1 + 1/x), base))
  }}


testbenford <- function() {
  stopifnot(
    BenfordLaw(x = 1, base = 2)==1
    #BenfordLaw(x = 1, base = 8)==3/10
    #BenfordLaw(x = 1:9, base = 10)==c(0.30103000, 0.17609126, 0.12493874, 0.09691001, 0.07918125, 0.06694679, 0.05799195, 0.05115252, 0.04575749),
    #BenfordLaw()
  )

}


testbenford()



PlotBenfordLaw <- function(base = c( 3, 4, 8, 10, 12, 16)) {
  p <- ggplot2::ggplot() + ggplot2::xlim(1, max(base) -1)  +  ggplot2::theme(text = ggplot2::element_text(size = 20))
  for(b in base) {
    p <- p + ggplot2::stat_function(fun=BenfordLaw, xlim = c(1, b - 1), args = list(base = b), ggplot2::aes_string(colour=b))
  }
  return(p)
}

PlotBenfordLaw()