library(rootSolve)

# La función fnc toma como argumentos el vector de los extremos del intervalo (a y b), la derivada de orden n+1 de f(x) (dfn1),
# y la derivada de orden n+2 de f(x) (dfn2). Devuelve el valor c a tomar para f^(n+1) (c) en el cálculo del error absoluto de
# interpolación, además de especificar el valor de la función en ese punto.


fnc <- function(a,b,dfn1,dfn2){
  
  roots <- uniroot.all(dfn2, c(a, b))
  
  if(is.na(roots[1])){
    if (abs(dfn1(a)) > abs(dfn1(b))){
      c = a
    } else {
        c = b
    }
  } else {
  
    c <- roots[1]
    
    
    if (length(roots) > 1){
      for (i in 2:length(roots)){
        if(abs(dfn1(roots[i])) > abs(dfn1(c))){
          c = roots[i]
        }
      }
    }
  }

  xt = seq(a-0.25,b+0.25, 0.001)
  yt = dfn1(xt)
  
  plot(xt,yt,pch = -2,
       xlab = 'x', ylab = 'y')  
    lines(xt,dfn2(xt), col = 'black', lwd = 2,lty = 2) 
    lines(xt,yt, col = 'red', lwd = 2)
    lines(xt, rep(0,length(xt)))
    lines(rep(c, 1000), seq(-5,5, length.out = 1000))
    legend(x = 'topleft',
           legend = c('f^(n+1) (x)', 'f^(n+2) (x)'), 
           lwd = c(2,2), lty = c(1, 2), col = c('red','black'))  
  
  cat(paste0('Se escogió c = ', c, '\nf^(n+1) (c) = ', dfn1(c), '\n'))
  
}


dfn1 <- function(x){
  x^3 + 4*x^2-6
}
dfn2 <- function(x){
  3*x^2+8*x
}

fnc(-4, 1, dfn1, dfn2)


fnc(1, 2, dfn1, dfn2)
