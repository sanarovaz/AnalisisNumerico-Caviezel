library(MASS)
library(Deriv)
library(rootSolve)

# La función fnc toma como argumentos el vector de los extremos del intervalo (a y b), la derivada de orden n+1 de f(x) (dfn1),
# y la derivada de orden n+2 de f(x) (dfn2). Devuelve el valor c a tomar para f^(n+1) (c) en el cálculo del error absoluto de
# interpolación, además de especificar el valor de la función en ese punto.


fnc <- function(f,a,b,n, print = TRUE, plot = TRUE){
  
  {dfn1 = Deriv(f, "x", nderiv = n + 1)
  dfn2 = Deriv(dfn1, "x")
  dfn3 = Deriv(dfn2, "x")
  
  positive = FALSE
  negative = FALSE
  
  roots   <- uniroot.all(dfn2, c(a, b))
    roots <- roots[dfn3(roots) < 0]}
  
  {if(length(roots) == 0){
    
    if(dfn1(a) > 0){
      positive = TRUE
    } else {
      negative = TRUE
    }
    
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
  }
  
  xs = seq(a, b, 0.001)
  ys = dfn1(xs)
  
  if (plot == TRUE){r = (b-a)*0.025
    xt = seq(a-r, b+r, 0.001)
    yt = dfn1(xt)
    yt2 = dfn2(xt)
    maxy = max(c(max(yt), max(yt2)))
    miny = min(c(min(yt), min(yt2)))
    
    plot(xt,yt,pch = -2,
         main = paste0("Maximización de f^(", n+1, ") (x)"),
         ylim = c(miny, maxy),
         xlab = 'x', ylab = 'f(x)',)  
    lines(xt,yt2, col = 'black', lwd = 2,lty = 2) 
    lines(xt,yt, col = 'red', lwd = 2)
    lines(xt, rep(0,length(xt)))
    lines(rep(c, 1000), seq(-5,5, length.out = 1000))
    legend(x = 'topleft',
           legend = c(paste0('f^(', n+1, ') (x)'), paste0('f^(', n+2, ') (x)')), 
           lwd = c(2,2), lty = c(1, 2), col = c('red','black'))}  
  
  {sdf1    <- unlist(strsplit(deparse1(dfn1),c("")))
    sdf1  <- paste(sdf1[-c(1:14)], collapse = "")
  sdf2    <- unlist(strsplit(deparse1(dfn2),c("")))
    sdf2  <- paste(sdf2[-c(1:14)], collapse = "")}
  
  if (print == TRUE){
    
    if (positive == TRUE){cat(paste0('\nLa derivada de orden ', n+1, ' es positiva en el intervalo\n'))}
    if (negative == TRUE){cat(paste0('\nLa derivada de orden ', n+1, ' es negativa en el intervalo\n'))}
    
      
    cat(paste0('\nANÁLISIS DE FUNCIÓN:\n',
               'Mínimo en [', a, ', ', b, ']: ', min(yt), '\n',
               'Máximo en [', a, ', ', b, ']: ', max(yt), '\n\n',
               '\nDERIVADAS DE F\n',
               'f^(', n+1, ') = ', sdf1, '\n',
               'f^(', n+2, ') = ', sdf2, '\n',
               '\n\nEVALUACIÓN DE C:\nSe escogió c = ', c, '\nf^(', n+1, ') (c) = ', dfn1(c), '\n\n\n'))
    
  }

  {df <- data.frame(fa = dfn1(a),
                   fb = dfn1(b),
                   fc = dfn1(c),
                   fmax = max(ys),
                   fmin = min(ys),
                   dfn1 = sdf1,
                   dfn2 = sdf2,
                   fabsmin = min(abs(ys)),
                   c)
  
  invisible(df)}
  
}


x <- fnc(f = function(x) x^4,
    a = 0, b = 4, n = 1)