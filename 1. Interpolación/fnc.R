library(rootSolve)

# La funci�n fnc toma como argumentos el vector de los extremos del intervalo (a y b), la derivada de orden n+1 de f(x) (dfn1),
# y la derivada de orden n+2 de f(x) (dfn2). Devuelve el valor c a tomar para f^(n+1) (c) en el c�lculo del error absoluto de
# interpolaci�n, adem�s de especificar el valor de la funci�n en ese punto.


fnc <- function(a,b,dfn1,dfn2, output = TRUE){
  
  roots <- uniroot.all(dfn2, c(a, b))
  
  if(is.na(roots[1])){
    
    if(dfn1(a) > 0){
      cat('\nLa derivada n+1-�sima es positiva en el intervalo\n')
    } else {
      cat('\nLa derivada n+1-�sima es negativa en el intervalo\n')
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
  
  {r = (b-a)*0.025
  xt = seq(a-r, b+r, 0.001)
  yt = dfn1(xt)
  yt2 = dfn2(xt)
  maxy = max(c(max(yt), max(yt2)))
  miny = min(c(min(yt), min(yt2)))
  
  plot(xt,yt,pch = -2,
       ylim = c(miny, maxy),
       xlab = 'x', ylab = 'f(x)',)  
    lines(xt,yt2, col = 'black', lwd = 2,lty = 2) 
    lines(xt,yt, col = 'red', lwd = 2)
    lines(xt, rep(0,length(xt)))
    lines(rep(c, 1000), seq(-5,5, length.out = 1000))
    legend(x = 'topleft',
           legend = c('f^(n+1) (x)', 'f^(n+2) (x)'), 
           lwd = c(2,2), lty = c(1, 2), col = c('red','black'))}  
  
  if (output == TRUE){
    cat(paste0('\nAN�LISIS DE FUNCI�N:\nf(', a, ') = ', dfn1(a), '\nf(', b, ') = ', dfn1(b),
               '\nM�nimo en [', a, ', ', b, ']: ', min(yt),
               '\nM�ximo en [', a, ', ', b, ']: ', max(yt),
               '\n\nEVALUACI�N DE C:\nSe escogi� c = ', c, '\nf^(n+1) (c) = ', dfn1(c), '\n\n'))
    }

  {invisible(data.frame(fa = dfn1(a),
                       fb = dfn1(b),
                       fc = dfn1(c), 
                       min = min(yt), 
                       max = max(yt), 
                       roots = uniroot.all(dfn2, c(a,b)),
                       c))}
    
}


fnc(0, pi,
    function(x) -cos(x),
    function(x) sin(x))