library(MASS)
library(rootSolve)

simpson <- function(f, a, b, n){
  x = seq(a, b, length.out = n+1)
  h = x[2] - x[1]
  xb = c(x[1], tail(x, 1))
  xi = x[-length(x)][-1]
  lxi = length(xi)
  
  if (n == 1){
    unevenx = NULL
    evenx = NULL
  } else if (n == 2) {
    unevenx = x[2]
    evenx = 0
  } else {
      unevenx = xi[seq(1, lxi, by = 2)]
      evenx   = xi[-seq(1, lxi, by = 2)]
  } 
  
  
  fb = f(xb)
  if (length(evenx) == 0){feven = 0}
    else {feven = f(evenx)}
  if (length(unevenx) == 0){funeven = 0}
    else{funeven = f(unevenx)}
  
  aprox = (h/3) * (sum(fb) + 4 * sum(funeven) + 2 * sum(feven))
  
  t = -(h^5 * n)/90
  cat("\nMÉTODO DE SIMPSON\n")
  cat(paste0('\nLa aproximación es\n\n', round(aprox, 8), ' - ', abs(round(t, 6)), ' * f^(4) (c)\n'))
  cat(paste0('= ', fractions(aprox), ' - ', fractions(abs(t)), ' * f^(4) (c)\n\n'))
  
  
  r = (b-a)*0.1
  xt = seq(from = a-r, to = b+r, r*0.05)
  yt = f(xt)

  if (length(uniroot.all(f, c(a,b))) == 0){
    if (f(a + r) > 0) {lims = c(0, max(yt))}
      else {lims = c(min(yt), 0)}
  } else {lims = c(min(yt), max(yt))}
    
  plot(xt,yt,pch = -2,
       xlab = 'x', ylab = 'f(x)',
       ylim = lims)
  polygon(
    c(xt[xt >= a & xt <= b], b, a),
    c(yt[xt >= a & xt <= b], 0, 0),
    col = "#fdae6b"
  )
  lines(xt, rep(0,length(xt)))
  lines(xt,yt, lwd = 2,lty = 2) 
  
}


simpson(function(x) -cos(x/8),
         4, 6, 2)
