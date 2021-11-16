library(MASS)

trapecio <- function(f, a, b, n){
  xs = seq(a, b, length.out = n+1)
  h = xs[2] - xs[1]
  
  br = c(xs[1], tail(xs, 1))
  r = xs[-length(xs)][-1]
  
  if (n == 1) {fr = 0}
  else {fr = f(r)}
  
  aprox = (h/2) * (sum(f(br)) + 2 * sum(fr))
  
  t = (-h^3 / 12) * n
  
  cat("\nMÉTODO DE TRAPECIOS\n")
  cat(paste0('\nLa aproximación es\n\n', round(aprox, 8), ' - ', abs(round(t, 6)), ' * f^(4) (c)\n'))
  cat(paste0('= ', fractions(aprox), ' - ', fractions(abs(t)), ' * f^(2) (c)\n\n'))
  
  r = (b-a)*0.1
  xt = seq(from = a-r, to = b+r, r*0.05)
  yt = f(xt)
  
  plot(xt,yt,pch = -2,
       xlab = 'x', ylab = 'f(x)')
  polygon(
    c(xt[xt >= a & xt <= b], b, a),
    c(yt[xt >= a & xt <= b], 0, 0),
    col = "#fdae6b"
  )
  lines(xt,yt, lwd = 2,lty = 2) 
}

trapecio(function(x) x^5,
         0, 1, 1)