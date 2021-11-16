library(MASS)
library(rootSolve)

trapecios <- function(f, a, b, n){
  xs = seq(a, b, length.out = n+1)
  h = xs[2] - xs[1]
  
  br = c(xs[1], tail(xs, 1))
  r = xs[-length(xs)][-1]
  
  if (n == 1) {fr = 0}
  else {fr = f(r)}
  
  aprox = (h/2) * (sum(f(br)) + 2 * sum(fr))
  
  t = (-h^3 / 12) * n
  
  {cat(paste0("\nMÉTODO DE TRAPECIOS\n",
               '\nIntegral definida entre ', a, ' y ', b , '\n',
                'h = ', h, '    n = ', n,'\n\n',
               '(h/2) * [f(bordes) + 2 * f(internos)] - (h^3 /12) * n * f^(2) (c) =\n',
               '= ', round(aprox, 8), ' - ', abs(round(t, 6)), ' * f^(2) (c) =\n',
               '= ', fractions(aprox), ' - ', fractions(abs(t)), ' * f^(2) (c)\n\n'))}
  
  {r = (b-a)*0.1
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
  lines(xt,yt, lwd = 2,lty = 2)} 
}

trapecios(function(x) -cos(6*x),
         4, 5, 2)
