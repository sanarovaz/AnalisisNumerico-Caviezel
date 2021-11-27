library(MASS)
library(Deriv)
library(rootSolve)

trapecios <- function(f, a, b, n){
  
  {xs = seq(a, b, length.out = n+1)
  h = xs[2] - xs[1]
  
  br = c(xs[1], tail(xs, 1))
  r = xs[-length(xs)][-1]
  
  if (n == 1) {fr = 0}
  else {fr = f(r)}
  
  aprox = (h/2) * (sum(f(br)) + 2 * sum(fr))
  
  t = (-h^3 / 12) * n}
  
  {f2c = fnc(f = f, a = a, b = b, n = 1, print = FALSE, plot = FALSE)
     c = f2c$c
     fc = f2c$fc
     fmin = f2c$fmin
     fmax = f2c$fmax
     sdf2 = f2c$dfn1
     error_extremes = sort(c(t*fmin, t*fmax))
     integral_extremes = sort(c(aprox + t*fmin, aprox + t*fmax))
     
   eq = paste0("\n\n                        MÉTODO DE TRAPECIOS\n",
               'Integral definida entre ', a, ' y ', b , '\n',
                'h = ', h, '    n = ', n,'\n\n',
               '(h/2) * [f(bordes) + 2 * f(internos)] - (h^3 /12) * n * f^(2) (c) =\n',
               '= ', round(aprox, 8), ' - ', abs(round(t, 6)), ' * f^(2) (c) =\n',
               '= ', fractions(aprox), ' - ', fractions(abs(t)), ' * f^(2) (c)\n\n\n',
               '\nANÁLISIS DEL ERROR\n',
               'f^(2) (x) = ', sdf2, '\n\n',
               'Se escogió c = ', c, '\n',
               'f^(2) (c) = ', fc, '\n',
               'Cota del error: ', abs(t*fc), '\n',
               'Extremos del error: [', error_extremes[1], ', ', error_extremes[2], ']\n',
               'Intervalo de integral: [', integral_extremes[1], ', ', integral_extremes[2], ']\n\n'
               )}
  
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
  
  {invisible(data.frame(eq, aprox, t, h, fc, fmin, fmax,c,
                       minerror = error_extremes[1],
                       maxerror = error_extremes[2],
                       minintegral = integral_extremes[1],
                       maxintegral = integral_extremes[2]
                       ))}
}


cat(trapecios(function(x) 1 / (x-3)^(1/2),
              4, 10, 3)$eq)
cat(trapecios(function(x) abs(cos(x)),
              pi/3, pi, 4)$eq)
cat(trapecios(function(x) log(x),
              1, 2.5, 2)$eq)