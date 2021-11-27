library(MASS)
library(Deriv)
library(rootSolve)

laplace <- function(f, a, b, n){
  
  bern <- function(x){
    if (x == 1){return(1/2)}
    if (x == 2){return(-1/12)}
    if (x == 3){return(1/24)}
    if (x == 4){return(-19/724)}
  }
  dd   <- function(i, f, x, h){
    if (i == 0){return(f(x))}
    if (i == 1){return(f(x+h) - f(x))}
    if (i == 2){return(f(x+2*h) - 2* f(x+h) + f(x))}
    if (i == 3){return(f(x+3*h) - 3 * f(x+2*h) + 3 * f(x+h) - f(x))}
  }
  
  {xs = seq(a, b, length.out = n+1)
  h = xs[2] - xs[1]}
  
  tx = xs[1:n]
  A = sum(f(tx))
  
  B = 0
  dds = c()
  for (s in 1:4){
    ddb = dd(s-1, f, b, h)
    dda = dd(s-1, f, a, h)
    B <- B + bern(s) * (ddb-dda)
    dds <- append(dds, c(ddb, dda))
  }
  
  aprox = h * A + h * B
  
  t = h^6 * n
  
  {f2c = fnc(f = f, a = a, b = b, n = 4, print = FALSE, plot = FALSE)
    c = f2c$c
    fc = f2c$fc
    fmin = f2c$fmin
    fmax = f2c$fmax
    sdf2 = f2c$dfn1
    error_extremes = sort(c(t*fmin, t*fmax))
    integral_extremes = sort(c(aprox + t*fmin, aprox + t*fmax))
    
    eq = paste0("\n\n                        MÉTODO DE LAPLACE\n",
                'Integral definida entre ', a, ' y ', b , '\n',
                'h = ', h, '    m = ', n,'\n\n',
                'h SUMA(t = [0, m-1]) f(a+th) + h SUMA(s = [1, n]) bss(1)[delta^(s-1) f(a+nh) - delta^(s-1) f(a)] + h^(n+2) * m * bn1n1 * f^(n+1) (c) =\n',
                '= h * ', round(A, 8), ' + h * ', round(B, 8), ' + ', t, ' bn1n1 * f^(n+1) (c) =\n',
                '= ', aprox, ' + ', t, ' bn1n1 * f^(n+1) (c) =\n',
                '= ', fractions(aprox), ' + ', fractions(abs(t)), ' bn1n1 * f^(n+1) (c)\n\n\n',
                '\nANÁLISIS DEL ERROR\n',
                'f^(5) (x) = ', sdf2, '\n\n',
                'Se escogió c = ', c, '\n',
                'f^(5) (c) = ', fc, '\n',
                'Cota del error: ', abs(t*fc), '\n',
                'Extremos del error: [', error_extremes[1], ' * bn1n1, ', error_extremes[2], ' * bn1n1]\n'
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


cat(laplace(function(x) 1/x,
              2, 4, 4)$eq)
