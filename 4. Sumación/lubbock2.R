library(MASS)
library(Deriv)
library(rootSolve)

lubbock2 <- function(f, m, n, k = 2){
  
  Av <- function(x, m){
    if (x == 1){return((m-1)/2)}
    if (x == 2){return(-(m^2 - 1)/(12*m))}
    if (x == 3){return(1/24)}
  }
  
  tx = 0:(n - 1)
  A = sum(f(tx))
  
  xs = 0:(n+1)
  ys = f(xs)
  diffs = difftable(xs, ys)
  B = 0
  for (i in 1:k){
    delta = diffs[n+1, 1+i] - diffs[1, 1+i]
    B = B + Av(i, m) * delta
  }
  
  
  aprox = m * A + B
  t = Av(k+1, m) * n
  
  real = 0
  for(i in 0:(m*n - 1)){
    real = real + f(i/m)}
  
  f2c = fnc(f = f, a = 0, b = n*m, n = k, print = FALSE, plot = FALSE)
    c = f2c$c
    fc = f2c$fc
    fmin = f2c$fmin
    fmax = f2c$fmax
    sdf2 = f2c$dfn1
    error_extremes = sort(c(t*fmin, t*fmax))
  
    
  eq = paste0("\n\n                        MÉTODO DE LÜBBOCK 2\n",
              'Sumatoria entre 0 y ', (m*n - 1) , '\n',
              'm = ', m, '    n = ', n,'    k = ', k, '\n\n',
              'SUMA(t = [0, mn-1]) f(t/m) =\n= m SUMA(t = [0, n-1]) f(t) + SUMA(v = [1, k]) Av[delta^v-1 f(n) - delta^v-1 f(0)] + A_k+1 n f^(k+1) (c)=\n',
              '= m * ', round(A, 5), ' + ', round(B, 5), ' + ', round(t, 5), ' * f^(k+1) (c) =\n',
              '= ', aprox, ' + ', round(t, 5), ' * f^(n+1) (c) =\n',
              '\nANÁLISIS DEL ERROR\n',
              'f^(', k+1 ,') (x) = ', sdf2, '\n\n',
              'Se escogió c = ', c, '\n',
              'f^(', k+1, ') (c) = ', fc, '\n',
              'Cota del error: ', abs(t*fc), '\n',
              'Extremos del error: [', error_extremes[1], ', ', error_extremes[2], ']\n\n',
              'Resultado teórico: ', real, '\n\n')
  
  {invisible(data.frame(eq, aprox))}

}


cat(lubbock2(function(x) x^2,
            3, 4)$eq)
