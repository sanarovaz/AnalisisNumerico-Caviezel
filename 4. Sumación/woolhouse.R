library(MASS)
library(Deriv)
library(rootSolve)

woolhouse <- function(f, m, n, k = 2){
  bern <- function(x){
    if (x == 2){return(1/12)}
    if (x == 4){return(-1/720)}
    if (x == 6){return(1/30240)}
  }
  
  A = 0
  for (i in 1:n){
    A <- A + f(i * m)
  }
  
  B = f(m*n) - f(0)
  
  C = 0
  for (j in 1:k){
    b = bern(2*j)
    f2 = (Deriv(f, "x", nderiv = 2*j-1))(m*n)
    f1 = (Deriv(f, "x", nderiv = 2*j-1))(0)
    C = C + b * (f2 - f1) * (m^(2*j) - 1)
  }
  
  
  aprox = m * A - 0.5*(m-1) * B - C
  
  real = 0
  for(i in 1:(m*n)){
    real = real + f(i)  
  }
  
  {eq = paste0("\n\n                        MÉTODO DE WOOLHOUSE\n",
               'Sumatoria entre 1 y ', m*n,' \n',
               'm = ', m, '    n = ', n,'    k = ', k, '\n\n',
               'm * SUMA(t = [p+1, n]) f(tm) - 1/2 (m-1) [f(mn)-f(0)] - SUMA(s=[1, k]) b_2s (m^s2 - 1) [f^(2s-1)(mn) - f^(2s-1)(0)] =\n',
               '= m * ', round(A, 4), ' - 1/2 (m-1) ', round(B, 4), ' - ', round(C,4), ' =\n',
               '= ', (aprox), '\n\n',
               'Resultado teórico: ', real, '\n\n'
               
  )}
  
  {invisible(data.frame(eq, aprox))}
}


cat(woolhouse(function(x) exp(-0.25*(x+1)^3),
              4, 3)$eq)
