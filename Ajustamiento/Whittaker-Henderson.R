# Función f que se utiliza para generar la matriz K.
f <- function(n,z){
  k_i = c()
  
  for (i in 0:z){
    p = dim(combn(z,i))[2] * (-1)^(z-i)
    k_i <- append(k_i, p)
    }
 
  ks = c()
  s = n-z
  
  for (j in 1:s){
    k_j = c(rep(0, j-1), k_i, rep(0, s-j))
    ks = append(ks,k_j)
  }
  
K = matrix(ks, ncol = n, byrow = TRUE)
K
}


# Función de Ajustamiento por Whittaker-Henderson. Toma como argumentos el grado de regularidad (z),
# la ponderación de la regularidad en relación a la fidelidad (h), el vector de tamaños de muestra
# correspondientes a cada preimagen (w), y el vector de imágenes observadas (U).
# El booleano wdado sirve para aclarar cuando se provee directamente un vector wdado con las ponderaciones
# de los argumentos. En ese caso se considera que el vector w es el vector que contiene dichas ponderaciones.
# La función devuelve un vector que contiene la imagen ajustada correspondiente a las preimágenes determinadas.

whittakerhenderson <- function(z, h, w, U, wdado = FALSE){
  
  if (wdado){
    w_x = w
  } else {
    w_x = w / mean(w)  
  }
  
  n = length(U)
  K = f(n, z)
  W = diag(n) * w_x
  C = W + h * (t(K) %*% K)
  V = solve(C) %*% W %*% U
  V
}

z = 1
h = 0.25
x = 1:4
u = c(9.75, 77.3, 91.05, 14.55)
w = c(1, 4, 1, 1)
v = c(17, 75, 87, 18)


whittakerhenderson(z,h,w,u, wdado = TRUE)



# Función que permite encontrar el vector de ponderadores w, dados los valores observados (U) y ajustados (V),
# además de los coeficientes z y h.

whittakerhenderson.getw <- function(z,h,U,V){
  n = length(U)
  K = f(n,z)
  A = h * t(K) %*% K %*% V
  B = U-V
  W = A/B
  as.vector(W)
}


j <- whittakerhenderson.getw(z,h,u,v)
whittakerhenderson(z,h,j,u, wdado = TRUE)



# Plot
{w_t <- whittakerhenderson(z,h,w,u)

plot(x, u,
 main = paste0('Ajustamiento por Whittaker-Henderson (h = ', h,', z = ', z, ')' ),
 pch = -2, xlab = 'x', ylab = 'f(x)')

lines(x, w_t, lty = 2, col = 'red', lwd = 2)

points(x, w_t, pch = 19, col = 'red', cex = 1)

points(x, u, lwd = 3, pch = 4)

legend(0.95,90, 
   legend = c("u(x)", "v(x)"), 
   pch = c(4, 19), lwd = c(-1,2), lty = c(0, 2), col = c('black', 'red'))
}
