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


# Función de Ajustamiento por Whittaker-Henderson. Toma como argumentos el grado del polinomio (z),
# la ponderación de la regularidad en relación a la fidelidad (h), el vector de preimágenes (xs),
# el vector de tamaños de muestra correspondientes a cada preimagen (n_x), y el vector de imágenes (f_x).
# La función devuelve un vector vertical que contiene la imagen ajustada correspondiente a las 
# preimágenes determinadas.

whittakerhenderson <- function(z, h, xs, n_x, f_x){
  w_x = n_x / mean(n_x)
  lenx = length(xs)
  U = f_x
  K = f(lenx, z)
  W = diag(lenx) * w_x
  C = W + h * (t(K) %*% K)
  V = solve(C) %*% W %*% U
  V
}


z = 3
h = 2
xs = seq(75,81)
n_x = c(134, 101, 136, 129, 110, 99, 109)
f_x = c(0.072, 0.069, 0.084, 0.089, 0.118, 0.105, 0.122)


w_x <- whittakerhenderson(z, h, xs, n_x, f_x)

w_x

