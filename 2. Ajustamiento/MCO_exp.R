mco.exp <- function(x, w){
  vm = c()
  for (i in 1:2){
    for (j in 1:2){
      vm <- append(vm,
                   sum(w * x^(j+i-2)))
    }
  }
  
  m = matrix(vm, nrow = 2)
  
  wus = c()
  for(k in 0:1){
    wui = w * log(u) * x^k
    wus <- append(wus, sum(wui))
  }
  
  R = solve(m) %*% wus
  a <- R[2]
  b <- exp(R[1])
  c(a,b)
}

x = c(4, 4.2, 4.5, 4.7, 5.1, 5.5, 5.9)
n_x = c(75,64,59,36,101,78,89)
w = n_x/mean(n_x)
u = c(102.56, 113.18, 130.11, 142.05, 167.53, 195.14, 224.87)

mco.exp(x,w)

v <- mco.exp(x,w)
a <- v[1]
b <- v[2]
g <- function(x){b * exp(a*x)}
uv = g(x)

plot(x,u, pch = 19,
     main = paste0('Ajustamiento MCO (a = ', round(a,4),', b = ', round(b,4), ')' ),
     xlab = 'x', ylab = 'f(x)')
lines(x, uv, lty = 2, col = 'blue', lwd = 2)
legend(x = 'topleft',
       legend = c("u(x)", paste0('v(x) = ', round(b,4), ' e^(', round(a,4), 'x)')), 
       pch = c(4, 19), lwd = c(-1,2), lty = c(0, 2), col = c('black', 'blue'))
