mco.axb <- function(x,w,u){
  vm = c()
  logx = log(x)
  for (i in 1:2){
    for (j in 1:2){
      vm <- append(vm,
                   sum(w * logx^(j+i-2)))
    }
  }
  
  m = matrix(vm, nrow = 2)
  
  wus = c()
  for(k in 0:1){
    wui = w * log(u) * logx^k
    wus <- append(wus, sum(wui))
  }
  
  R = solve(m) %*% wus
  a <- exp(R[1])
  b <- (R[2])
  c(a,b)
}

x = c(87:90)
w = c(0.896755162, 1.038348083, 1.002949853, 1.061946903)
u = c(50, 62, 64, 71) / c(76,88,85,90)

mco.axb(x,w,u)

v <- mco.axb(x,w,u)
a <- v[1]
b <- v[2]
g <- function(x){a * x^b}
uv = g(x)

plot(x,u, pch = 19,
     main = paste0('Ajustamiento MCO (a = ', (a),', b = ', round(b,4), ')' ),
     xlab = 'x', ylab = 'f(x)')
lines(x, uv, lty = 2, col = 'blue', lwd = 2)
legend(x = 'topleft',
       legend = c("u(x)", paste0('v(x) = ', (a), ' x ^ (', round(b,4), ')')), 
       pch = c(4, 19), lwd = c(-1,2), lty = c(0, 2), col = c('black', 'blue'))
