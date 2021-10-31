mco.poly <- function(x, w, u, n){
  vm = c()
  for (i in 1:n){
    for (j in 1:n){
      vm <- append(vm,
                   sum(w * x^(j+i-2)))
    }
  }
  
  m = matrix(vm, nrow = n)
  
  wus = c()
  for(k in 0:(n-1)){
    wui = w * u * x^k
    wus <- append(wus, sum(wui))
  }
  
  R = solve(m) %*% wus
  R
}

x = c(4, 4.2, 4.5, 4.7, 5.1, 5.5, 5.9)
n_x = c(75,64,59,36,101,78,89)
w = n_x/mean(n_x)
u = c(102.56, 113.18, 130.11, 142.05, 167.53, 195.14, 224.87)
n = 3

mco.poly(x,w,u,3)

v <- mco.poly(x,w,u,3)
a <- v[1]
b <- v[2]
c <- v[3]
g <- function(x){a + b*x + c*x^2}
uv = g(x)

plot(x,u, pch = 19,
     main = paste0('Ajustamiento MCO (a = ', round(a,4),', b = ', round(b,4), ', c = ', round(c,4), ')' ),
     xlab = 'x', ylab = 'f(x)')
lines(x, uv, lty = 2, col = 'blue', lwd = 2)
legend(x = 'topleft',
       legend = c("u(x)", paste0('v(x) = ', round(a,4), ' + ', round(b,4), 'x + ', round(c,4), 'x^2')), 
       pch = c(4, 19), lwd = c(-1,2), lty = c(0, 2), col = c('black', 'blue'))
