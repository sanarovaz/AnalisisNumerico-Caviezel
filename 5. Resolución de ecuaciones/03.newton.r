# MÉTODO DE NEWTON

# El método de Newton es aplicable a problemas que buscan encontrar la raíz de una función aproximándose a él en la sucesión p_n = p_{n-1} - f(p_{n-1}) / f'(p_{n-1})
 
library(MASS)

newt <- function(f, p_0, tol = 1e-10, lim = 1000) {
    i <- 1
    p_i <- p_0
    f_derivada = Deriv(f, "x")

    while (abs(f(p_i) / f_derivada(p_i)) > tol     &    i <= lim) {
        p_i <- p_i - f(p_i) / f_derivada(p_i)
        i <- i + 1
    }
    if (i > lim)    {return("No se encontró una solución satisfactoria en la cantidad de iteraciones especificadas.")}
    else            {return(p_i)}
}

#Ejemplo con función cos(x) - 1
f           <- function(x) {cos(x) - x}
f(newt(f, pi/4))        #-7.771561e-16