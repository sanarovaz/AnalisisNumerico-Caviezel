# ITERACIÓN DEL PUNTO FIJO

# El método se basa en el Teorema del Punto Fijo, que estipula que dada una función g(x) ∈ C[a;b], con g(x) ∈ [a,b] para todo
# x ∈ [a;b], y si g'(x) existe en (a;b) donde cumple |g'(x)| < 1, entonces la sucesión p_n = g(p_{n-1}) converge al único
# punto fijo p en [a;b].

# Se busca el punto fijo "p" de una función "f" que cumpla con f(p) = p. La función toma como argumentos una función "f",
# un valor "p_0" desde donde iniciar el proceso iterativo, una tolerancia al error de aproximación "tol" y una cota para 
# el número de iteraciones "lim".

ipf <- function(f, p_0, tol = 1e-10, lim = 10000) {
    i <- 1
    p_i <- p_0

    # El proceso iterativo se repetirá al menos tantas veces como lo permita el parámetro "lim", y siempre que el próximo elemento de la
    # sucesión no cause desbordamiento. Mientras esas condiciones no ocurran, el proceso se reiterará hasta conseguir un valor "p_i"
    # que tenga una distancia respecto de su imagen menor o igual al error tolerado "tol".
    while (abs(p_i - f(p_i)) > tol   &   !(is.na(f(p_i)))   &    i <= lim) {
        p_i <- f(p_i)
        i <- i + 1
    }

    # Si el valor de f(p_i) causa desbordamiento, concluir que la sucesión diverge.
    if (is.na(f(p_i))) {return(paste0("La sucesión es divergente. (Causa desbordamiento buscando f(p_", i, "))."))}

    # Si no se encuentra punto fijo aceptable en la cantidad de iteraciones permitidas, reportar el error.
    else if (i > lim)            {return("La función no generó una aproximación satisfactoria en las iteraciones permitidas.")}

    # Si la diferencia entre p_0 y f(p_0) es menor o igual al error tolerado, se considera "p_0" como una aproximación
    # aceptable de punto fijo de la función.
    else                    {return(p_i)}
}


# Ejemplo con función de raíz cuadrada.
s1 <- ipf(sqrt, 3.5, tol = 1e-12)       # 1
sqrt(s1)                                # 1

# Ejemplo de caso divergente
g <- function(x) {x^3 + 4*x^2 - 10}
ipf(g, p_0 = 0)                         # La sucesión es divergente. (El valor f(p_7) causa desbordamiento.)

# Ejemplo con función polinómica
h <- function(x) {0.5 * (10-x^3)^(0.5)}
s2 <- ipf(h, p_0 = 1.5)                 # 1.36523
h(s2)                                   # 1.36523 

# ----------------------------------------------------------------------------------------------------------------------------------------

# Es posible modificar problemas de encontrar la solución p de la función f(x) = 0, alterando algebraicamente la igualdad para
# llegar a una expresión de forma x = g(x), con punto fijo en la raíz de f.

# Ejemplo de búsqueda de la raíz de una función j(x)
j           <- function(x) {(exp(-x) * (x^2 + 5*x +2)) + 1}             
j_despejada <- function(x) {(exp(x)+ x^3 + 4*x^2 + 2*x + 2)/(x^2+3*x-3)}
s3 <- ipf(j_despejada, -2)              # -0.5791589
j(s3)                                   # -6.727952e-14
