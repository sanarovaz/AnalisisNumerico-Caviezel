# Método por bisección
# El método se utiliza para encontrar la raíz de f(x) dentro de un intervalo [a;b] donde f es continua y f(a) y f(b) de signo opuesto. La existencia
# de una raíz en dicho intervalo está garantizada por el Teorema de Valor Medio.
# Aplicada a ecuaciones univariables f(x) = 0, la función toma como argumentos una función "f", los valores reales "a" y "b" que definen un intervalo
# de los números reales. Además se define un valor de tolerancia al error "tol" y una cota "lim" a las iteraciones del proceso.

bisect <- function(f, a, b, tol = 1e-10, lim = 1000) { 
    
    # Se verifica que a < b.
    if (b < a) {
        a <- a + b
        b <- a - b
        a <- a -b
    }

    # Se verifica que las imágenes de los extremos sean de signo distinto.
    if (sign(f(a)) * sign(f(b)) > 0) {
            stop("Las imágenes de los extremos del intervalo no son de signo opuesto.")            
    } else {
        for (i in 1:lim) {

            # Se define "k", el punto medio del intervalo [a;b]
            k = a + (b-a)/2
            fk = f(k)
            fa = f(a)

            # Si "k" es raíz de la función, o si el intervalo [a;b] es de un ancho menor al error tolerable "tol", tomamos "k" como
            # aproximación aceptable de la raíz de la función.
            if (fk == 0 || (b-a) < tol) {
                return(k)

            # Si f(k) es de mismo signo que f(a) - ergo de sentido contrario a f(b) - por Teorema del Valor Medio sabemos que existe una raíz de la función
            # entre "k" y "b". El proceso se reiterará buscando una raíz en el intervalo [k;b].
            } else if (sign(f(k)) * sign(f(a)) > 0) {
                a = k

            # Análogamente, si f(b) es de mismo signo que f(k) el proceso se reiterará sobre el intervalo [a;k].
            } else {
                b = k
            }
        }
    }

    # En caso de no encontrar una aproximación aceptable en la cantidad de iteraciones definidas, devolver mensaje de error.
    stop("No se encontró una raíz en ", lim, " iteraciones.")     
}

# Ejemplo con la función seno:
bisect(sin, a = 3, b =  3.5, tol = 1e-15, lim = 1000)       # 3.141593.

# Ejemplo con función polinómica:
f <- function(x) {(2/6) * x^5 - 6 * x^3 - 100 * x^2 + 2000}

s1 <- bisect(f, a = -5, b = -4, tol = 1e-15, lim = 1000)    # -4.421828
# Evaluando la función en el punto s1:
f(s1)                                                       # -4.547474e-13

s2 <- bisect(f, a = 4, b = 5, tol = 1e-15, lim = 1000)      # 4.575856
f(s2)                                                       # 0

s3 <- bisect(f, a = 6.2, b = 6.8, tol = 1e-15, lim = 1000)  # 6.505568
f(s3)                                                       # 9.094947e-13
