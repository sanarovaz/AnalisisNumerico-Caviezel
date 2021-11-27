# Análisis Numérico - Curso Caviezel (2Q2021)
 
La carpeta **Interpolación** incluye algunos de los métodos numéricos de interpolación estudiados teóricamente en clase, en un intento de traslación a R. Nótese que falta el MRC, y que el Método de Neville no está adaptado para casos de argumentos reiterados. Incluye también fnc.R, que obtiene el valor de f^(n+1) (c) necesario para calcular la cota de error de interpolación, así como tablas.R, conteniendo funciones para generar tablas de diferencias simples y divididas que se emplean para algunos métodos de interpolación.

La carpeta **Ajustamiento** incluye tres algoritmos de méotodo de ajuste por Mínimos Cuadrados para tres modelos de v(x): a * x^b, a * *e*^b y polinomio de grado n. Además, Whittaker-Henderson.r incluye una función de ajuste por dicho procedimiento.

La carpeta **Sumación** contiene el script correspondiente a la fórmula de Woolhouse para calcular sumatorias.

La carpeta **Integración** contiene algoritmos correspondientes a los métodos generalizados de trapecios, Simpson, Euler-McLaurin y Laplace, todos requiriendo del algoritmo fnc que permite obtener los valores máximos de una derivada n-ésima en un intervalo.
