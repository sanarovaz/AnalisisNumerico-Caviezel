# Análisis Numérico - Curso Caviezel (2Q2021)
 
La carpeta **Interpolación** incluye algunos de los métodos numéricos de interpolación estudiados teóricamente en clase, en un intento de traslación a R. Nótese que falta el MRC, y que el Método de Neville no está adaptado para casos de argumentos reiterados. Incluye también fnc.R, que obtiene el valor de f^(n+1) (c) necesario para calcular la cota de error de interpolación, así como tablas.R, conteniendo funciones para generar tablas de diferencias simples y divididas que se emplean para algunos métodos de interpolación.

La carpeta **Ajustamiento** incluye un algoritmo relativo al método de Whittaker-Henderson para obtener las imágenes ajustadas y para obtener el vector de ponderadores w. La misma carpeta incluye tres distintos algoritmos de méotodo de ajuste de MCO para tres modelos de v(x): polinomio de grado n, potencia de _e_ y de forma ax^b.
