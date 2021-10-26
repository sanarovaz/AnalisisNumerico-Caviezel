lagrange <- function(x, xs = NULL, ys = NULL, df = NULL, scan = FALSE, as_df = FALSE) {

    if (scan){
        print("Defina preimágenes en orden de tabla.")
            xs = scan()
        print("Defina imágenes en orden de tabla.")
            ys = scan()
    }

    if(is.data.frame(xs)){
        df = xs
        as_df = TRUE
    }

    if(as_df){
        if(length(df) != 2){stop("El dataframe contiene un número de columnas distinto de 2.")}

        colnames(df) = c("x", "y")
        lendf = length(df$x)
        op   <- 0

        for (i in 1:lendf){
            w   <- df$x[-i]
            h   <- prod(x-w) / prod(df$x[i]-w)
            op  <- op + df$y[i] * h
        }
    } else {

        df <- data.frame(x = xs, y = ys)
        lendf = length(df$x)
        op   <- 0

        for (i in 1:lendf){
            w   <- df$x[-i]
            h   <- prod(x-w) / prod(df$x[i]-w)
            op  <- op + df$y[i] * h
        }
    }
  op
}
 
xs = c(0.1, 0.35, 0.58, 0.8)
ys = 57^xs

lagrange(0.5, xs, ys)
