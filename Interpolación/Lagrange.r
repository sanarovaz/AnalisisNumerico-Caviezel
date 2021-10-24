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
 
xs = c(1910, 1930, 1950, 1970, 1990)
ys = c(125320, 133420, 117183, 120323, 145311)
df2 = data.frame(xs,ys)

lagrange(1978, xs,ys) - lagrange(1964, xs,ys)

lagrange(1978, df2) - lagrange(1964, df2)
