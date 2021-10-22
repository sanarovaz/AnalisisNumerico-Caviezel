divdifftable <- function(x, y, n = length(x) - 1){
    df <- data.frame(x,y)
    yt = ys
    for (i in 1:n){
        yt = diff(df[,length(df)][1:length(yt)])
        xt = diff(df$x, lag = i)
        newcol = yt/xt
        df <- cbind(df, append(newcol, rep(0, i)))
        colnames(df) = c(colnames(df)[-length(df)], paste0("diffdiv_", i))
    }
df
}

descnewton <- function(x, xs, ys) {
    tab <- divdifftable(xs,ys)
    len <- length(xs) - 1
    op  <- tab[1,2]
    
    for(i in 1:len){
        w <- xs[1:i]
        op <- op + tab[1,2+i] * prod(x-w)
    }
op
}


xs = c(0.6, 0.8, 1, 1.2)
ys = c(-0.22185, -0.09691, 0, 0.07918)
View(divdifftable(xs, ys))

descnewton(1978, xs, ys) - descnewton(1964, xs, ys)


