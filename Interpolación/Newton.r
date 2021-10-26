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

xs = c(0.1, 0.35, 0.58, 0.8)
ys = 57^xs

descnewton(0.5, xs, ys)


