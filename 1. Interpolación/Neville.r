neville <- function(x, xs, ys) {
    df <- data.frame(x = xs, y = ys)
    len <- length(xs)
    if (len == 1){
        op = df[1,2]
    } else {
        a   = x-xs[len]
        aa  = a * neville(x,xs[-len], ys[-len])
        b   = x-xs[1]
        bb  = b * neville(x,xs[-1], ys[-1])
        op  = (aa - bb)/ (a - b)
    }
op
}

xs = c(0.1, 0.35, 0.58, 0.8)
ys = 57^xs

neville(0.5, xs, ys)
 